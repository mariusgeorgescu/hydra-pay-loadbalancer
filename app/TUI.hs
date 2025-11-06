{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use <$>" #-}

module TUI
  ( Service(..)
  , serviceBaseUrl
  , checkServiceAvailable
  , scanAvailableServices
  , runTUI
  ) where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Main (viewportScroll, vScrollBy, hScrollBy)
import Brick.Types qualified as T
import Brick.Util qualified as Util
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core qualified as W
import Brick.Widgets.Edit qualified as E
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put, modify)
import Data.List (isInfixOf, find)
import HydraPay.Client (runHydraClient, HydraClientError(..), formatError, getHeadState, getHead, getUncommittedDeposits)
import Data.Text qualified as Text
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import HydraPay.API.Types (HeadStateResponse(..), ManageHeadSchema(..), OperationResponse(..), UncommittedDepositsResponse(..), UncommittedDeposit(..), TxOutRef(..))
import HydraPay.Database (initDatabase, insertCommittedDeposits, deleteDepositsByServicePort, getDepositsByServicePort, runDB)
import HydraPay.Operations (execQueryFunds, execDeposit, execWithdraw, execPayMerchant, execOpenHead, execCloseHead)
import Database.Persist.Sqlite qualified as Sqlite
import System.IO
import Data.Maybe (mapMaybe)
import qualified Data.Maybe

-- | Service configuration
data Service = Service
  { serviceName :: String
  , serviceHost :: String
  , servicePort :: Int
  }
  deriving (Eq, Show)

-- | Hydra Head information
data HydraHeadInfo = HydraHeadInfo
  { hydraHeadPort :: Int
  , hydraHeadId :: Maybe String  -- Nothing means "Closed", Just id means active head
  }
  deriving (Eq, Show)

-- | Users by head ID mapping
type UsersByHead = [(String, [String])]  -- (headId, [addresses])

-- | Pending deposits (uncommitted deposits)
type PendingDeposits = [UncommittedDeposit]

-- | Get base URL for a service
serviceBaseUrl :: Service -> String
serviceBaseUrl (Service _ host port) = "http://" ++ host ++ ":" ++ show port

-- | Check if a service is available by making a test HTTP request
checkServiceAvailable :: Service -> IO Bool
checkServiceAvailable service = do
  let baseUrl = serviceBaseUrl service
      testUrl = baseUrl ++ "/state?id=test-availability-check"

  manager <- HTTP.newManager HTTP.defaultManagerSettings
  request <- HTTP.parseRequest testUrl

  -- Set a short timeout (2 seconds)
  let requestWithTimeout = request
        { HTTP.responseTimeout = HTTP.responseTimeoutMicro 2000000  -- 2 seconds
        }

  catch
    (do
      response <- HTTP.httpLbs requestWithTimeout manager
      -- If we get any HTTP response (even 400/404), the service is available
      let status = HTTP.responseStatus response
      return $ statusCode status >= 200 && statusCode status < 600)
    (\e -> do
      -- Connection errors mean service is not available
      -- Catch all exceptions (HttpException, IOException, etc.)
      let _ = e :: HTTP.HttpException
      return False)

-- | Scan ports from startPort to endPort (inclusive) and return available services
scanAvailableServices :: String -> Int -> Int -> IO [Service]
scanAvailableServices host startPort endPort = do
  let ports = [startPort .. endPort]
      services = map (\port -> Service ("Blazar-Pay Service: " ++ show port) host port) ports

  -- Check services concurrently would be better, but for simplicity we'll do sequentially
  -- with a small delay to avoid overwhelming the system
  filterM checkWithDelay services
  where
    checkWithDelay service = do
      available <- checkServiceAvailable service
      threadDelay 50000  -- 50ms delay between checks
      return available

-- | Name for different UI resources
data ResourceName
  = QueryFundsEditField
  | DepositUserAddressField
  | DepositPublicKeyField
  | DepositAssetUnitField
  | DepositAmountField
  | WithdrawAddressField
  | WithdrawOwnerField
  | WithdrawUtxoHashField
  | WithdrawUtxoIndexField
  | WithdrawSignatureField
  | PayMerchantMerchantAddressField
  | PayMerchantUtxoHashField
  | PayMerchantUtxoIndexField
  | PayMerchantAssetUnitField
  | PayMerchantAmountField
  | PayMerchantSignatureField
  | PayMerchantMerchantUtxoHashField
  | PayMerchantMerchantUtxoIndexField
  | OpenHeadUrlsField
  | CloseHeadIdField
  | ResultViewport  -- Viewport for scrolling result screens
  | UserActionsViewport  -- Viewport for scrolling User Actions menu
  | AdminActionsViewport  -- Viewport for scrolling Admin Actions menu
  | UsersByHeadViewport  -- Viewport for scrolling Users By Head widget
  | PendingDepositsViewport  -- Viewport for scrolling Pending Deposits widget
  deriving (Eq, Ord, Show)

-- | Current screen shown to the user
data Screen
  = SplashScreen
  | MainMenuScreen
  | QueryFundsFormScreen {qfEdit :: E.Editor String ResourceName}
  | DepositFormScreen
      { dsfCurrentField :: Int
      , dsfFields :: [E.Editor String ResourceName]
      }
  | WithdrawFormScreen
      { wfsfCurrentField :: Int
      , wfsfFields :: [E.Editor String ResourceName]
      }
  | PayMerchantFormScreen
      { pmfsCurrentField :: Int
      , pmfsFields :: [E.Editor String ResourceName]
      }
  | OpenHeadFormScreen {ohEdit :: E.Editor String ResourceName}
  | CloseHeadFormScreen  -- No longer needs editor, uses current service's head ID
  | ResultScreen {rsMessage :: [String], rsTitle :: String, rsScrollOffset :: Int}
  deriving (Show)

-- | Application state
data AppState = AppState
  { currentScreen :: Screen
  , services :: [Service]
  , selectedServiceIndex :: Int  -- Index of currently selected service
  , splashLogo :: [String]
  , headerLogo :: [String]  -- Header logo for main screen
  , hydraHeads :: [HydraHeadInfo]  -- Cached Hydra Heads data
  , dbPool :: Sqlite.ConnectionPool  -- Database connection pool
  , usersByHead :: UsersByHead  -- Cached users by head ID
  , pendingDeposits :: PendingDeposits  -- Cached uncommitted deposits
  , refreshCounter :: Int  -- Counter to force re-render on refresh
  } deriving (Show)

instance Show Sqlite.ConnectionPool  where
  show _ = "ConnectionPool"


-- | Get the currently selected service
selectedService :: AppState -> Service
selectedService appState = services appState !! selectedServiceIndex appState

-- | Get base URL for the currently selected service
currentBaseUrl :: AppState -> String
currentBaseUrl = serviceBaseUrl . selectedService

-- | Get peer API URL for Open Head based on selected service
-- Extracts service number from port (e.g., 3001 -> 1, 3002 -> 2)
-- Returns "http://hydra-node-{serviceNo}:400{serviceNo}/commit"
getPeerApiUrl :: AppState -> String
getPeerApiUrl appState =
  let svc = selectedService appState
      port = servicePort svc
      -- Extract service number from port (3001 -> 1, 3002 -> 2, etc.)
      serviceNo = port - 3000
  in "http://hydra-node-" ++ show serviceNo ++ ":400" ++ show serviceNo ++ "/commit"

-- | Drawing function
drawUI :: AppState -> [T.Widget ResourceName]
drawUI appState =
  let mainWidget = case currentScreen appState of
        SplashScreen -> drawSplashScreen (splashLogo appState)
        MainMenuScreen -> drawMainMenu (hydraHeads appState) appState
        QueryFundsFormScreen edit -> drawQueryFundsForm edit
        DepositFormScreen current fields -> drawDepositForm current fields
        WithdrawFormScreen current fields -> drawWithdrawForm current fields
        PayMerchantFormScreen current fields -> drawPayMerchantForm current fields
        OpenHeadFormScreen edit -> drawOpenHeadForm edit
        CloseHeadFormScreen -> drawCloseHeadForm appState
        ResultScreen message title scrollOffset -> drawResultScreen message title scrollOffset
      servicesWidget = drawServicesPanel appState
      -- Show services panel only when not on splash screen and not on main menu
      layout = case currentScreen appState of
        SplashScreen -> [mainWidget]
        MainMenuScreen -> [mainWidget]  -- Services widget is integrated in main menu
        _ -> [W.hBox [servicesWidget, W.str " ", mainWidget]]
  in layout

-- | Services panel widget
drawServicesPanel :: AppState -> T.Widget ResourceName
drawServicesPanel appState =
  let servicesList = services appState
      selectedIdx = selectedServiceIndex appState
      renderService idx svc =
        let name = serviceName svc
            url = serviceBaseUrl svc
            prefix = if idx == selectedIdx then "> " else "  "
            attr = if idx == selectedIdx then W.withAttr (A.attrName "selected") else id
        in attr $ W.str $ prefix ++ name ++ " (" ++ url ++ ")"
      serviceWidgets = zipWith renderService [0 ..] servicesList
      selectedName = serviceName (selectedService appState)
  in B.borderWithLabel (W.str "Available Services") $
       W.vBox $
         [ W.str ""
         , W.str "Press Tab to switch service"
         , W.str ""
         ] ++ serviceWidgets ++
         [ W.str ""
         , W.str $ "Selected: " ++ selectedName
         , W.str ""
         ]

-- | Splash screen with BlazaLabs logo
drawSplashScreen :: [String] -> T.Widget ResourceName
drawSplashScreen logoLines =
  C.center $
    W.withAttr (A.attrName "cyan") $
      W.vBox $
        [W.str ""]
          ++ map W.str logoLines
          ++ [ W.str ""
             , W.str ""
             , W.str "                             Press any key to continue"
             , W.str ""
             ]

-- | Main menu with Dashboard and Admin zones
drawMainMenu :: [HydraHeadInfo] -> AppState -> T.Widget ResourceName
drawMainMenu heads appState =
  let -- Header logo (centered and cyan colored)
      logoWidget = C.center $ W.withAttr (A.attrName "cyan") $ W.vBox $ map W.str (headerLogo appState)

      -- Dashboard zone (top): Hydra Heads widget, Users By Head widget, and Pending Deposits widget
      headsTable = drawHydraHeadsTable heads
      usersByHeadWidget = drawUsersByHeadWidget heads appState
      pendingDepositsWidget = drawPendingDepositsWidget appState
      dashboardZone = B.borderWithLabel (W.str "Dashboard") $
        W.hBox [headsTable, W.str "  ", usersByHeadWidget, W.str "  ", pendingDepositsWidget]

      -- User Actions menu (left side of actions area)
      userActionsMenu = W.vBox
        [ W.str "User Actions:"
        , W.str ""
        , W.str "  1. Query Balance"
        , W.str "  2. Deposit Funds"
        , W.str "  3. Withdraw Funds"
        , W.str "  4. Pay Merchant"
        ]
      userActionsViewport = W.viewport UserActionsViewport T.Vertical userActionsMenu
      userActionsViewportWithScrollbar = W.withVScrollBars T.OnRight userActionsViewport
      userActionsBox = B.borderWithLabel (W.str "User Actions") userActionsViewportWithScrollbar

      -- Admin Actions menu (right side of actions area)
      adminActionsMenu = W.vBox
        [ W.str "Admin Actions:"
        , W.str ""
        , W.str "  5. Open Head"
        , W.str "  6. Close Head"
        , W.str ""
        , W.str "  r. Refresh"
        , W.str "  q. Quit"
        ]
      adminActionsViewport = W.viewport AdminActionsViewport T.Vertical adminActionsMenu
      adminActionsViewportWithScrollbar = W.withVScrollBars T.OnRight adminActionsViewport
      adminActionsBox = B.borderWithLabel (W.str "Admin Actions") adminActionsViewportWithScrollbar

      -- Services widget (left side)
      servicesWidget = drawServicesPanel appState

      -- Actions zone: User Actions and Admin Actions side by side
      actionsZone = W.hBox [userActionsBox, W.str "  ", adminActionsBox]

      -- Admin zone (bottom): Services on left, Actions on right
      adminZone = B.borderWithLabel (W.str "Admin") $
        W.hBox [servicesWidget, W.str "  ", actionsZone]

      -- Full layout: Logo centered on top, Dashboard and Admin centered horizontally
  in W.vBox
       [ logoWidget
       , W.str ""
       , C.center dashboardZone
       , W.str ""
       , C.center adminZone
       ]

-- | Users By Head widget - shows addresses for each head
drawUsersByHeadWidget :: [HydraHeadInfo] -> AppState -> T.Widget ResourceName
drawUsersByHeadWidget heads appState =
  let -- Build content for each head using cached data
      buildHeadContent :: HydraHeadInfo -> [String]
      buildHeadContent headInfo =
        case hydraHeadId headInfo of
          Nothing -> []  -- Skip closed heads
          Just headId -> 
            let header = "Service " ++ show (hydraHeadPort headInfo) ++ " (" ++ headId ++ "):"
                addresses = Data.Maybe.fromMaybe [] $ lookup headId (usersByHead appState)
            in if null addresses
               then [header, "  No users"]
               else header : map ("  " ++) addresses

      -- Filter only open heads
      openHeads = filter (\h -> Data.Maybe.isJust (hydraHeadId h)) heads
      
      -- Build all content for open heads
      allContent = concatMap buildHeadContent openHeads
      
      -- If no content, show a message
      finalContent = if null allContent
                     then ["No open heads"]
                     else allContent

      contentWidget = W.vBox $ map W.str finalContent
      viewportWidget = W.viewport UsersByHeadViewport T.Vertical contentWidget
      viewportWithScrollbar = W.withVScrollBars T.OnRight viewportWidget
  in B.borderWithLabel (W.str "Users By Head") viewportWithScrollbar

-- | Pending Deposits widget - shows uncommitted deposits
drawPendingDepositsWidget :: AppState -> T.Widget ResourceName
drawPendingDepositsWidget appState =
  let -- Build content for each pending deposit
      buildDepositContent :: UncommittedDeposit -> [String]
      buildDepositContent uncommittedDep =
        let addr = Text.unpack $ uncommittedDepositAddress uncommittedDep
            amountList = uncommittedDepositAmount uncommittedDep
            amountStr = if null amountList
                       then "No amount"
                       else unwords $ map (\(unit, val) -> Text.unpack unit ++ ":" ++ show val) amountList
            utxoRef = uncommittedDepositFundsUtxoRef uncommittedDep
            utxoStr = case utxoRef of
              Just ref -> Text.unpack (txOutRefHash ref) ++ ":" ++ show (txOutRefIndex ref)
              Nothing -> "N/A"
        in [ "Address: " ++ addr
           , "  Amount: " ++ amountStr
           , "  UTxO: " ++ utxoStr
           , ""
           ]

      -- Build all content
      depositCount = length (pendingDeposits appState)
      debugInfo = "Total: " ++ show depositCount ++ " deposits"
      allContent = if null (pendingDeposits appState)
                   then ["No pending deposits", "", debugInfo]
                   else debugInfo : "" : concatMap buildDepositContent (pendingDeposits appState)

      contentWidget = W.vBox $ map W.str allContent
      viewportWidget = W.viewport PendingDepositsViewport T.Vertical contentWidget
      viewportWithScrollbar = W.withVScrollBars T.OnRight viewportWidget
  in B.borderWithLabel (W.str "Pending Deposits") viewportWithScrollbar

-- | Hydra Heads table widget (compact version for main menu)
drawHydraHeadsTable :: [HydraHeadInfo] -> T.Widget ResourceName
drawHydraHeadsTable heads =
  let -- Helper function to pad a string to a fixed width (left-aligned)
      padToWidth :: Int -> String -> String
      padToWidth width s = s ++ replicate (max 0 (width - length s)) ' '

      -- Calculate column widths (very compact)
      portColWidth = 8
      headIdColWidth = 30

      -- Create a cell widget (no padding)
      makeCell :: Int -> String -> T.Widget ResourceName
      makeCell width content = W.str $ padToWidth width content

      -- Create header row (compact headers, no border)
      headerPort = makeCell portColWidth "Port"
      headerHeadId = makeCell headIdColWidth "HeadID"
      headerRow = W.hBox [headerPort, W.str " | ", headerHeadId]

      -- Render a data row (no borders for compactness)
      renderRow headInfo =
        let portStr = show (hydraHeadPort headInfo)
            (headIdStr, headIdAttr) = case hydraHeadId headInfo of
              Nothing -> ("Closed", A.attrName "headClosed")
              Just "OPENING" -> ("OPENING", A.attrName "headOpening")
              Just headId -> ( headId, A.attrName "headOpen")
            portCell = makeCell portColWidth portStr
            headIdCell = W.withAttr headIdAttr $ makeCell headIdColWidth headIdStr
        in W.hBox [portCell, W.str " | ", headIdCell]

      -- Create all rows: header, then data rows
      dataRows = map renderRow heads
      allRows = headerRow : dataRows

      -- Create table content (ultra compact, no borders)
      tableContent = W.vBox allRows
  in B.borderWithLabel (W.str "Hydra Heads") tableContent

-- | Query funds form
drawQueryFundsForm :: E.Editor String ResourceName -> T.Widget ResourceName
drawQueryFundsForm edit =
  C.center $
    B.borderWithLabel (W.str "Query Funds") $
      W.vBox
        [ W.str ""
        , W.str "Enter Cardano address:"
        , W.str ""
        , E.renderEditor (W.str . unlines) True edit
        , W.str ""
        , W.str "Press Enter to execute, Esc to go back"
        , W.str ""
        ]

-- | Deposit form
drawDepositForm :: Int -> [E.Editor String ResourceName] -> T.Widget ResourceName
drawDepositForm current fields =
  C.center $
    B.borderWithLabel (W.str "Deposit") $
      W.vBox
        [ W.str ""
        , W.str "User Address:"
        , E.renderEditor (W.str . unlines) (current == 0) (fields !! 0)
        , W.str "Public Key (hex):"
        , E.renderEditor (W.str . unlines) (current == 1) (fields !! 1)
        , W.str "Asset Unit:"
        , E.renderEditor (W.str . unlines) (current == 2) (fields !! 2)
        , W.str "Amount:"
        , E.renderEditor (W.str . unlines) (current == 3) (fields !! 3)
        , W.str ""
        , W.str "Press Tab to move between fields, Enter to execute, Esc to go back"
        , W.str ""
        ]

-- | Withdraw form
drawWithdrawForm :: Int -> [E.Editor String ResourceName] -> T.Widget ResourceName
drawWithdrawForm current fields =
  C.center $
    B.borderWithLabel (W.str "Withdraw") $
      W.vBox
        [ W.str ""
        , W.str "Address:"
        , E.renderEditor (W.str . unlines) (current == 0) (fields !! 0)
        , W.str "Owner (user/merchant):"
        , E.renderEditor (W.str . unlines) (current == 1) (fields !! 1)
        , W.str "UTxO Hash:"
        , E.renderEditor (W.str . unlines) (current == 2) (fields !! 2)
        , W.str "UTxO Index:"
        , E.renderEditor (W.str . unlines) (current == 3) (fields !! 3)
        , W.str "Signature:"
        , E.renderEditor (W.str . unlines) (current == 4) (fields !! 4)
        , W.str ""
        , W.str "Press Tab to move between fields, Enter to execute, Esc to go back"
        , W.str ""
        ]

-- | Pay merchant form
drawPayMerchantForm :: Int -> [E.Editor String ResourceName] -> T.Widget ResourceName
drawPayMerchantForm current fields =
  C.center $
    B.borderWithLabel (W.str "Pay Merchant") $
      W.vBox
        [ W.str ""
        , W.str "Merchant Address:"
        , E.renderEditor (W.str . unlines) (current == 0) (fields !! 0)
        , W.str "UTxO Hash:"
        , E.renderEditor (W.str . unlines) (current == 1) (fields !! 1)
        , W.str "UTxO Index:"
        , E.renderEditor (W.str . unlines) (current == 2) (fields !! 2)
        , W.str "Asset Unit:"
        , E.renderEditor (W.str . unlines) (current == 3) (fields !! 3)
        , W.str "Amount:"
        , E.renderEditor (W.str . unlines) (current == 4) (fields !! 4)
        , W.str "Signature:"
        , E.renderEditor (W.str . unlines) (current == 5) (fields !! 5)
        , W.str "Merchant UTxO Hash (optional):"
        , E.renderEditor (W.str . unlines) (current == 6) (fields !! 6)
        , W.str "Merchant UTxO Index:"
        , E.renderEditor (W.str . unlines) (current == 7) (fields !! 7)
        , W.str ""
        , W.str "Press Tab to move between fields, Enter to execute, Esc to go back"
        , W.str ""
        ]

-- | Open head form
drawOpenHeadForm :: E.Editor String ResourceName -> T.Widget ResourceName
drawOpenHeadForm edit =
  C.center $
    B.borderWithLabel (W.str "Open Head") $
      W.vBox
        [ W.str ""
        , W.str "Peer API URLs (space-separated):"
        , W.str ""
        , E.renderEditor (W.str . unlines) True edit
        , W.str ""
        , W.str "Press Enter to execute, Esc to go back"
        , W.str ""
        ]

-- | Close head form - shows current service's head ID or message
drawCloseHeadForm :: AppState -> T.Widget ResourceName
drawCloseHeadForm appState =
  let currentService = selectedService appState
      currentPort = servicePort currentService
      -- Find head info for current service
      maybeHeadInfo = find (\h -> hydraHeadPort h == currentPort) (hydraHeads appState)
      (headIdMessage, headIdAttr) = case maybeHeadInfo >>= hydraHeadId of
        Nothing -> ("The head is not open", A.attrName "headClosed")
        Just headId -> (headId, A.attrName "headOpen")
      headIdWidget = W.withAttr headIdAttr $ W.str headIdMessage
  in C.center $
       B.borderWithLabel (W.str "Close Head") $
         W.vBox
           [ W.str ""
           , W.str ("Service Port: " ++ show currentPort)
           , W.str ""
           , W.str "Head ID:"
           , W.str ""
           , headIdWidget
           , W.str ""
           , W.str "Press Enter to close head, Esc to go back"
           , W.str ""
           ]

-- | Result screen with scrolling support using Brick viewports
drawResultScreen :: [String] -> String -> Int -> T.Widget ResourceName
drawResultScreen message title _scrollOffset =
  let contentWidget = W.vBox $ map W.str message
      viewportWidget = W.viewport ResultViewport T.Vertical contentWidget
      viewportWithScrollbar = W.withVScrollBars T.OnRight viewportWidget
  in C.center $
       B.borderWithLabel (W.str title) $
         W.vBox
           [ W.str ""
           , viewportWithScrollbar
           , W.str ""
           , W.str "Use ↑↓/PgUp/PgDn/Home/End to scroll, Esc to go back"
           , W.str ""
           ]

-- | Attribute map for styling
theMap :: A.AttrMap
theMap =
  A.attrMap V.defAttr
    [ (A.attrName "cyan", Util.fg V.cyan)
    , (A.attrName "selected", Util.fg V.cyan `V.withStyle` V.bold)
    , (A.attrName "headOpen", Util.fg V.yellow)
    , (A.attrName "headOpening", Util.fg V.yellow)
    , (A.attrName "headOpen", Util.fg V.green)
    , (A.attrName "headClosed", Util.fg V.red)
    ]

-- | Get editor content
getEditorText :: E.Editor String n -> String
getEditorText ed =
  case E.getEditContents ed of
    [line] -> line
    [] -> ""
    lineList -> unlines lineList

-- | Handle app events
handleEvent :: T.BrickEvent ResourceName e -> T.EventM ResourceName AppState ()
handleEvent ev = do
  appState <- get
  case currentScreen appState of
    SplashScreen ->
      case ev of
        -- Any keypress goes to main menu
        _ -> do
          put $ appState {currentScreen = MainMenuScreen}
    MainMenuScreen ->
      case ev of
        (T.VtyEvent (V.EvKey (V.KChar '1') [])) -> do
          put $ appState {currentScreen = QueryFundsFormScreen {qfEdit = E.editor QueryFundsEditField (Just 1) ""}}
        (T.VtyEvent (V.EvKey (V.KChar '2') [])) -> do
          put $
            appState
              { currentScreen =
                  DepositFormScreen
                    { dsfCurrentField = 0
                    , dsfFields =
                        [ E.editor DepositUserAddressField (Just 1) ""
                        , E.editor DepositPublicKeyField (Just 1) ""
                        , E.editor DepositAssetUnitField (Just 1) ""
                        , E.editor DepositAmountField (Just 1) ""
                        ]
                    }
              }
        (T.VtyEvent (V.EvKey (V.KChar '3') [])) -> do
          put $
            appState
              { currentScreen =
                  WithdrawFormScreen
                    { wfsfCurrentField = 0
                    , wfsfFields =
                        [ E.editor WithdrawAddressField (Just 1) ""
                        , E.editor WithdrawOwnerField (Just 1) "user"
                        , E.editor WithdrawUtxoHashField (Just 1) ""
                        , E.editor WithdrawUtxoIndexField (Just 1) ""
                        , E.editor WithdrawSignatureField (Just 1) ""
                        ]
                    }
              }
        (T.VtyEvent (V.EvKey (V.KChar '4') [])) -> do
          put $
            appState
              { currentScreen =
                  PayMerchantFormScreen
                    { pmfsCurrentField = 0
                    , pmfsFields =
                        [ E.editor PayMerchantMerchantAddressField (Just 1) ""
                        , E.editor PayMerchantUtxoHashField (Just 1) ""
                        , E.editor PayMerchantUtxoIndexField (Just 1) ""
                        , E.editor PayMerchantAssetUnitField (Just 1) ""
                        , E.editor PayMerchantAmountField (Just 1) ""
                        , E.editor PayMerchantSignatureField (Just 1) ""
                        , E.editor PayMerchantMerchantUtxoHashField (Just 1) ""
                        , E.editor PayMerchantMerchantUtxoIndexField (Just 1) ""
                        ]
                    }
              }
        (T.VtyEvent (V.EvKey (V.KChar '5') [])) -> do
          let peerApiUrl = getPeerApiUrl appState
          put $ appState {currentScreen = OpenHeadFormScreen {ohEdit = E.editor OpenHeadUrlsField (Just 1) peerApiUrl}}
        (T.VtyEvent (V.EvKey (V.KChar '6') [])) -> do
          put $ appState {currentScreen = CloseHeadFormScreen}
        (T.VtyEvent (V.EvKey (V.KChar 'r') [])) -> do
          -- Refresh Hydra Heads data, users by head, and pending deposits
          newHeads <- liftIO $ fetchHydraHeads (services appState)
          -- Persist deposits for any open heads that don't have deposits in the database yet
          liftIO $ persistMissingDeposits (dbPool appState) (services appState) newHeads
          newUsersByHead <- liftIO $ fetchUsersByHead (dbPool appState) newHeads
          -- Fetch pending deposits from the currently selected service
          newPendingDeposits <- liftIO $ fetchPendingDeposits (currentBaseUrl appState)
          -- Force re-render by incrementing refresh counter even if data didn't change
          let newRefreshCounter = refreshCounter appState + 1
          put $ appState {hydraHeads = newHeads, usersByHead = newUsersByHead, pendingDeposits = newPendingDeposits, refreshCounter = newRefreshCounter}
        (T.VtyEvent (V.EvKey (V.KChar 'q') [])) -> M.halt
        (T.VtyEvent (V.EvKey V.KEsc [])) -> M.halt
        -- Allow scrolling in User Actions viewport
        (T.VtyEvent (V.EvKey V.KUp [])) -> do
          vScrollBy (viewportScroll UserActionsViewport) (-1)
        (T.VtyEvent (V.EvKey V.KDown [])) -> do
          vScrollBy (viewportScroll UserActionsViewport) 1
        (T.VtyEvent (V.EvKey V.KPageUp [])) -> do
          vScrollBy (viewportScroll UserActionsViewport) (-5)
        (T.VtyEvent (V.EvKey V.KPageDown [])) -> do
          vScrollBy (viewportScroll UserActionsViewport) 5
        -- Allow scrolling in Admin Actions viewport
        (T.VtyEvent (V.EvKey (V.KChar 'a') [])) -> do
          vScrollBy (viewportScroll AdminActionsViewport) (-1)
        (T.VtyEvent (V.EvKey (V.KChar 's') [])) -> do
          vScrollBy (viewportScroll AdminActionsViewport) 1
        -- Allow scrolling in UsersByHead viewport (only when on main menu)
        (T.VtyEvent (V.EvKey (V.KChar 'u') [])) -> do
          vScrollBy (viewportScroll UsersByHeadViewport) (-1)
        (T.VtyEvent (V.EvKey (V.KChar 'd') [])) -> do
          vScrollBy (viewportScroll UsersByHeadViewport) 1
        -- Allow scrolling in PendingDeposits viewport (only when on main menu)
        (T.VtyEvent (V.EvKey (V.KChar 'p') [])) -> do
          vScrollBy (viewportScroll PendingDepositsViewport) (-1)
        (T.VtyEvent (V.EvKey (V.KChar 'n') [])) -> do
          vScrollBy (viewportScroll PendingDepositsViewport) 1
        (T.VtyEvent (V.EvKey (V.KChar '\t') [])) -> do
          -- Switch to next service
          let currentIdx = selectedServiceIndex appState
              numServices = length (services appState)
              nextIdx = (currentIdx + 1) `mod` numServices
          -- Update pending deposits for the new service
          newPendingDeposits <- liftIO $ fetchPendingDeposits (serviceBaseUrl $ services appState !! nextIdx)
          let newRefreshCounter = refreshCounter appState + 1
          put $ appState {selectedServiceIndex = nextIdx, pendingDeposits = newPendingDeposits, refreshCounter = newRefreshCounter}
        _ -> return ()
    QueryFundsFormScreen edit ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let addr = getEditorText edit
          result <- liftIO $ execQueryFunds (currentBaseUrl appState) addr
          put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Query Funds Result", rsScrollOffset = 0}}
        _ -> do
          newEdit <- handleEditorEvent ev edit
          put $ appState {currentScreen = QueryFundsFormScreen {qfEdit = newEdit}}
    DepositFormScreen current fields ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey (V.KChar '\t') [])) -> do
          -- Handle Tab character to move to next field
          let nextField = (current + 1) `mod` length fields
          put $ appState {currentScreen = DepositFormScreen {dsfCurrentField = nextField, dsfFields = fields}}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let userAddress = getEditorText $ fields !! 0
          let publicKey = getEditorText $ fields !! 1
          let assetUnit = getEditorText $ fields !! 2
          let amount = getEditorText $ fields !! 3
          if null userAddress || null assetUnit || null amount
            then do
              put $ appState {currentScreen = ResultScreen {rsMessage = ["Error", "Address, Asset Unit, and Amount are required. Public Key is optional."], rsTitle = "Validation Error", rsScrollOffset = 0}}
            else do
              result <- liftIO $ execDeposit (currentBaseUrl appState) userAddress publicKey assetUnit amount
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Deposit Result", rsScrollOffset = 0}}
        _ -> do
          let currentEditor = fields !! current
          newEditor <- handleEditorEvent ev currentEditor
          let newFields = take current fields ++ [newEditor] ++ drop (current + 1) fields
          put $ appState {currentScreen = DepositFormScreen {dsfCurrentField = current, dsfFields = newFields}}
    WithdrawFormScreen current fields ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey (V.KChar '\t') [])) -> do
          let nextField = (current + 1) `mod` length fields
          put $ appState {currentScreen = WithdrawFormScreen {wfsfCurrentField = nextField, wfsfFields = fields}}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let address = getEditorText $ fields !! 0
          let owner = getEditorText $ fields !! 1
          let utxoHash = getEditorText $ fields !! 2
          let utxoIndex = getEditorText $ fields !! 3
          let signature = getEditorText $ fields !! 4
          if null address || null owner || null utxoHash || null utxoIndex || null signature
            then do
              put $ appState {currentScreen = ResultScreen {rsMessage = ["Error", "All fields are required. Please fill in all fields before submitting."], rsTitle = "Validation Error", rsScrollOffset = 0}}
            else do
              result <- liftIO $ execWithdraw (currentBaseUrl appState) address owner utxoHash utxoIndex signature
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Withdraw Result", rsScrollOffset = 0}}
        _ -> do
          let currentEditor = fields !! current
          newEditor <- handleEditorEvent ev currentEditor
          let newFields = take current fields ++ [newEditor] ++ drop (current + 1) fields
          put $ appState {currentScreen = WithdrawFormScreen {wfsfCurrentField = current, wfsfFields = newFields}}
    PayMerchantFormScreen current fields ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey (V.KChar '\t') [])) -> do
          let nextField = (current + 1) `mod` length fields
          put $ appState {currentScreen = PayMerchantFormScreen {pmfsCurrentField = nextField, pmfsFields = fields}}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let merchantAddress = getEditorText $ fields !! 0
          let utxoHash = getEditorText $ fields !! 1
          let utxoIndex = getEditorText $ fields !! 2
          let assetUnit = getEditorText $ fields !! 3
          let amount = getEditorText $ fields !! 4
          let signature = getEditorText $ fields !! 5
          let merchantUtxoHash = getEditorText $ fields !! 6
          let merchantUtxoIndex = getEditorText $ fields !! 7
          if null merchantAddress || null utxoHash || null utxoIndex || null assetUnit || null amount || null signature
            then do
              put $ appState {currentScreen = ResultScreen {rsMessage = ["Error", "All fields except Merchant Funds UTxO are required."], rsTitle = "Validation Error", rsScrollOffset = 0}}
            else do
              result <- liftIO $ execPayMerchant (currentBaseUrl appState) merchantAddress utxoHash utxoIndex assetUnit amount signature merchantUtxoHash merchantUtxoIndex
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Pay Merchant Result", rsScrollOffset = 0}}
        _ -> do
          let currentEditor = fields !! current
          newEditor <- handleEditorEvent ev currentEditor
          let newFields = take current fields ++ [newEditor] ++ drop (current + 1) fields
          put $ appState {currentScreen = PayMerchantFormScreen {pmfsCurrentField = current, pmfsFields = newFields}}
    OpenHeadFormScreen edit ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          -- Check if head is already open for the current service
          let currentService = selectedService appState
              currentPort = servicePort currentService
              maybeHeadInfo = find (\h -> hydraHeadPort h == currentPort) (hydraHeads appState)
          case maybeHeadInfo >>= hydraHeadId of
            Just headId -> do
              -- Head is already open, show error message
              put $ appState {currentScreen = ResultScreen {rsMessage = ["Error", "Head is already open", "", "Head ID: " ++ headId, "Service Port: " ++ show currentPort], rsTitle = "Open Head Error", rsScrollOffset = 0}}
            Nothing -> do
              -- No head open, proceed with opening
              let urls = getEditorText edit
                  currentService = selectedService appState
                  servicePort' = servicePort currentService
              result <- liftIO $ execOpenHead (dbPool appState) servicePort' (currentBaseUrl appState) urls
              -- Refresh Hydra Heads, users by head, and pending deposits after opening head
              newHeads <- liftIO $ fetchHydraHeads (services appState)
              newUsersByHead <- liftIO $ fetchUsersByHead (dbPool appState) newHeads
              newPendingDeposits <- liftIO $ fetchPendingDeposits (currentBaseUrl appState)
              let newRefreshCounter = refreshCounter appState + 1
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Open Head Result", rsScrollOffset = 0}, hydraHeads = newHeads, usersByHead = newUsersByHead, pendingDeposits = newPendingDeposits, refreshCounter = newRefreshCounter}
        _ -> do
          newEdit <- handleEditorEvent ev edit
          put $ appState {currentScreen = OpenHeadFormScreen {ohEdit = newEdit}}
    CloseHeadFormScreen ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          -- Get head ID for current service
          let currentService = selectedService appState
              currentPort = servicePort currentService
              maybeHeadInfo = find (\h -> hydraHeadPort h == currentPort) (hydraHeads appState)
          case maybeHeadInfo >>= hydraHeadId of
            Nothing -> do
              -- Head is not open, show error message
              put $ appState {currentScreen = ResultScreen {rsMessage = ["Error", "The head is not open for service port " ++ show currentPort], rsTitle = "Close Head Error", rsScrollOffset = 0}}
            Just headId -> do
              -- Close the head using the found ID
              let currentService = selectedService appState
                  servicePort' = servicePort currentService
              result <- liftIO $ execCloseHead (dbPool appState) servicePort' (currentBaseUrl appState) headId
              -- Refresh Hydra Heads, users by head, and pending deposits after closing head
              newHeads <- liftIO $ fetchHydraHeads (services appState)
              newUsersByHead <- liftIO $ fetchUsersByHead (dbPool appState) newHeads
              newPendingDeposits <- liftIO $ fetchPendingDeposits (currentBaseUrl appState)
              let newRefreshCounter = refreshCounter appState + 1
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Close Head Result", rsScrollOffset = 0}, hydraHeads = newHeads, usersByHead = newUsersByHead, pendingDeposits = newPendingDeposits, refreshCounter = newRefreshCounter}
        _ -> return ()
    ResultScreen _ _ _scrollOffset ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KUp [])) -> do
          vScrollBy (viewportScroll ResultViewport) (-1)
        (T.VtyEvent (V.EvKey V.KDown [])) -> do
          vScrollBy (viewportScroll ResultViewport) 1
        (T.VtyEvent (V.EvKey V.KPageUp [])) -> do
          vScrollBy (viewportScroll ResultViewport) (-10)
        (T.VtyEvent (V.EvKey V.KPageDown [])) -> do
          vScrollBy (viewportScroll ResultViewport) 10
        (T.VtyEvent (V.EvKey V.KHome [])) -> do
          M.vScrollToBeginning (viewportScroll ResultViewport)
        (T.VtyEvent (V.EvKey V.KEnd [])) -> do
          M.vScrollToEnd (viewportScroll ResultViewport)
        _ -> return ()

-- Helper function that wraps the editor state action in our app state
handleEditorEvent :: T.BrickEvent ResourceName e -> E.Editor String ResourceName -> T.EventM ResourceName AppState (E.Editor String ResourceName)
handleEditorEvent ev edit = do
  (newEditor, _) <- T.nestEventM edit $ E.handleEditorEvent ev
  return newEditor

-- | Fetch Hydra Heads for all services
fetchHydraHeads :: [Service] -> IO [HydraHeadInfo]
fetchHydraHeads services = do
  -- Fetch head for each service sequentially
  mapM fetchHeadForService services
  where
    fetchHeadForService :: Service -> IO HydraHeadInfo
    fetchHeadForService service = do
      let baseUrl = serviceBaseUrl service
      result <- runHydraClient baseUrl getHead
      case result of
        Left _ -> return $ HydraHeadInfo {hydraHeadPort = servicePort service, hydraHeadId = Nothing}
        Right maybeHeadId -> return $ HydraHeadInfo {hydraHeadPort = servicePort service, hydraHeadId = maybeHeadId}

-- | Persist deposits for open heads that don't have deposits in the database yet
-- This helps recover deposits for heads that were opened before persistence was implemented
persistMissingDeposits :: Sqlite.ConnectionPool -> [Service] -> [HydraHeadInfo] -> IO ()
persistMissingDeposits dbPool' services heads = do
  -- For each open head, check if it has deposits in the database using service port
  mapM_ (\headInfo -> do
    case hydraHeadId headInfo of
      Nothing -> return ()  -- Skip closed heads
      Just "OPENING" -> return ()  -- Skip opening heads
      Just _ -> do
        let port = hydraHeadPort headInfo
        -- Check if this service port has deposits in the database
        existingAddresses <- runDB dbPool' $ getDepositsByServicePort port
        -- If no deposits exist, try to fetch and persist uncommitted deposits
        if null existingAddresses
          then do
            -- Find the corresponding service
            let maybeService = find (\s -> servicePort s == port) services
            case maybeService of
              Just service -> do
                let serviceUrl = serviceBaseUrl service
                -- Fetch uncommitted deposits for this service
                uncommittedResult <- runHydraClient serviceUrl getUncommittedDeposits
                case uncommittedResult of
                  Right uncommittedResp -> do
                    let deposits = uncommittedDepositsDeposits uncommittedResp
                    if not (Vec.null deposits)
                      then runDB dbPool' $ insertCommittedDeposits port deposits
                      else return ()
                  Left _ -> return ()  -- Ignore errors
              Nothing -> return ()
          else return ()
    ) heads
  return ()

-- | Fetch users by head from database (using service port)
fetchUsersByHead :: Sqlite.ConnectionPool -> [HydraHeadInfo] -> IO UsersByHead
fetchUsersByHead dbPool' heads = do
  userLists <- mapM (\headInfo -> do
    case hydraHeadId headInfo of
      Nothing -> return (show (hydraHeadPort headInfo) ++ " (Closed)", [])
      Just "OPENING" -> return (show (hydraHeadPort headInfo) ++ " (OPENING)", [])
      Just headId -> do
        let port = hydraHeadPort headInfo
        addresses <- runDB dbPool' $ getDepositsByServicePort port
        return (headId, map Text.unpack addresses)
    ) heads
  return userLists

-- | Fetch pending deposits (uncommitted deposits) from API
fetchPendingDeposits :: String -> IO PendingDeposits
fetchPendingDeposits baseUrl = do
  result <- runHydraClient baseUrl getUncommittedDeposits
  case result of
    Left _ -> return []  -- Return empty list on error
    Right resp -> do
      let depositsVector = uncommittedDepositsDeposits resp
          deposits = Vec.toList depositsVector
      return deposits

-- | Run the TUI application
runTUI :: [Service] -> IO ()
runTUI servicesList = do
  -- Suppress SQL logging by redirecting stderr temporarily or using NoLoggingT
  -- Initialize database
  dbPool' <- initDatabase "./committed-deposits.db"

  splashLogoContent <- readFile "./ascii_art/SplashLogo.txt"
  headerLogoContent <- readFile "./ascii_art/HeaderLogo.txt"
  let splashLogoLines = lines splashLogoContent
      headerLogoLines = lines headerLogoContent
      -- Ensure we have at least one service
      defaultServices = if null servicesList
                         then [Service "Local" "127.0.0.1" 3001]
                         else servicesList
  -- Fetch initial Hydra Heads data
  initialHeads <- fetchHydraHeads defaultServices
  initialUsersByHead <- fetchUsersByHead dbPool' initialHeads
  -- Fetch initial pending deposits from first service
  let firstServiceUrl = serviceBaseUrl $ head defaultServices
  initialPendingDeposits <- fetchPendingDeposits firstServiceUrl
  let initialState =
        AppState
          { currentScreen = SplashScreen
          , services = defaultServices
          , selectedServiceIndex = 0
          , splashLogo = splashLogoLines
          , headerLogo = headerLogoLines
          , hydraHeads = initialHeads
          , dbPool = dbPool'
          , usersByHead = initialUsersByHead
          , pendingDeposits = initialPendingDeposits
          , refreshCounter = 0
          }
  let app =
        M.App
          { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = handleEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }
  _ <- M.defaultMain app initialState
  return ()

