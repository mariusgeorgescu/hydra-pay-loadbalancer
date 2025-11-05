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
import Data.Aeson (encode, toJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List (isInfixOf, find)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import Control.Concurrent (threadDelay)
import Control.Exception (catch, SomeException)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (statusCode)
import HydraPay.API.Types (HeadStateResponse(..))
import HydraPay.Client (runHydraClient, HydraClientError(..), formatError, getHeadState, getHead)
import MyLib
import System.IO
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
  | ActionsViewport  -- Viewport for scrolling Actions menu
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
  }
  deriving (Show)

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
      
      -- Dashboard zone (top): Hydra Heads widget
      headsTable = drawHydraHeadsTable heads
      dashboardZone = B.borderWithLabel (W.str "Dashboard") headsTable
      
      -- Actions menu (right side) with scrollbar
      menuOptions = W.vBox
        [ W.str "Select an operation:"
        , W.str ""
        , W.str "  1. Query Funds"
        , W.str "  2. Deposit"
        , W.str "  3. Withdraw"
        , W.str "  4. Pay Merchant"
        , W.str "  5. Open Head"
        , W.str "  6. Close Head"
        , W.str ""
        , W.str "  r. Refresh Heads"
        , W.str "  q. Quit"
        ]
      viewportWidget = W.viewport ActionsViewport T.Vertical menuOptions
      viewportWithScrollbar = W.withVScrollBars T.OnRight viewportWidget
      actionsBox = B.borderWithLabel (W.str "Actions") viewportWithScrollbar
      
      -- Services widget (left side)
      servicesWidget = drawServicesPanel appState
      
      -- Admin zone (bottom): Services on left, Actions on right
      adminZone = B.borderWithLabel (W.str "Admin") $
        W.hBox [servicesWidget, W.str "  ", actionsBox]
      
      -- Full layout: Logo centered on top, Dashboard and Admin centered horizontally
  in W.vBox
       [ logoWidget
       , W.str ""
       , C.center dashboardZone
       , W.str ""
       , C.center adminZone
       ]

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
    , (A.attrName "selected", Util.fg V.green `V.withStyle` V.bold)
    , (A.attrName "headOpen", Util.fg V.yellow)
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
          -- Refresh Hydra Heads data
          newHeads <- liftIO $ fetchHydraHeads (services appState)
          put $ appState {hydraHeads = newHeads}
        (T.VtyEvent (V.EvKey (V.KChar 'q') [])) -> M.halt
        (T.VtyEvent (V.EvKey V.KEsc [])) -> M.halt
        -- Allow scrolling in Actions viewport
        (T.VtyEvent (V.EvKey V.KUp [])) -> do
          vScrollBy (viewportScroll ActionsViewport) (-1)
        (T.VtyEvent (V.EvKey V.KDown [])) -> do
          vScrollBy (viewportScroll ActionsViewport) 1
        (T.VtyEvent (V.EvKey V.KPageUp [])) -> do
          vScrollBy (viewportScroll ActionsViewport) (-5)
        (T.VtyEvent (V.EvKey V.KPageDown [])) -> do
          vScrollBy (viewportScroll ActionsViewport) 5
        (T.VtyEvent (V.EvKey (V.KChar '\t') [])) -> do
          -- Switch to next service
          let currentIdx = selectedServiceIndex appState
              numServices = length (services appState)
              nextIdx = (currentIdx + 1) `mod` numServices
          put $ appState {selectedServiceIndex = nextIdx}
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
          let urls = getEditorText edit
          result <- liftIO $ execOpenHead (currentBaseUrl appState) urls
          -- Refresh Hydra Heads after opening head
          newHeads <- liftIO $ fetchHydraHeads (services appState)
          put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Open Head Result", rsScrollOffset = 0}, hydraHeads = newHeads}
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
              result <- liftIO $ execCloseHead (currentBaseUrl appState) headId
              -- Refresh Hydra Heads after closing head
              newHeads <- liftIO $ fetchHydraHeads (services appState)
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Close Head Result", rsScrollOffset = 0}, hydraHeads = newHeads}
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

-- | Helper to format request debug info (for ToJSON requests)
formatRequestJsonDebug :: ToJSON req => req -> [String]
formatRequestJsonDebug req =
  let debugJsonPretty = TL.unpack $ TLE.decodeUtf8 $ encodePretty req
      debugJsonCompact = TL.unpack $ TLE.decodeUtf8 $ encode req
  in ["Request JSON (Compact):", debugJsonCompact, "", "Request JSON (Pretty):", debugJsonPretty, ""]

-- | Helper to format request debug info (for non-ToJSON requests like query-funds)
formatRequestDebug :: String -> [String]
formatRequestDebug requestStr = ["Request:", requestStr, ""]

-- | Execute a client action with debug info and error formatting
-- This helper consolidates the common pattern of:
-- 1. Building a request
-- 2. Encoding it to JSON for debug output
-- 3. Running the client action
-- 4. Formatting success/error responses
execWithDebug :: (ToJSON req, ToJSON res) => String -> req -> (req -> IO (Either HydraClientError res)) -> IO [String]
execWithDebug _baseUrl req clientFn = do
  let debugInfo = formatRequestJsonDebug req
  result <- clientFn req

  case result of
    Right res ->
      return $ ["Success!", ""] ++ debugInfo ++ [TL.unpack $ TLE.decodeUtf8 $ encodePretty res]
    Left err ->
      return $ debugInfo ++ formatError err

-- | Execute query funds
execQueryFunds :: String -> String -> IO [String]
execQueryFunds baseUrl addr = do
  let debugInfo = formatRequestDebug $ "Address: " ++ addr
  result <- runHydraClient baseUrl $ queryFunds addr
  case result of
    Left err -> return $ debugInfo ++ formatError err
    Right funds -> return $ ["Success!", ""] ++ debugInfo ++ [TL.unpack $ TLE.decodeUtf8 $ encodePretty funds]

-- | Execute deposit
execDeposit :: String -> String -> String -> String -> String -> IO [String]
execDeposit baseUrl userAddress publicKey assetUnit amount = do
  let amountList = [(Text.pack assetUnit, read amount :: Integer)]
  let userAddrMaybe = if null userAddress then Nothing else Just $ Text.pack userAddress
  let pubKeyMaybe = if null publicKey then Nothing else Just $ Text.pack publicKey
  let depositReq =
        DepositSchema
          { depositUserAddress = userAddrMaybe
          , depositPublicKey = pubKeyMaybe
          , depositAmount = amountList
          , depositFundsUtxoRef = Nothing
          }
  execWithDebug baseUrl depositReq (\r -> runHydraClient baseUrl $ deposit r)

-- | Execute withdraw
execWithdraw :: String -> String -> String -> String -> String -> String -> IO [String]
execWithdraw baseUrl address owner utxoHash utxoIndexStr signature = do
  let utxo =
        FundsUtxo
          { fundsUtxoSignature = Just $ Text.pack signature
          , fundsUtxoRef =
              TxOutRef
                { txOutRefHash = Text.pack utxoHash
                , txOutRefIndex = read utxoIndexStr
                }
          }
  let withdrawReq =
        WithdrawSchema
          { withdrawAddress = Text.pack address
          , withdrawOwner = Text.pack owner
          , withdrawFundsUtxos = Vec.fromList [utxo]
          , withdrawNetworkLayer = "L1"
          }
  execWithDebug baseUrl withdrawReq (runHydraClient baseUrl . withdraw)

-- | Execute pay merchant
execPayMerchant :: String -> String -> String -> String -> String -> String -> String -> String -> String -> IO [String]
execPayMerchant baseUrl merchantAddress utxoHash utxoIndexStr assetUnit amount signature merchantUtxoHash merchantUtxoIndexStr = do
  let merchantUtxo =
        if null merchantUtxoHash
          then Nothing
          else
            Just
              TxOutRef
                { txOutRefHash = Text.pack merchantUtxoHash
                , txOutRefIndex = read merchantUtxoIndexStr
                }
  let amountList = [(Text.pack assetUnit, read amount :: Integer)]
  let payMerchantReq =
        PayMerchantSchema
          { payMerchantMerchantAddress = Text.pack merchantAddress
          , payMerchantFundsUtxoRef =
              TxOutRef
                { txOutRefHash = Text.pack utxoHash
                , txOutRefIndex = read utxoIndexStr
                }
          , payMerchantAmount = amountList
          , payMerchantSignature = Text.pack signature
          , payMerchantMerchantFundsUtxo = merchantUtxo
          }
  execWithDebug baseUrl payMerchantReq (\r -> runHydraClient baseUrl $ payMerchant r)

-- | Execute open head
execOpenHead :: String -> String -> IO [String]
execOpenHead baseUrl urlsStr = do
  let urls = fmap Text.pack $ words urlsStr
  let openHeadReq = ManageHeadSchema {manageHeadPeerApiUrls = Vec.fromList urls}
  let debugInfo = formatRequestJsonDebug openHeadReq

  result <- runHydraClient baseUrl $ openHead openHeadReq
  case result of
    Left err -> return $ debugInfo ++ formatError err
    Right opResp -> return $ ["Success!", "Head opened successfully!", "", "Operation ID: " ++ Text.unpack (operationResponseOperationId opResp), ""] ++ debugInfo

-- | Execute close head
execCloseHead :: String -> String -> IO [String]
execCloseHead baseUrl headId = do
  let debugInfo = formatRequestDebug $ "Head ID: " ++ headId
  result <- runHydraClient baseUrl $ closeHead headId
  case result of
    Left err -> return $ debugInfo ++ formatError err
    Right stateResp -> return $ ["Success!", "Head closed successfully!", "", "Status: " ++ Text.unpack (headStateStatus stateResp), ""] ++ debugInfo

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

-- | Run the TUI application
runTUI :: [Service] -> IO ()
runTUI servicesList = do
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
  let initialState =
        AppState
          { currentScreen = SplashScreen
          , services = defaultServices
          , selectedServiceIndex = 0
          , splashLogo = splashLogoLines
          , headerLogo = headerLogoLines
          , hydraHeads = initialHeads
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

