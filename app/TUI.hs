{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use <$>" #-}

module TUI where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
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
import Data.List (isInfixOf)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import HydraPay.API.Types (HeadStateResponse(..))
import HydraPay.Client (runHydraClient, HydraClientError(..), formatError, getHeadState)
import MyLib
import System.IO

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
  | StateHeadIdField
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
  | CloseHeadFormScreen {chEdit :: E.Editor String ResourceName}
  | StateHeadFormScreen {shEdit :: E.Editor String ResourceName}
  | ResultScreen {rsMessage :: [String], rsTitle :: String}
  deriving (Show)

-- | Application state
data AppState = AppState
  { currentScreen :: Screen
  , apiBaseUrl :: String
  , splashLogo :: [String]
  }
  deriving (Show)

-- | Drawing function
drawUI :: AppState -> [T.Widget ResourceName]
drawUI appState =
  let widget = case currentScreen appState of
        SplashScreen -> drawSplashScreen (splashLogo appState)
        MainMenuScreen -> drawMainMenu
        QueryFundsFormScreen edit -> drawQueryFundsForm edit
        DepositFormScreen current fields -> drawDepositForm current fields
        WithdrawFormScreen current fields -> drawWithdrawForm current fields
        PayMerchantFormScreen current fields -> drawPayMerchantForm current fields
        OpenHeadFormScreen edit -> drawOpenHeadForm edit
        CloseHeadFormScreen edit -> drawCloseHeadForm edit
        StateHeadFormScreen edit -> drawStateHeadForm edit
        ResultScreen message title -> drawResultScreen message title
  in [widget]

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

-- | Main menu
drawMainMenu :: T.Widget ResourceName
drawMainMenu =
  C.center $
    B.borderWithLabel (W.str "Blazar Pay Admin") $
      W.vBox
        [ W.str ""
        , W.str "Select an operation:"
        , W.str ""
        , W.str "  1. Query Funds"
        , W.str "  2. Deposit"
        , W.str "  3. Withdraw"
        , W.str "  4. Pay Merchant"
        , W.str "  5. Open Head"
        , W.str "  6. Close Head"
        , W.str "  7. Check Head State"
        , W.str ""
        , W.str "  q. Quit"
        , W.str ""
        ]

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

-- | Close head form
drawCloseHeadForm :: E.Editor String ResourceName -> T.Widget ResourceName
drawCloseHeadForm edit =
  C.center $
    B.borderWithLabel (W.str "Close Head") $
      W.vBox
        [ W.str ""
        , W.str "Enter Head ID:"
        , W.str ""
        , E.renderEditor (W.str . unlines) True edit
        , W.str ""
        , W.str "Press Enter to execute, Esc to go back"
        , W.str ""
        ]

-- | State head form
drawStateHeadForm :: E.Editor String ResourceName -> T.Widget ResourceName
drawStateHeadForm edit =
  C.center $
    B.borderWithLabel (W.str "Check Head State") $
      W.vBox
        [ W.str ""
        , W.str "Enter Head ID:"
        , W.str ""
        , E.renderEditor (W.str . unlines) True edit
        , W.str ""
        , W.str "Press Enter to execute, Esc to go back"
        , W.str ""
        ]

-- | Result screen
drawResultScreen :: [String] -> String -> T.Widget ResourceName
drawResultScreen message title =
  C.center $
    B.borderWithLabel (W.str title) $
      W.vBox
        [ W.str ""
        , W.vBox $ map W.str message
        , W.str ""
        , W.str "Press Esc to go back"
        , W.str ""
        ]

-- | Attribute map for styling
theMap :: A.AttrMap
theMap =
  A.attrMap V.defAttr
    [ (A.attrName "cyan", Util.fg V.cyan)
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
          put $ appState {currentScreen = OpenHeadFormScreen {ohEdit = E.editor OpenHeadUrlsField (Just 1) ""}}
        (T.VtyEvent (V.EvKey (V.KChar '6') [])) -> do
          put $ appState {currentScreen = CloseHeadFormScreen {chEdit = E.editor CloseHeadIdField (Just 1) ""}}
        (T.VtyEvent (V.EvKey (V.KChar '7') [])) -> do
          put $ appState {currentScreen = StateHeadFormScreen {shEdit = E.editor StateHeadIdField (Just 1) ""}}
        (T.VtyEvent (V.EvKey (V.KChar 'q') [])) -> M.halt
        (T.VtyEvent (V.EvKey V.KEsc [])) -> M.halt
        _ -> return ()
    QueryFundsFormScreen edit ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let addr = getEditorText edit
          result <- liftIO $ execQueryFunds (apiBaseUrl appState) addr
          put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Query Funds Result"}}
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
              put $ appState {currentScreen = ResultScreen {rsMessage = ["Error", "Address, Asset Unit, and Amount are required. Public Key is optional."], rsTitle = "Validation Error"}}
            else do
              result <- liftIO $ execDeposit (apiBaseUrl appState) userAddress publicKey assetUnit amount
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Deposit Result"}}
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
              put $ appState {currentScreen = ResultScreen {rsMessage = ["Error", "All fields are required. Please fill in all fields before submitting."], rsTitle = "Validation Error"}}
            else do
              result <- liftIO $ execWithdraw (apiBaseUrl appState) address owner utxoHash utxoIndex signature
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Withdraw Result"}}
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
              put $ appState {currentScreen = ResultScreen {rsMessage = ["Error", "All fields except Merchant Funds UTxO are required."], rsTitle = "Validation Error"}}
            else do
              result <- liftIO $ execPayMerchant (apiBaseUrl appState) merchantAddress utxoHash utxoIndex assetUnit amount signature merchantUtxoHash merchantUtxoIndex
              put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Pay Merchant Result"}}
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
          result <- liftIO $ execOpenHead (apiBaseUrl appState) urls
          put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Open Head Result"}}
        _ -> do
          newEdit <- handleEditorEvent ev edit
          put $ appState {currentScreen = OpenHeadFormScreen {ohEdit = newEdit}}
    CloseHeadFormScreen edit ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let headId = getEditorText edit
          result <- liftIO $ execCloseHead (apiBaseUrl appState) headId
          put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Close Head Result"}}
        _ -> do
          newEdit <- handleEditorEvent ev edit
          put $ appState {currentScreen = CloseHeadFormScreen {chEdit = newEdit}}
    StateHeadFormScreen edit ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let headId = getEditorText edit
          result <- liftIO $ execHeadState (apiBaseUrl appState) headId
          put $ appState {currentScreen = ResultScreen {rsMessage = result, rsTitle = "Head State Result"}}
        _ -> do
          newEdit <- handleEditorEvent ev edit
          put $ appState {currentScreen = StateHeadFormScreen {shEdit = newEdit}}
    ResultScreen _ _ ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
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
    Right _ -> return $ ["Success!", "Head opened successfully!", ""] ++ debugInfo

-- | Execute close head
execCloseHead :: String -> String -> IO [String]
execCloseHead baseUrl headId = do
  let debugInfo = formatRequestDebug $ "Head ID: " ++ headId
  result <- runHydraClient baseUrl $ closeHead headId
  case result of
    Left err -> return $ debugInfo ++ formatError err
    Right _ -> return $ ["Success!", "Head closed successfully!", ""] ++ debugInfo

-- | Execute get head state
execHeadState :: String -> String -> IO [String]
execHeadState baseUrl headId = do
  let debugInfo = formatRequestDebug $ "Head ID: " ++ headId
  result <- runHydraClient baseUrl $ getHeadState headId
  case result of
    Left err -> return $ debugInfo ++ formatError err
    Right state -> return $ ["Success!", ""] ++ debugInfo ++ ["Status: " ++ show (headStateStatus state)]

-- | Run the TUI application
runTUI :: String -> IO ()
runTUI baseUrl = do
  logoContent <- readFile "BlazaLabsAsciiLogo.txt"
  let logoLines = lines logoContent
  let initialState =
        AppState
          { currentScreen = SplashScreen
          , apiBaseUrl = baseUrl
          , splashLogo = logoLines
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

