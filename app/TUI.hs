{-# LANGUAGE OverloadedStrings #-}

module TUI where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core qualified as W
import Brick.Widgets.Edit qualified as E
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as Vec
import Graphics.Vty qualified as V
import HydraPay.API.Types
import HydraPay.Client
import MyLib

-- | Name for different UI resources
data ResourceName
  = EditField
  deriving (Eq, Ord, Show)

-- | Current screen shown to the user
data Screen
  = MainMenuScreen
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
  deriving (Show)

-- | Application state
data AppState = AppState
  { currentScreen :: Screen
  , apiBaseUrl :: String
  }
  deriving (Show)

-- | Drawing function
drawUI :: AppState -> [T.Widget ResourceName]
drawUI appState =
  let widget = case currentScreen appState of
        MainMenuScreen -> drawMainMenu
        QueryFundsFormScreen edit -> drawQueryFundsForm edit
        DepositFormScreen current fields -> drawDepositForm current fields
        WithdrawFormScreen current fields -> drawWithdrawForm current fields
        PayMerchantFormScreen current fields -> drawPayMerchantForm current fields
        OpenHeadFormScreen edit -> drawOpenHeadForm edit
        CloseHeadFormScreen edit -> drawCloseHeadForm edit
  in [widget]

-- | Main menu
drawMainMenu :: T.Widget ResourceName
drawMainMenu =
  C.center $
    B.borderWithLabel (W.str "Hydra Pay Load Balancer") $
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
        , W.str "Press Enter to execute, Esc to go back"
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
        , W.str "Press Enter to execute, Esc to go back"
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
        , W.str "Press Enter to execute, Esc to go back"
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

-- | Attribute map for styling
theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

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
    MainMenuScreen ->
      case ev of
        (T.VtyEvent (V.EvKey (V.KChar '1') [])) -> do
          put $ appState {currentScreen = QueryFundsFormScreen {qfEdit = E.editor EditField (Just 1) ""}}
        (T.VtyEvent (V.EvKey (V.KChar '2') [])) -> do
          put $
            appState
              { currentScreen =
                  DepositFormScreen
                    { dsfCurrentField = 0
                    , dsfFields =
                        [ E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
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
                        [ E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) "user"
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
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
                        [ E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        , E.editor EditField (Just 1) ""
                        ]
                    }
              }
        (T.VtyEvent (V.EvKey (V.KChar '5') [])) -> do
          put $ appState {currentScreen = OpenHeadFormScreen {ohEdit = E.editor EditField (Just 1) ""}}
        (T.VtyEvent (V.EvKey (V.KChar '6') [])) -> do
          put $ appState {currentScreen = CloseHeadFormScreen {chEdit = E.editor EditField (Just 1) ""}}
        (T.VtyEvent (V.EvKey (V.KChar 'q') [])) -> M.halt
        (T.VtyEvent (V.EvKey V.KEsc [])) -> M.halt
        _ -> return ()
    QueryFundsFormScreen edit ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let addr = getEditorText edit
          liftIO $ execQueryFunds (apiBaseUrl appState) addr
        _ -> do
          newEdit <- handleEditorEvent ev edit
          put $ appState {currentScreen = QueryFundsFormScreen {qfEdit = newEdit}}
    DepositFormScreen current fields ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          liftIO $ execDeposit (apiBaseUrl appState) (getEditorText $ fields !! 0) (getEditorText $ fields !! 1) (getEditorText $ fields !! 2) (getEditorText $ fields !! 3)
        _ -> do
          let currentEditor = fields !! current
          newEditor <- handleEditorEvent ev currentEditor
          let newFields = take current fields ++ [newEditor] ++ drop (current + 1) fields
          put $ appState {currentScreen = DepositFormScreen {dsfCurrentField = current, dsfFields = newFields}}
    WithdrawFormScreen current fields ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          liftIO $ execWithdraw (apiBaseUrl appState) (getEditorText $ fields !! 0) (getEditorText $ fields !! 1) (getEditorText $ fields !! 2) (getEditorText $ fields !! 3) (getEditorText $ fields !! 4)
        _ -> do
          let currentEditor = fields !! current
          newEditor <- handleEditorEvent ev currentEditor
          let newFields = take current fields ++ [newEditor] ++ drop (current + 1) fields
          put $ appState {currentScreen = WithdrawFormScreen {wfsfCurrentField = current, wfsfFields = newFields}}
    PayMerchantFormScreen current fields ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          liftIO $ execPayMerchant (apiBaseUrl appState) (getEditorText $ fields !! 0) (getEditorText $ fields !! 1) (getEditorText $ fields !! 2) (getEditorText $ fields !! 3) (getEditorText $ fields !! 4) (getEditorText $ fields !! 5) (getEditorText $ fields !! 6) (getEditorText $ fields !! 7)
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
          liftIO $ execOpenHead (apiBaseUrl appState) urls
        _ -> do
          newEdit <- handleEditorEvent ev edit
          put $ appState {currentScreen = OpenHeadFormScreen {ohEdit = newEdit}}
    CloseHeadFormScreen edit ->
      case ev of
        (T.VtyEvent (V.EvKey V.KEsc [])) -> do
          put $ appState {currentScreen = MainMenuScreen}
        (T.VtyEvent (V.EvKey V.KEnter [])) -> do
          let headId = getEditorText edit
          liftIO $ execCloseHead (apiBaseUrl appState) headId
        _ -> do
          newEdit <- handleEditorEvent ev edit
          put $ appState {currentScreen = CloseHeadFormScreen {chEdit = newEdit}}

-- Helper function that wraps the editor state action in our app state
handleEditorEvent :: T.BrickEvent ResourceName e -> E.Editor String ResourceName -> T.EventM ResourceName AppState (E.Editor String ResourceName)
handleEditorEvent ev edit = do
  (newEditor, _) <- T.nestEventM edit $ E.handleEditorEvent ev
  return newEditor

-- | Execute query funds
execQueryFunds :: String -> String -> IO ()
execQueryFunds baseUrl addr = do
  result <- runHydraClient baseUrl $ queryFunds addr
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right funds -> putStrLn $ TL.unpack $ TLE.decodeUtf8 $ encodePretty funds

-- | Execute deposit
execDeposit :: String -> String -> String -> String -> String -> IO ()
execDeposit baseUrl userAddress publicKey assetUnit amount = do
  let amountVec = Vec.fromList [Vec.fromList [Text.pack assetUnit, Text.pack amount]]
  let depositReq =
        DepositSchema
          { depositUserAddress = Text.pack userAddress
          , depositPublicKey = Text.pack publicKey
          , depositAmount = amountVec
          , depositFundsUtxoRef = Nothing
          }
  result <- runHydraClient baseUrl $ deposit depositReq
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right tx -> putStrLn $ TL.unpack $ TLE.decodeUtf8 $ encodePretty tx

-- | Execute withdraw
execWithdraw :: String -> String -> String -> String -> String -> String -> IO ()
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
  result <- runHydraClient baseUrl $ withdraw withdrawReq
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right tx -> putStrLn $ TL.unpack $ TLE.decodeUtf8 $ encodePretty tx

-- | Execute pay merchant
execPayMerchant :: String -> String -> String -> String -> String -> String -> String -> String -> String -> IO ()
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
  let amountVec = Vec.fromList [Vec.fromList [Text.pack assetUnit, Text.pack amount]]
  let payMerchantReq =
        PayMerchantSchema
          { payMerchantMerchantAddress = Text.pack merchantAddress
          , payMerchantFundsUtxoRef =
              TxOutRef
                { txOutRefHash = Text.pack utxoHash
                , txOutRefIndex = read utxoIndexStr
                }
          , payMerchantAmount = amountVec
          , payMerchantSignature = Text.pack signature
          , payMerchantMerchantFundsUtxo = merchantUtxo
          }
  result <- runHydraClient baseUrl $ payMerchant payMerchantReq
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right tx -> putStrLn $ TL.unpack $ TLE.decodeUtf8 $ encodePretty tx

-- | Execute open head
execOpenHead :: String -> String -> IO ()
execOpenHead baseUrl urlsStr = do
  let urls = fmap Text.pack $ words urlsStr
  let openHeadReq = ManageHeadSchema {manageHeadPeerApiUrls = Vec.fromList urls}
  result <- runHydraClient baseUrl $ openHead openHeadReq
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> putStrLn "Head opened successfully!"

-- | Execute close head
execCloseHead :: String -> String -> IO ()
execCloseHead _baseUrl headId = do
  putStrLn $ "Close Head operation requires proper endpoint implementation: " ++ headId

-- | Run the TUI application
runTUI :: String -> IO ()
runTUI baseUrl = do
  let initialState =
        AppState
          { currentScreen = MainMenuScreen
          , apiBaseUrl = baseUrl
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

