{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Client3 where
import           Control.Monad              (Monad (..), void)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Error.Lens   (throwing)
import           Data.Text.Prettyprint.Doc  hiding ((<>))
import           Control.Lens
-- import           Data.Map                   (Map)
-- import qualified Data.Map                   as Map
import           Data.Maybe                 (maybeToList)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Language.PlutusTx.AssocMap (Map)
import qualified Language.PlutusTx.AssocMap as Map
import           Language.Plutus.Contract
import qualified Language.Plutus.Contract.Tx  as Tx
import           Language.Marlowe.Semantics hiding (Contract)
import qualified Language.PlutusCore.Universe          as PLC
import qualified Language.Marlowe.Semantics as Marlowe
import qualified Language.PlutusTx          as PlutusTx
import qualified Language.PlutusTx.Prelude  as P
import           Ledger                     (SlotRange, TxInfo, CurrencySymbol, ValidatorCtx(..),TokenName, Address(..),Datum (..), PubKeyHash (..), pubKeyHash, Slot (..), Tx, TxOut (..), TxOutRef(..), TxOutTx (..), interval,

                                             mkValidatorScript, pubKeyHashTxOut, scriptAddress, scriptTxIn, scriptTxOut, validatorHash, txOutDatum, txOutTxDatum,
                                             txOutRefs, scriptTxOut', valueSpent, pubKeyAddress)
import           Ledger.Ada                 (adaSymbol, adaValueOf)
import           Ledger.AddressMap                 (outRefMap)
import           Ledger.Interval
import           Language.Plutus.Contract.StateMachine (AsSMContractError, StateMachine (..), Void)
import qualified Language.Plutus.Contract.StateMachine as SM
import           Ledger.Scripts             (Redeemer (..), Validator)
import qualified Ledger.Typed.Scripts       as Scripts
import Ledger.Constraints
import Ledger.Constraints.TxConstraints
import qualified Ledger.Value               as Val
import Debug.Trace

type MarloweSchema =
    BlockchainActions
        .\/ Endpoint "create" (MarloweParams, Marlowe.Contract)
        .\/ Endpoint "apply-inputs" (MarloweParams, [Input])
        .\/ Endpoint "sub" MarloweParams

data MarloweError =
    StateMachineError (SM.SMContractError MarloweData [Input])
    | OtherContractError ContractError
  deriving (Show)

makeClassyPrisms ''MarloweError

instance AsSMContractError MarloweError MarloweData [Input] where
    _SMContractError = _StateMachineError

instance AsContractError MarloweError where
    _ContractError = _OtherContractError


marloweContract2 :: forall e. (AsContractError e
    , AsSMContractError e MarloweData [Input]
    )
    => Contract MarloweSchema e ()
marloweContract2 = do
    create `select` apply {- <|> void sub -}
  where
    create = do
        -- traceM "Here create"
        (params, cont) <- endpoint @"create" @(MarloweParams, Marlowe.Contract) @MarloweSchema
        -- traceM $ "Here cont " <> show cont
        createContract params cont
        void apply
    apply = do
        -- traceM "Here apply"
        (params, inputs) <- endpoint @"apply-inputs" @(MarloweParams, [Input]) @MarloweSchema
        -- traceM $ "Here endpoint " <> show inputs
        MarloweData{..} <- applyInputs params inputs
        case marloweContract of
            Close -> pure ()
            _ -> void apply


{-| Create a Marlowe contract.
    Uses wallet public key to generate a unique script address.
 -}
createContract :: (AsContractError e, AsSMContractError e MarloweData [Input])
    => MarloweParams
    -> Marlowe.Contract
    -> Contract MarloweSchema e ()
createContract params contract = do
    slot <- awaitSlot 0
    creator <- pubKeyHash <$> ownPubKey
    -- traceM $ "createContract: " <> show slot <> " " <> show creator
    let
        inst = scriptInstance params

        marloweData = MarloweData {
            marloweContract = contract,
            marloweState = emptyState slot }
        ds = Datum $ PlutusTx.toData marloweData

    let payValue = adaValueOf 0
    let theClient = client params

    void $ SM.runInitialise theClient marloweData payValue


applyInputs :: (AsContractError e, AsSMContractError e MarloweData [Input])
    => MarloweParams
    -> [Input]
    -> Contract MarloweSchema e MarloweData
applyInputs params inputs = do
    (Slot slot) <- awaitSlot 1
    -- let slot = Slot 0
    let slotRange = interval (Slot $ slot - 1)  (Slot $ slot + 10)
    traceM $ "slotRange: " <> show slotRange
    let inst = scriptInstance params
        address = (Scripts.scriptAddress inst) -- scriptAddress validator
    let theClient = client params
    dat <- SM.runStep theClient inputs slotRange
    -- traceM $ "AAAA " <> show (marloweContract dat) <> " ==> " <>  show (isFinal1 dat)
    return dat



rolePayoutScript :: Validator
rolePayoutScript = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]))
  where
    wrapped = Scripts.wrapValidator rolePayoutValidator


{-# INLINABLE rolePayoutValidator #-}
rolePayoutValidator :: (CurrencySymbol, TokenName) -> () -> ValidatorCtx -> Bool
rolePayoutValidator (currency, role) _ ctx =
    Val.valueOf (valueSpent (valCtxTxInfo ctx)) currency role P.> 0


marloweParams :: CurrencySymbol -> MarloweParams
marloweParams rolesCurrency = MarloweParams
    { rolesCurrency = rolesCurrency
    , rolePayoutValidatorHash = validatorHash rolePayoutScript }


defaultMarloweParams :: MarloweParams
defaultMarloweParams = marloweParams adaSymbol


{-# INLINABLE transition #-}
transition :: MarloweParams -> SM.State MarloweData -> [Input] -> SlotRange -> Maybe (TxConstraints Void Void, SM.State MarloweData)
transition params SM.State{ SM.stateData=MarloweData{..}, SM.stateValue=currentValue} inputs range = do
    let interval = case range of
            Interval (LowerBound (Finite l) True) (UpperBound (Finite h) True) -> (l, h)
            _ -> P.traceErrorH "Tx valid slot must have lower bound and upper bounds"
    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    -- traceM $ "Here6 " <> show computedResult
    -- traceM $ "Here6 " <> show txInput
    -- traceM $ "Here6" <> show marloweContract
    -- traceM $ "Here6" <> show marloweState
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let (deducedTxOutputs, finalBalance) = case txOutContract of
                    Close -> (txPaymentOuts txOutPayments, P.zero)
                    _ -> let
                        txWithPayouts = txPaymentOuts txOutPayments
                        totalPayouts = foldMap (\(Payment _ v) -> v) txOutPayments
                        finalBalance = totalIncome P.- totalPayouts
                        in (txWithPayouts, finalBalance)

            Just (deducedTxOutputs, SM.State marloweData finalBalance)
        Error txError -> Nothing

  where
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                    = P.zero

    totalIncome = foldMap collectDeposits inputs

    txPaymentOuts :: [Payment] -> TxConstraints i0 o0
    txPaymentOuts payments = foldMap paymentToTxOut paymentsByParty
      where
        paymentsByParty = Map.toList $ foldr collectPayments Map.empty payments

        paymentToTxOut (party, value) = case party of
            PK pk  -> mustPayToPubKey pk value
            Role role -> let
                dataValue = Datum $ PlutusTx.toData (rolesCurrency params, role)
                in mustPayToOtherScript (rolePayoutValidatorHash params) dataValue value

    collectPayments :: Payment -> Map Party Money -> Map Party Money
    collectPayments (Payment party money) payments = let
        newValue = money P.+ P.fromMaybe P.zero (Map.lookup party payments)
        in Map.insert party newValue payments


{-# INLINABLE isFinal #-}
isFinal :: MarloweData -> Bool
isFinal MarloweData{marloweContract=c} = c P.== Close


{-# INLINABLE mkValidator #-}
mkValidator :: MarloweParams -> Scripts.ValidatorType MarloweSym
mkValidator p = SM.mkValidator $ SM.mkStateMachine (transition p) isFinal

validatorCode :: MarloweParams -> PlutusTx.CompiledCode PLC.DefaultUni (Scripts.ValidatorType MarloweSym)
validatorCode params = $$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params

type MarloweSym = StateMachine MarloweData [Input]

scriptInstance :: MarloweParams -> Scripts.ScriptInstance MarloweSym
scriptInstance params = Scripts.validator @MarloweSym
    (validatorCode params)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MarloweData @[Input]

machineInstance :: MarloweParams -> SM.StateMachineInstance MarloweData [Input]
machineInstance params =
    SM.StateMachineInstance
    (SM.mkStateMachine (transition params) isFinal)
    (scriptInstance params)

client :: MarloweParams -> SM.StateMachineClient MarloweData [Input]
client p = SM.mkStateMachineClient (machineInstance p)

