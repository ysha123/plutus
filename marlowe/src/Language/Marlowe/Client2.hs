{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
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

module Language.Marlowe.Client2 where
import           Control.Monad              (Monad (..), void)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Error.Lens   (throwing)
import           Data.Text.Prettyprint.Doc  hiding ((<>))
import           Control.Lens
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (maybeToList)
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Language.Plutus.Contract
import qualified Language.Plutus.Contract.Tx  as Tx
import           Language.Marlowe.Semantics hiding (Contract)
import qualified Language.Marlowe.Semantics as Marlowe
import qualified Language.PlutusTx          as PlutusTx
import qualified Language.PlutusTx.Prelude  as P
import           Ledger                     (SlotRange, TxInfo, CurrencySymbol, ValidatorCtx(..),TokenName, Address(..),Datum (..), PubKeyHash (..), pubKeyHash, Slot (..), Tx, TxOut (..), TxOutRef(..), TxOutTx (..), interval,

                                             mkValidatorScript, pubKeyHashTxOut, scriptAddress, scriptTxIn, scriptTxOut, validatorHash, txOutDatum, txOutTxDatum,
                                             txOutRefs, scriptTxOut', valueSpent, pubKeyAddress)
import           Ledger.Ada                 (adaSymbol, adaValueOf)
import           Ledger.AddressMap                 (outRefMap)
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


marloweContract2 :: forall e. (AsContractError e)
    => Contract MarloweSchema e ()
marloweContract2 = do
    create `select` apply {- <|> void sub -}
  where
    {- sub = do
        creator <- endpoint @"sub" @MarloweParams @MarloweSchema
        pk <- ownPubKey
        let validator = validatorScript creator
            address = Ledger.scriptAddress validator
        void $ utxoAt address
        utxoAt (pubKeyAddress pk) -}
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
createContract :: (AsContractError e)
    => MarloweParams
    -> Marlowe.Contract
    -> Contract MarloweSchema e ()
createContract params contract = do
    slot <- awaitSlot 0
    creator <- pubKeyHash <$> ownPubKey
    -- traceM $ "createContract: " <> show slot <> " " <> show creator
    let validator = validatorScript params
        vh = validatorHash validator
        inst = scriptInstance params

        marloweData = MarloweData {
            marloweContract = contract,
            marloweState = emptyState slot }
        ds = Datum $ PlutusTx.toData marloweData

    let payValue = adaValueOf 0

    let slotRange = interval slot (slot + 10)
    let tx = mustPayToOtherScript vh ds payValue
             <> mustValidateIn slotRange
    void $ submitTx tx

    -- void $ submitTx tx


applyInputs :: (AsContractError e)
    => MarloweParams
    -> [Input]
    -> Contract MarloweSchema e MarloweData
applyInputs params inputs = do
    let redeemer = mkRedeemer inputs
        validator = validatorScript params
        vh = validatorHash validator
        inst = scriptInstance params
        address = {- (Scripts.scriptAddress inst) -} scriptAddress validator
    traceM $ "Here " <> show address <> ", inst " <> show inst <> ", inst addr " <> show (Scripts.scriptAddress inst)
    slot <- awaitSlot 0
    traceM $ "Here1 " <> show slot
    pk <- ownPubKey
    traceM "Here2"
    void $ utxoAt (pubKeyAddress pk)
    traceM "Here3"
    utxo <- utxoAt address
    traceM $ "Here4 " <> show utxo
    let [(ref, out)] = Map.toList utxo


    let convert :: TxOutRef -> TxOutTx -> Maybe (TxOutRef, Val.Value, Datum, MarloweData)
        convert ref out = case (txOutTxDatum out) of
            Just dv -> case PlutusTx.fromData (getDatum dv) of
                Just marloweData -> Just (ref, txOutValue (txOutTxOut out), dv, marloweData)
                _ -> Nothing
            _ -> Nothing

    (ref, value, dataValue, MarloweData{..}) <- case convert ref out of
        Just r -> pure r
        Nothing -> throwing _OtherError $ (Text.pack $ "Not found")
    -- traceM "Here5"
    -- For now, we expect a transaction to happen whithin 10 slots from now.
    -- That's about 3 minutes, should be fine.
    let slotRange = interval slot (slot + Slot 10)
    let txInput = TransactionInput {
            txInterval = (slot, slot + Slot 10),
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    -- traceM $ "Here6 " <> show computedResult
    -- traceM $ "Here6 " <> show txInput
    -- traceM $ "Here6" <> show marloweContract
    -- traceM $ "Here6" <> show marloweState
    (tx, md) <- case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let marloweData = MarloweData {
                    -- marloweCreator,
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let deducedTxOutputs = case txOutContract of
                    Close -> txPaymentOuts txOutPayments
                    _ -> let
                        txWithPayouts = txPaymentOuts txOutPayments
                        totalPayouts = foldMap (\(Payment _ v) -> v) txOutPayments
                        finalBalance = totalIncome P.- totalPayouts
                        dataValue = Datum (PlutusTx.toData marloweData)
                        scriptOut = mustPayToTheScript marloweData finalBalance
                        in scriptOut <> txWithPayouts

            return (deducedTxOutputs, marloweData)
        Error txError -> throwing _OtherError $ (Text.pack $ show txError)
    -- traceM "Here7"
    let asdf = addTxIn ref inputs tx
    let finalTx = asdf <> {- mustSpendScriptOutput ref redeemer <>  -}mustValidateIn slotRange
    traceM ("AAAAAAA" <> show  finalTx)
    -- throwing _OtherError $ (Text.pack $ {- show finalTx -} "AAA" )
    void $ submitTxConstraintsSpending inst utxo finalTx
    pure md
  where
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                    = P.zero

    totalIncome = foldMap collectDeposits inputs

    isAddress address (TxOut{txOutAddress}, _) = txOutAddress == address

    -- txPaymentOuts :: [Payment] -> UnbalancedTx
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


{-| Generate a validator script for 'creator' PubKey -}
validatorScript :: MarloweParams -> Validator
validatorScript params = mkValidatorScript ($$(PlutusTx.compile [|| validatorParam ||])
    `PlutusTx.applyCode`
        PlutusTx.liftCode params)
  where
    validatorParam k = Scripts.wrapValidator (marloweValidator k)

data Marlowe
instance Scripts.ScriptType Marlowe where
    type instance RedeemerType Marlowe = [Input]
    type instance DatumType Marlowe = MarloweData


{- validator vc wrapper =
    let val = mkValidatorScript $ wrapper `applyCode` vc
        hsh = validatorHash val
        mps = mkMonetaryPolicy hsh
    in Validator
        { instanceScript  = val
        , instanceHash    = hsh
        , instanceMPS     = mps
        , instanceMPSHash = Ledger.Scripts.monetaryPolicyHash mps
        }
 -}
scriptInstance :: MarloweParams -> Scripts.ScriptInstance Marlowe
scriptInstance params = Scripts.validator @Marlowe
    ($$(PlutusTx.compile [|| marloweValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MarloweData @[Input]

{-| Make redeemer script -}
mkRedeemer :: [Input] -> Redeemer
mkRedeemer inputs = Redeemer (PlutusTx.toData inputs)
