-- TODO: rename modules from BlocklyEditor -> MarloweBlocklyEditor
module BlocklyEditor.State where

import Prelude
import BlocklyComponent.Types as Blockly
import BlocklyEditor.Types (Action(..), BottomPanelView, State, _bottomPanelState, _errorMessage, _hasHoles, _marloweCode)
import BottomPanel.State (handleAction) as BottomPanel
import BottomPanel.Types (Action(..), State) as BottomPanel
import CloseAnalysis (analyseClose)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk)
import Data.Either (Either(..), either, hush, note)
import Data.Lens (assign, modifying, over, set)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Env (Env)
import Examples.Marlowe.Contracts (example) as ME
import Halogen (HalogenM, modify_, query)
import Halogen as H
import Halogen.Extra (mapSubmodule)
import MainFrame.Types (ChildSlots, _blocklySlot)
import Marlowe.Blockly (blockToContract)
import Marlowe.Extended as EM
import Marlowe.Holes as Holes
import Marlowe.Linter as Linter
import Marlowe.Parser as Parser
import SessionStorage as SessionStorage
import SimulationPage.Types (_templateContent)
import StaticAnalysis.Reachability (analyseReachability)
import StaticAnalysis.StaticTools (analyseContract)
import StaticAnalysis.Types (AnalysisExecutionState(..), _analysisExecutionState, _analysisState)
import StaticData (marloweBufferLocalStorageKey)
import Text.Pretty (pretty)

toBottomPanel ::
  forall m a.
  Functor m =>
  HalogenM (BottomPanel.State BottomPanelView) (BottomPanel.Action BottomPanelView Action) ChildSlots Void m a ->
  HalogenM State Action ChildSlots Void m a
toBottomPanel = mapSubmodule _bottomPanelState BottomPanelAction

handleAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  Action ->
  HalogenM State Action ChildSlots Void m Unit
handleAction Init = do
  mContents <- liftEffect $ SessionStorage.getItem marloweBufferLocalStorageKey
  handleAction $ InitBlocklyProject $ fromMaybe ME.example mContents

handleAction (HandleBlocklyMessage Blockly.CodeChange) = do
  eContract <-
    runExceptT do
      block <- ExceptT <<< map (note "Blockly Workspace is empty") $ query _blocklySlot unit $ H.request Blockly.GetBlockRepresentation
      except $ blockToContract block
  case eContract of
    Left e ->
      modify_
        ( set _errorMessage (Just $ unexpected e)
            <<< set _marloweCode Nothing
        )
    Right holesContract -> do
      let
        hasHoles = Linter.hasHoles $ Linter.lint Nil holesContract

        prettyContract = show $ pretty holesContract

        mExtendedContract :: Maybe EM.Contract
        mExtendedContract = Holes.fromTerm holesContract

        maybeUpdateTemplateContent =
          maybe
            identity
            (EM.updateTemplateContent <<< EM.getPlaceholderIds)
            mExtendedContract
      liftEffect $ SessionStorage.setItem marloweBufferLocalStorageKey prettyContract
      modify_
        ( set _errorMessage Nothing
            <<< set _marloweCode (Just $ prettyContract)
            <<< set _hasHoles hasHoles
            <<< over (_analysisState <<< _templateContent) maybeUpdateTemplateContent
        )
  where
  unexpected s = "An unexpected error has occurred, please raise a support issue at https://github.com/input-output-hk/plutus/issues/new: " <> s

handleAction (InitBlocklyProject code) = do
  void $ query _blocklySlot unit $ H.tell (Blockly.SetCode code)
  liftEffect $ SessionStorage.setItem marloweBufferLocalStorageKey code
  let
    hasHoles = either (const false) identity $ (Linter.hasHoles <<< Linter.lint Nil) <$> Parser.parseContract code
  modify_
    ( set _marloweCode (Just code)
        <<< set _hasHoles hasHoles
    )

handleAction SendToSimulator = pure unit

handleAction ViewAsMarlowe = pure unit

handleAction Save = pure unit

handleAction (BottomPanelAction (BottomPanel.PanelAction action)) = handleAction action

handleAction (BottomPanelAction action) = do
  toBottomPanel (BottomPanel.handleAction action)
  void $ query _blocklySlot unit $ H.tell Blockly.Resize

handleAction (SetIntegerTemplateParam templateType key value) =
  modifying
    (_analysisState <<< _templateContent <<< EM.typeToLens templateType)
    (Map.insert key value)

handleAction AnalyseContract = runAnalysis $ analyseContract

handleAction AnalyseReachabilityContract = runAnalysis $ analyseReachability

handleAction AnalyseContractForCloseRefund = runAnalysis $ analyseClose

handleAction ClearAnalysisResults = assign (_analysisState <<< _analysisExecutionState) NoneAsked

runAnalysis ::
  forall m.
  MonadAff m =>
  (EM.Contract -> HalogenM State Action ChildSlots Void m Unit) ->
  HalogenM State Action ChildSlots Void m Unit
runAnalysis doAnalyze =
  void
    $ runMaybeT do
        block <- MaybeT $ query _blocklySlot unit $ H.request Blockly.GetBlockRepresentation
        -- FIXME: See if we can use runExceptT and show the error somewhere
        contract <- MaybeT $ pure $ Holes.fromTerm =<< (hush $ blockToContract block)
        lift $ doAnalyze contract

editorGetValue :: forall state action msg m. HalogenM state action ChildSlots msg m (Maybe String)
editorGetValue =
  runMaybeT do
    block <- MaybeT $ query _blocklySlot unit $ H.request Blockly.GetBlockRepresentation
    contract <- hoistMaybe $ hush $ blockToContract block
    pure $ show $ pretty $ contract
