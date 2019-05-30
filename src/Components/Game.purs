module Components.Game where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.String (toLower)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (CreateComponent, component, useReducer)
import React.Basic.Hooks as React

data GameChoice
  = Rock
  | Paper
  | Scissors

data GameResult
  = PlayerWins String
  | ComputerWins String
  | Tie

data Action
  = PlayerMove GameChoice
  | CpuMove GameChoice
  | Reset

type State =
  { player :: Maybe GameChoice
  , cpu :: Maybe GameChoice
  , gameResult :: String
  , score :: Int
  }

init :: State
init =
  { player: Nothing
  , cpu: Nothing
  , gameResult: "choose..!"
  , score: 0
  }

update :: Action -> State -> State
update Reset _ = init
update (PlayerMove player) state =
  updateScore $ state { player = Just player, cpu = Nothing }
update (CpuMove cpu) state =
  updateScore $ state { cpu = Just cpu }

updateScore :: State -> State
updateScore state =
  case determineWinner <$> state.player <*> state.cpu of
    Just (PlayerWins reason) ->
      state
        { gameResult = "Player wins!  " <> reason <> "!"
        , score = state.score + 1
        }
    Just (ComputerWins reason) ->
      state
        { gameResult = "Computer wins!  " <> reason <> "!"
        , score = state.score - 1
        }
    Just Tie ->
      state
        { gameResult = "It's a tie!"
        }
    _ ->
      state
        { gameResult = ""
        }
  where
    determineWinner player computer =
      let
        didPlayerWin = didLeftWin player computer
        didComputerWin = didLeftWin computer player
      in
        if fst didPlayerWin
          then PlayerWins (snd didPlayerWin)
          else if fst (didLeftWin computer player)
            then ComputerWins (snd didComputerWin)
            else Tie

    didLeftWin left right =
      case left, right of
        Rock, Scissors -> true /\ "Rock crushes Scissors"
        Paper, Rock -> true /\ "Paper covers Rock"
        Scissors, Paper -> true /\ "Scissors cuts Paper"
        _, _ -> false /\ "loss"

foreign import randomInt :: Int -> Int -> Effect Int

getRandomChoice :: Effect GameChoice
getRandomChoice = do
  i <- randomInt 1 5
  pure case i of
    1 -> Rock
    2 -> Paper
    _ -> Scissors

printMaybeChoice :: Maybe GameChoice -> String
printMaybeChoice = maybe "-- choosing --" printChoice

printChoice :: GameChoice -> String
printChoice Rock = "Rock"
printChoice Paper = "Paper"
printChoice Scissors = "Scissors"

choiceToClassName :: GameChoice -> String
choiceToClassName = toLower <<< printChoice

mkGame :: CreateComponent {}
mkGame = do
  component "Game" \_ -> React.do
    state /\ dispatch <- useReducer init (flip update)

    let
      move player = do
        dispatch $ PlayerMove player
        cpu <- getRandomChoice
        dispatch $ CpuMove cpu

    pure $ R.div_
      [ choiceButton move Rock
      , choiceButton move Paper
      , choiceButton move Scissors
      , divider
      , choiceRow
          [ showChoice state.player
          , showChoice state.cpu
          ]
      , showGameResult state.gameResult
      , showPlayerScore state.score
      , divider
      , resetButton $ dispatch Reset
      ]
  where
    choiceButton move gameChoice =
      R.button
        { onClick: capture_ do move gameChoice
        , className: "choice " <> choiceToClassName gameChoice
        , children:
            [ R.text $ printChoice gameChoice
            ]
        }

    resetButton reset =
      R.button
        { onClick: capture_ reset
        , className: "reset"
        , children: [ R.text "Reset!" ]
        }

    choiceRow children =
      R.div
        { className: "choice-row"
        , children
        }

    showChoice maybeGameChoice =
      R.div { className: "card " <> gameChoice }
      where
        gameChoice = maybe "choosing" choiceToClassName maybeGameChoice

    showGameResult gameResult =
      R.div_ [ R.text $ "Game result: " <> gameResult ]

    showPlayerScore playerScore =
      R.div_ [ R.text $ "Score: " <> show playerScore ]

    divider = R.hr {}
