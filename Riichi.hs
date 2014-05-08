{-# LANGUAGE TemplateHaskell #-}
module Riichi where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import Data.Maybe
import Control.Arrow
import qualified Data.Map as Map
import Data.List
import System.Random.Shuffle

data Number = Ii | Ryan | San | Suu | Wu | Rou | Chii | Paa | Chuu
            deriving (Show, Read, Eq, Enum)

data Sangenpai = Haku | Hatsu | Chun
               deriving (Show, Read, Eq, Enum)

data Kazehai = Ton | Nan | Shaa | Rei
             deriving (Show, Read, Eq, Enum)

data Tile = Man Number Bool
          | Pin Number Bool
          | Sou Number Bool
          | Sangen Sangenpai
          | Kaze Kazehai
          deriving (Show, Read, Eq)

riichiTiles :: [Tile]
riichiTiles = join . replicate 4 $ 
    concatMap (\suit -> map (`suit` False) [Ii .. Chuu]) [Man, Pin, Sou]
    ++ map Sangen [Haku .. Chun]
    ++ map Kaze [Ton .. Rei]

type Player = Int

type Points = Int

data Set = Shuntsu [Tile] -- straight
         | Koutsu [Tile] -- triplet
         | Kantsu [Tile] -- quadret

data Shout = Pon | Kan | Chi | Ron

data TurnAction = Discard Player Tile Bool -- ^ Riichi?
                | Ankan Tile

         -- Jantou [Tile] -- pair

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handOpen :: [Set]
          , _handPick :: Maybe Tile
          , _handDiscards :: [(Tile, Maybe Player)] -- maybe shouted
          , _handRiichi :: Maybe Int
          }

makeLenses ''Hand

initHand :: [Tile] -> Hand
initHand tiles = Hand tiles [] Nothing [] Nothing

discard :: Tile -> Hand -> Maybe Hand
discard tile hand = do

    guard $ hand ^. handRiichi . to isJust

    let (xs, ys) = break (== tile) (_handConcealed hand)
    case ys of
        []      -> Nothing
        (_:ys') -> Just $ hand &
            set handConcealed (xs ++ ys')
            . over handDiscards (++ [(tile, Nothing)])

data RiichiState = RiichiState
                 { _riichiWall :: [Tile]
                 , _riichiWanpai :: [Tile]
                 , _riichiDora :: [Tile]
                 , _riichiRound :: Kazehai
                 , _riichiDealer :: Player
                 , _riichiHands :: Map Player Hand
                 , _riichiPoints :: Map Player Points
                 , _riichiTurn :: Player
                 , _riichiEvents :: [Either Shout TurnAction]
                 }

makeLenses ''RiichiState

initRiichi :: RiichiState
initRiichi = RiichiState
    { _riichiRound = Ton
    , _riichiDealer = 1
    , _riichiPoints = Map.fromList $ zip [1..] (replicate 4 25000)
    }

nextRound :: RiichiState -> IO RiichiState
nextRound state = do
    tiles <- shuffleM riichiTiles
    let (hands            , xs)   = splitAt (13 * 4) tiles
        ((h1, h2), (h3, h4)) = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
        (dora : wanpai, wall) = splitAt 14 xs

    return $ state
        { _riichiWanpai = wanpai
        , _riichiDora = [dora]
        , _riichiHands = Map.fromList $ zip [1..] (map initHand [h1, h2, h3, h4])
        , _riichiWall = wall
        , _riichiTurn = _riichiDealer state
        }

actionApply :: RiichiState -> TurnAction -> RiichiState
actionApply state (Discard player tile riichi) =
    case state ^. riichiHands . at player of
        Nothing -> error "Invalid player"
        Just hand -> case discard tile hand of
             Nothing -> error "Tile not discardable"
             Just hand' -> riichiHands . at player ?~ hand' $ state


