{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Riichi where

import Data.Text (Text)
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

data Mentsu = Shuntsu [Tile] -- straight
            | Koutsu [Tile] -- triplet
            | Kantsu [Tile] -- quadret
            deriving (Show, Read, Eq)

data Shout = Pon | Kan | Chi | Ron
           deriving (Show, Read, Eq)

data TurnAction = Discard Player Tile Bool -- ^ Riichi?
                | Ankan Tile
                deriving (Show, Read)

         -- Jantou [Tile] -- pair

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handOpen :: [Mentsu]
          , _handPick :: Maybe Tile
          , _handDiscards :: [(Tile, Maybe Player)] -- maybe shouted
          , _handRiichi :: Bool
          , _handFuriten :: Maybe Bool -- ^ Just (temporary?)
          } deriving (Show, Read, Eq)

makeLenses ''Hand

initHand :: [Tile] -> Hand
initHand tiles = Hand tiles [] Nothing [] False Nothing

discard :: Tile -> Hand -> Either Text Hand
discard tile hand
    | hand ^. handPick == Just tile = Right $ hand & set handPick Nothing . setDiscard
    | hand ^. handRiichi            = Left "Cannot change wait in riichi"
    | otherwise                     = case ys of
        []      -> Left "Tile not in hand"
        (_:ys') -> Right $ hand & set handConcealed (xs ++ ys') . setDiscard
    where
        (xs, ys) = break (== tile) (_handConcealed hand)
        setDiscard = over handDiscards (++ [(tile, Nothing)])

--- game -------------------------------

data RiichiSecret = RiichiSecret
                 { _riichiWall :: [Tile]
                 , _riichiWanpai :: [Tile]
                 , _riichiHands :: Map Player Hand
                 } deriving (Show, Read)

data RiichiPublic = RiichiPublic
                 { _riichiDora :: [Tile]
                 , _riichiRound :: Kazehai
                 , _riichiDealer :: Player
                 , _riichiPoints :: Map Player Points
                 , _riichiTurn :: Player
                 , _riichiEvents :: [Either Shout TurnAction]
                 } deriving (Show, Read)

data RiichiPlayer = RiichiPlayer
                  { _riichiPlayer :: Player
                  , _riichiPublic :: RiichiPublic
                  , _riichiHand :: Hand
                  } deriving (Show, Read)

type RiichiState = (RiichiSecret, RiichiPublic)

makeLenses ''RiichiSecret
makeLenses ''RiichiPublic
makeLenses ''RiichiPlayer

newGame :: RiichiPublic
newGame = RiichiPublic
    { _riichiDora   = []
    , _riichiRound  = Ton
    , _riichiDealer = 1
    , _riichiPoints = Map.fromList $ zip [1..] (replicate 4 25000)
    , _riichiTurn   = 1
    , _riichiEvents = []
    }

newRiichiState :: IO RiichiState
newRiichiState = liftM (`setSecret` newGame) newSecret

nextRound :: RiichiState -> IO RiichiState
nextRound (_, public) = do
    secret <- newSecret
    return $ setSecret secret $ public & set riichiTurn (public ^. riichiDealer)

actionApply :: RiichiState -> TurnAction -> Either Text RiichiState
actionApply state (Discard player tile riichi) =
    maybe (Left "No such player") go (state ^. _1.riichiHands.at player)
    where
        go = fmap updateHand . discard tile . (if riichi then set handRiichi True else id)
        updateHand hand = state & _1.riichiHands.at player ?~ hand

getRiichiPlayer :: RiichiState -> Player -> Maybe RiichiPlayer
getRiichiPlayer (secret, public) player =
    secret ^. riichiHands . at player <&> RiichiPlayer player public 

defaultPlayers :: [Player]
defaultPlayers = [1..4]

-- * Internal

setSecret :: RiichiSecret -> RiichiPublic -> RiichiState
setSecret secret public =
    (secret & set riichiWanpai wanpai', public & set riichiDora [dora])
    where
        (dora : wanpai') = secret ^. riichiWanpai

newSecret :: IO RiichiSecret
newSecret = liftM dealTiles $ shuffleM riichiTiles
    where
        dealTiles tiles = RiichiSecret
            { _riichiWall = wall
            , _riichiWanpai = wanpai
            , _riichiHands = Map.fromList $ zip [1..] (map initHand [h1, h2, h3, h4])
            } where
                (hands, xs)             = splitAt (13 * 4) tiles
                ((h1, h2), (h3, h4))    = (splitAt 13 *** splitAt 13) $ splitAt (13*2) hands
                (wanpai, wall)          = splitAt 14 xs

