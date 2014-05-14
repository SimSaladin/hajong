{-# LANGUAGE ConstraintKinds, NoImplicitPrelude, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
module Riichi where

import ClassyPrelude
import Control.Lens
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as Map
import System.Random.Shuffle

-- * Types

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

data Mentsu = Shuntsu { mentsuPai :: [Tile], mentsuOpen :: Bool }  -- straight
            | Koutsu  { mentsuPai :: [Tile], mentsuOpen :: Bool } -- triplet
            | Kantsu  { mentsuPai :: [Tile], mentsuOpen :: Bool } -- quadret
            | Jantou  { mentsuPai :: [Tile], mentsuOpen :: Bool } -- pair
            deriving (Show, Read, Eq)

data Shout = Pon | Kan | Chi | Ron
           deriving (Show, Read, Eq)

data Hand = Hand
          { _handConcealed :: [Tile]
          , _handOpen :: [Mentsu]
          , _handPick :: Maybe Tile
          , _handDiscards :: [(Tile, Maybe Player)] -- maybe shouted
          , _handRiichi :: Bool
          , _handFuriten :: Maybe Bool -- ^ Just (temporary?)
          } deriving (Show, Read, Eq)

data Game playerID = Game
                   { _gamePlayers :: Map Player playerID
                   , _gameState :: Maybe RiichiState -- maybe in running game
                   , _gamePoints :: Map Player Points
                   }

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
                  { _riichiPublic :: RiichiPublic
                  , _riichiHand :: Hand
                  } deriving (Show, Read)

type RiichiState = (RiichiSecret, RiichiPublic)

data Yaku = Yaku 
          { _yakuConcealed :: Int
          , _yakuOpen :: Maybe Int
          }

data TurnAction = TurnRiichi Tile
                | TurnDiscard Tile
                | TurnDraw Bool (Maybe Tile)
                | TurnAnkan Tile
                | TurnShouted Shout Player -- shout [by who]
                deriving (Show, Read)

data RoundEvent = RoundAction Player TurnAction
                | RoundTsumo Player
                | RoundRon Player [Player] -- From, who?
                | RoundDraw [Player] -- tenpai players

type GameMonad m = ( MonadReader RiichiPublic m
                   , MonadState RiichiSecret m
                   , MonadWriter RoundEvent m
                   , MonadError Text m
                   )

liftE :: GameMonad m => (a -> m b) -> Either Text a -> m b
liftE = either throwError

makeLenses ''Game
makeLenses ''Yaku
makeLenses ''RiichiSecret
makeLenses ''RiichiPublic
makeLenses ''RiichiPlayer
makeLenses ''Hand

-- | State visible to the player
riichiPlayer :: RiichiState -> Player -> Maybe RiichiPlayer
riichiPlayer (secret, public) player =
    secret ^. riichiHands . at player <&> RiichiPlayer public 

-- * Initialize

initHand :: [Tile] -> Hand
initHand tiles = Hand tiles [] Nothing [] False Nothing

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

defaultPlayers :: [Player]
defaultPlayers = [1..4]

-- * Game operations

-- | Advance the game to next round
nextRound :: RiichiState -> IO RiichiState
nextRound (_, public) = do
    secret <- newSecret
    return $ setSecret secret $ public & set riichiTurn (public ^. riichiDealer)

handOf' :: GameMonad m => Player -> m Hand
handOf' player = use (handOf player) >>= maybe (throwError "Player not found") return

handOf player = riichiHands.at player

-- | Apply an action on a player
runTurn :: GameMonad m => TurnAction -> m ()
runTurn action = do
    player <- view riichiTurn
    hand <- use (handOf player) >>= maybe (throwError "Hand of current player not found") return

    case action of
        TurnRiichi tile         -> liftE (handOf player ?=) $ setRiichi tile hand
        TurnDiscard tile        -> liftE (handOf player ?=) $ discard tile hand
        TurnDraw dead Nothing   -> undefined
        TurnDraw _ _            -> throwError "Draw action cannot specify the tile"
        TurnAnkan tile          -> undefined
        TurnShouted shout shouter -> undefined

    tell $ RoundAction player action -- here only if no error was thrown

-- ** Hand

-- | Discard a tile; returns Left err if discard is not possible due to the
-- tile not being in the hand, or due to riichi.
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

setRiichi :: Tile -> Hand -> Either Text Hand
setRiichi tile hand
    | hand ^. handRiichi = Left "Already in riichi"
    | tenpai hand        = Right $ set handRiichi True hand 
    | otherwise          = Left "Hand not tenpai"

tenpai :: Hand -> Bool
tenpai hand = True -- TODO implement

-- * Mentsu

-- | Get possible (distinct) mentsu combinations
handMentsu :: Hand -> [[Mentsu]]
handMentsu hand = undefined

-- * Yaku

getYaku :: RiichiPlayer -> [[Yaku]] -- list of plausible yaku combinations
getYaku _ = undefined

-- ** Hand-Tile-based

yakuPinfu :: Yaku
yakuPinfu = undefined

yakuTanyao :: Yaku
yakuTanyao = undefined

yakuIipeikou :: Yaku
yakuIipeikou = undefined

yakuFanpai :: Yaku
yakuFanpai = undefined

yakuSanshokuDoujin :: Yaku
yakuSanshokuDoujin = undefined

yakuIttsuu :: Yaku
yakuIttsuu = undefined

yakuChanta :: Yaku
yakuChanta = undefined

yakuHonroutou :: Yaku
yakuHonroutou = undefined

yakuToitoi :: Yaku
yakuToitoi = undefined

yakuSanankou :: Yaku
yakuSanankou = undefined

yakuSanKantsu :: Yaku
yakuSanKantsu = undefined

yakuSanshokuDoukou :: Yaku
yakuSanshokuDoukou = undefined

yakuChiitoitsu :: Yaku
yakuChiitoitsu = undefined

yakuShouSangen :: Yaku
yakuShouSangen = undefined

yakuHonitsu :: Yaku
yakuHonitsu = undefined

yakuJunchan :: Yaku
yakuJunchan = undefined

yakuRyanpeikou :: Yaku
yakuRyanpeikou = undefined

yakuChinitsu :: Yaku
yakuChinitsu = undefined

-- ** Other

yakuMenzenTsumo :: Yaku
yakuMenzenTsumo = undefined

yakuRiichi :: Yaku
yakuRiichi = undefined

yakuIppatsu :: Yaku
yakuIppatsu = undefined

yakuDoubleRiichi :: Yaku
yakuDoubleRiichi = undefined

yakuHouteiRaoyui :: Yaku
yakuHouteiRaoyui = undefined

yakuRinshanKaihou :: Yaku
yakuRinshanKaihou = undefined

yakuChankan :: Yaku
yakuChankan = undefined

yakuNagashiMangan :: Yaku
yakuNagashiMangan = undefined

-- * PrettyPrint
--
--     mm-mm-mm
--     mm-mm-mm                                    mm-mm-mm
--     mm-mm-mm    N       Player1                 mm-mm-mm-mm
--     mm-mm-mm-mm _ _ _ _ _ _ _ _ _ _ _ _ _       mm-mm-mm-mm
--                                                 mm-mm-mm
--  E        |    XX XX XX XX XX XX XX XX      XX  |
--           |          XX XX XX XX XX XX      XX  | W
--           |          XX XX XX XX XX XX      XX  |
--           |  XX XX XX                 XX XX XX  |  
--   Player3 |  XX XX XX  (NN)           XX XX XX  | Player4
--           |  XX XX XX                 XX XX XX  |  
--   (25000) |  XX XX XX Do Ra He Re ..  XX XX XX  | (25000)
--           |  XX XX XX                 XX XX XX  |  
--           |  XX XX XX                 XX XX XX  |  
--           |  XX XX XX                 XX XX XX  |  
--           |  XX      XX XX XX XX XX XX          |
--    mm-mm-mm  XX      XX XX XX XX XX XX          |
--    mm-mm-mm  XX      XX XX XX XX XX XX XX XX XX |
--    mm-mm-mm
-- mm-mm-mm-mm     _ _ _ _ _ _ _ _ _ _ _ _ _  mm-mm-mm-mm
--                 S Player3      (25000)     mm-mm-mm
--                                            mm-mm-mm
--                                            mm-mm-mm
--
--  Parts:  - mentsu (ankan, open)
--          - discard pile
--          - dead wall (NN)
--          - Dora indicators
--          - player avatars, points, seat winds



