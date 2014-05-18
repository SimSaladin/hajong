{-# LANGUAGE DeriveFunctor #-}
module Yaku where

import ClassyPrelude
import Tiles
import Control.Applicative
import Control.Monad.Free
import qualified Data.List as L

type CompleteHand = [Mentsu]

-- * Mentsu splitting

-- [ [ [ Mentsu ] ] ] -> [CompleteHand] : [[Mentsu]]
-- ^ ^ ^ possibility
-- | \ list of possible within group
-- \ groups

mentsuSearch :: [Tile] -> [CompleteHand]
mentsuSearch =
    mapMaybe isComplete . buildVariations . map getMentsu . groupBy compareSuit . sort

buildVariations :: [[[Mentsu]]] -> [CompleteHand]
buildVariations = go
    where go (x : xs) = x >>= (`map` go xs) . (++)
          go []       = []

-- | This gets only total mentsu: no orphan tiles are left in the result.
getMentsu :: [Tile] -> [[Mentsu]]
getMentsu tiles = map fst $ go ([], tiles)
    where 
        go :: ([Mentsu], [Tile]) -> [([Mentsu], [Tile])] -- (mentsu, leftovers)
        go (done, xs@(a:b:es)) = let

            takingJantou  = if a == b then go (Jantou [a,b] False : done, es) ++ takingKoutsu else []
            takingKoutsu  = case es of
                (c:es') | c == a      -> go (Koutsu [a,b,c] False : done, es') ++ takingKantsu
                        | otherwise   -> []
                _                     -> []
            takingKantsu  = case es of
                (c:d:es') | c == d    -> go (Kantsu [a,b,c,d] False : done, es')
                          | otherwise -> []
                _                     -> []
            takingShuntsu = case (tileSucc a, tileSucc a >>= tileSucc) of
                (Just r, Just s)
                    | tileSuited a && r `elem` xs && s `elem` xs -> 
                        go (Shuntsu [a,r,s] False : done, xs L.\\ [a,r,s])
                    | otherwise -> []
                _ -> []

            in takingJantou ++ takingShuntsu

        go (done, []) = [(done, [])]
        go (_,  _)    = [] -- branch cannot be complete with one orphan tile

isComplete :: [Mentsu] -> Maybe CompleteHand
isComplete xs = do
    guard $ (== 1) $ length $ filter isJantou xs
    return xs

-- | Documentation for 'isJantou'
isJantou :: Mentsu -> Bool
isJantou (Jantou{}) = True
isJantou _ = False

-- * Yaku

data YakuChecker next = YakuMentsu MentsuKind TileKind next -- tile kind about first tile. As optimization put these before tile-dependant checks
                      | YakuMentsu' MentsuKind TileKind (Tile -> next)
                      | YakuStateful (YakuInfo -> next)
                      | YakuHandConcealed next
                      | YakuHandOpen next
                      | YakuProperty () next -- todo: property of hand
                      | YakuMatches
                      | YakuNot
                      deriving (Functor)

type Yaku = Free YakuChecker

data YakuInfo = YakuInfo
              { yakuRoundKaze :: Kazehai
              , yakuPlayerKaze :: Kazehai
              }

data MentsuKind = MentsuJantou
                | MentsuAny      -- Note: NOT jantou
                | MentsuShuntsu
                | MentsuKoutsu
                | MentsuKantsu
                | MentsuKoutsuKantsu

type TileKind = (Bool, Tile) -> Bool

concealedHand :: Yaku ()
concealedHand = liftF $ YakuHandConcealed ()

openHand :: Yaku ()
openHand = liftF $ YakuHandOpen ()

yakuMatch :: Yaku ()
yakuMatch = liftF YakuMatches

anyKoutsu, anyKantsu, anyShuntsu, anyJantou, anyMentsu, anyKoutsuKantsu :: TileKind -> Yaku ()
anyKoutsu  tkind = liftF $ YakuMentsu MentsuKoutsu  tkind ()
anyShuntsu tkind = liftF $ YakuMentsu MentsuShuntsu tkind ()
anyKantsu  tkind = liftF $ YakuMentsu MentsuKantsu  tkind ()
anyJantou  tkind = liftF $ YakuMentsu MentsuJantou  tkind ()
anyMentsu  tkind = liftF $ YakuMentsu MentsuAny     tkind ()
anyKoutsuKantsu  tkind = liftF $ YakuMentsu MentsuKoutsuKantsu tkind ()

anyShuntsu' :: TileKind -> Yaku Tile
anyShuntsu' tkind = liftF $ YakuMentsu' MentsuShuntsu tkind id

anyMentsu' :: TileKind -> Yaku Tile
anyMentsu' tkind = liftF $ YakuMentsu' MentsuAny tkind id

anyKoutsuKantsu' :: TileKind -> Yaku Tile
anyKoutsuKantsu' tkind = liftF $ YakuMentsu' MentsuKoutsuKantsu tkind id

yakuState :: Yaku YakuInfo
yakuState = liftF (YakuStateful id)

terminal, honor, sangenpai, suited, anyTile :: TileKind
terminal  (_, tile      ) = tileSuited tile && liftA2 (||) (== Ii) (== Chuu) (tileNumber tile)
honor     (_, Kaze _    ) = True
honor     (_, Sangen _  ) = True
honor     (_, _         ) = False
sangenpai (_, Sangen _  ) = True
sangenpai (_, _         ) = False
suited    (_, Kaze _    ) = False
suited    (_, Sangen _  ) = False
suited    (_, _         ) = True
anyTile   (_, _         ) = True
concealed (open, _      ) = not open

sameTile :: Tile -> TileKind
sameTile  this (_, that) = this == that

sameNumber :: Tile -> TileKind
sameNumber  this (_, that) = tileNumber this == tileNumber that

ofNumber :: Number -> TileKind
ofNumber n (_, that) = tileNumber that == n

sameSuit :: Tile -> TileKind
sameSuit this (_, that) = compareSuit this that

(&.) :: TileKind -> TileKind -> TileKind
a &. b = \t -> a t && b t
infixl 1 &.

(|.) :: TileKind -> TileKind -> TileKind
a |. b = \t -> a t || b t
infixl 1 |.

allMentsuOfKind :: TileKind -> Yaku ()
allMentsuOfKind tkind = do
    replicateM_ 4 $ anyMentsu tkind
    anyJantou tkind

-- ** Shuntsu

yakuPinfu :: Yaku Int
yakuPinfu = do
    concealedHand
    replicateM_ 4 (anyShuntsu suited)
    anyJantou suited
    return 1

yakuIipeikou :: Yaku Int
yakuIipeikou = do
    concealedHand
    tile <- anyShuntsu' anyTile
    anyShuntsu (sameTile tile)
    return 1

yakuRyanpeikou :: Yaku Int
yakuRyanpeikou = do
    concealedHand
    yakuIipeikou
    yakuIipeikou
    return 3

yakuSanshokuDoujin :: Yaku Int
yakuSanshokuDoujin = do
    tile  <- anyShuntsu' anyTile
    tile' <- anyShuntsu' (f tile)
    anyShuntsu (f tile' &. f tile)

    -- TODO degrade -1 when open
    return 2
    where
        f tile = sameNumber tile &. not.sameSuit tile

yakuIttsuu :: Yaku Int
yakuIttsuu = do
    tile <- anyShuntsu' (ofNumber Ii)
    anyShuntsu (sameSuit tile &. ofNumber Suu)
    anyShuntsu (sameSuit tile &. ofNumber Chii)
    -- TODO degrade -1 when open
    return 2

-- ** Koutsu or Kantsu

-- NOTE this does not combine with chanta
yakuHonroutou :: Yaku Int
yakuHonroutou = do
    replicateM_ 4 $ anyKoutsuKantsu (terminal |. honor) 
    anyJantou (terminal |. honor)
    return 2

yakuToitoi :: Yaku Int
yakuToitoi = do
    replicateM_ 4 $ anyKoutsuKantsu anyTile
    return 2

yakuSanankou :: Yaku Int
yakuSanankou = do
    replicateM_ 3 $ anyKoutsuKantsu concealed
    return 2

yakuSanKantsu :: Yaku Int
yakuSanKantsu = do
    replicateM_ 3 $ anyKantsu anyTile
    return 2

yakuSanshokuDoukou :: Yaku Int
yakuSanshokuDoukou = do
    tile <- anyKoutsuKantsu' anyTile
    replicateM_ 2 $ anyKoutsuKantsu (sameNumber tile)
    return 2

yakuShouSangen :: Yaku Int
yakuShouSangen = do
    anyKoutsuKantsu sangen
    anyKoutsuKantsu sangen
    anyJantou sangen
    anyMentsu (not . sangen)
    return 2

-- ** Tile kind based

yakuFanpai :: Yaku Int
yakuFanpai = do
    info <- yakuState
    let roundTile = Kaze $ yakuRoundKaze info
        playerKaze = Kaze $ yakuPlayerKaze info
    tile <- anyMentsu' (sangenpai |. sameTile roundTile |. sameTile playerKaze)
    return $ if roundTile == playerKaze && roundTile == tile
        then 2
        else 1

yakuTanyao :: Yaku Int
yakuTanyao = do
    concealedHand
    allMentsuOfKind suited
    return 1

yakuKuitan :: Yaku Int
yakuKuitan = do
    openHand
    allMentsuOfKind suited
    return 1

yakuChanta :: Yaku Int
yakuChanta = do
    allMentsuOfKind (terminal |. honor) -- TODO this does not notice 7-8-9 Shuntsu!
    return 2

yakuHonitsu :: Yaku Int
yakuHonitsu = do
    anyMentsuJantou honor
    anyMentsu
    return 3 -- TODO degrades -1 when hand open

yakuJunchan :: Yaku Int
yakuJunchan = do
    allMentsuOfKind terminal -- TODO this does not notice 7-8-9 shuntsu
    return 3 -- TODO degrades -1 when open

yakuChinitsu :: Yaku Int
yakuChinitsu = do
    tile <- anyMentsu suited
    replicateM_ 3 (anyMentsu $ sameSuit tile)
    anyJantou (sameSuit tile)
    return 6 -- TODO degrades -1 when open

-- ** Special

yakuChiitoitsu :: Yaku ()
yakuChiitoitsu = undefined

-- ** Unrelated to mentsu

yakuMenzenTsumo :: Yaku ()
yakuMenzenTsumo = undefined

yakuRiichi :: Yaku ()
yakuRiichi = undefined

yakuIppatsu :: Yaku ()
yakuIppatsu = undefined

yakuDoubleRiichi :: Yaku ()
yakuDoubleRiichi = undefined

yakuHouteiRaoyui :: Yaku ()
yakuHouteiRaoyui = undefined

yakuRinshanKaihou :: Yaku ()
yakuRinshanKaihou = undefined

yakuChankan :: Yaku ()
yakuChankan = undefined

yakuNagashiMangan :: Yaku ()
yakuNagashiMangan = undefined
