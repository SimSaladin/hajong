{-# LANGUAGE DeriveFunctor #-}
module Yaku where

import ClassyPrelude
import Tiles
import Control.Lens
import Control.Monad.Free
import Control.Monad.State
import qualified Data.List as L

-- * Mentsu and complete hands

type Mentsus = [Mentsu]
type CompleteHand = [Mentsu]

handComplete :: Mentsus -> [Tile] -> Bool
handComplete mentsu = not . null .
    mapMaybe (isComplete . (mentsu ++)) . mentsuSearch

mentsuSearch :: [Tile] -> [Mentsus]
mentsuSearch =
    buildVariations . map getMentsu .
    groupBy compareSuit . sort

buildVariations :: [[Mentsus]] -> [Mentsus]
buildVariations = go
    where go (x : xs) = x >>= (`map` go xs) . (++)
          go []       = []

-- | This gets only total mentsu: no orphan tiles are left in the result.
getMentsu :: [Tile] -> [Mentsus]
getMentsu tiles = map fst $ go ([], tiles)
    where 
        go :: ([Mentsu], [Tile]) -> [([Mentsu], [Tile])] -- (mentsu, leftovers)
        go (done, xs@(a:b:es)) = let

            takingJantou  = if a == b then go (Jantou [a,b] Nothing : done, es) ++ takingKoutsu else []
            takingKoutsu  = case es of
                (c:es') | c == a      -> go (Koutsu [a,b,c] Nothing : done, es') ++ takingKantsu
                        | otherwise   -> []
                _                     -> []
            takingKantsu  = case es of
                (c:d:es') | c == d    -> go (Kantsu [a,b,c,d] Nothing : done, es')
                          | otherwise -> []
                _                     -> []
            takingShuntsu = case shuntsuOf a xs of
                Just ment   -> go (ment : done, xs L.\\ mentsuPai ment)
                Nothing     -> []
            in takingJantou ++ takingShuntsu

        go (done, []) = [(done, [])]
        go (_,  _)    = [] -- branch cannot be complete with one orphan tile

shuntsuOf :: Tile -> [Tile] -> Maybe Mentsu
shuntsuOf a xs = do
    r <- tileSucc a
    s <- tileSucc r
    guard (tileSuited a && r `elem` xs && s `elem` xs)
    return $ Shuntsu [a,r,s] Nothing

isComplete :: Mentsus -> Maybe CompleteHand
isComplete xs = do
    guard $ length xs == 5
    guard $ length (filter isJantou xs) == 1
    return xs

-- ** Checks

isShuntsu' :: [Tile] -> Bool
isShuntsu' xs = case sort xs of
    (x:y:z:[]) -> tileSuited x && Just y == tileSucc x && Just z == tileSucc y
    _ -> False

-- | Documentation for 'isJantou'
isJantou, isShuntsu, isKoutsu, isKantsu :: Mentsu -> Bool
isJantou x
    | Jantou{} <- x = True
    | otherwise    = False
isShuntsu x
    | Shuntsu{} <- x = True
    | otherwise     = False
isKoutsu x
    | Koutsu{} <- x = True
    | otherwise    = False
isKantsu x
    | Kantsu{} <- x = True
    | otherwise    = False


-- * Yaku

type Yaku = Free YakuChecker

data YakuChecker next = YakuMentsu MentsuProp next
                      -- ^ Require a simple mentsu property. Requiring this
                      -- first could allow for simple optimization by
                      -- removing some repeated checking.
                      | YakuMentsu' MentsuProp (Tile -> next)
                      -- ^ Require a mentsu property, but allow upcoming
                      -- properties depend on the matched tile.
                      | YakuStateful (YakuInfo -> next)
                      -- ^ Depend on game state.
                      | YakuHandConcealedDegrades next
                      | YakuHandConcealed next
                      | YakuHandOpen next
                      deriving (Functor)

data YakuInfo = YakuInfo
              { yakuRoundKaze :: Kazehai
              , yakuPlayerKaze :: Kazehai
              , yakuIsConcealed :: Bool
              }

calculateYaku :: CompleteHand -> [Yaku Int] -- XXX: Well, maybe not so stupid return type
calculateYaku hand = undefined

runChecker :: YakuInfo -> CompleteHand -> Yaku Int -> Maybe Int
runChecker yi hand = fmap fst . (`runStateT` hand) . iterM f
    where
        f :: YakuChecker (StateT [Mentsu] Maybe Int) -> StateT [Mentsu] Maybe Int
        f (YakuMentsu  mp s)            = get >>= lift . findMatch mp >>  putRes >>  s
        f (YakuMentsu' mp f)            = get >>= lift . findMatch mp >>= putRes >>= f
        f (YakuStateful f)              = f yi
        f (YakuHandConcealedDegrades s) = if yakuIsConcealed yi then s else (\x -> x - 1) <$> s
        f (YakuHandConcealed s)         = if yakuIsConcealed yi then s else lift Nothing
        f (YakuHandOpen s)              = if yakuIsConcealed yi then lift Nothing else s

        putRes (xs, t) = put xs >> return t

-- | Find a match in a list of mentsu. Returns the matches identifier tile and leftovers.
findMatch :: MentsuProp -> [Mentsu] -> Maybe ([Mentsu], Tile)
findMatch _  []   = Nothing
findMatch mp (x:xs)
    | matchProp mp x = Just (xs, unsafeHead $ mentsuPai x)
    | otherwise      = (_1 %~ (x:)) <$> findMatch mp xs

-- * Defining yaku

-- ** Yaku primitives

-- | Value degrades by one if open.
concealedHandDegrade :: Yaku ()
concealedHandDegrade = liftF $ YakuHandConcealedDegrades ()

-- | Must be concealed
concealedHand :: Yaku ()
concealedHand = liftF $ YakuHandConcealed ()

-- | Must be open
openHand :: Yaku ()
openHand = liftF $ YakuHandOpen ()

-- | Yaku that depend on game info. See "YakuInfo".
yakuState :: Yaku YakuInfo
yakuState = liftF (YakuStateful id)

-- | Require any mentsu with a property.
anyKoutsu, anyKantsu, anyShuntsu, anyJantou, anyMentsu, anyKoutsuKantsu, anyMentsuJantou :: MentsuProp -> Yaku ()
anyMentsu        tkind = liftF $ YakuMentsu tkind ()
anyKoutsu        tkind = liftF $ YakuMentsu (MentsuKoutsu       &. tkind) ()
anyShuntsu       tkind = liftF $ YakuMentsu (MentsuShuntsu      &. tkind) ()
anyKantsu        tkind = liftF $ YakuMentsu (MentsuKantsu       &. tkind) ()
anyJantou        tkind = liftF $ YakuMentsu (MentsuJantou       &. tkind) ()
anyKoutsuKantsu  tkind = liftF $ YakuMentsu (MentsuKoutsuKantsu &. tkind) ()
anyMentsuJantou  tkind = liftF $ YakuMentsu (MentsuAnyJantou    &. tkind) ()

-- | Require any mentsu with a property. Rest of the definition may depend
-- on the matched tile.
anyShuntsu', anyKoutsuKantsu', anyMentsu', anyMentsuJantou' :: MentsuProp -> Yaku Tile
anyMentsu'       tkind = liftF $ YakuMentsu' tkind id
anyKoutsuKantsu' tkind = liftF $ YakuMentsu' (MentsuKoutsuKantsu &. tkind) id
anyShuntsu'      tkind = liftF $ YakuMentsu' (MentsuShuntsu      &. tkind) id
anyMentsuJantou' tkind = liftF $ YakuMentsu' (MentsuAnyJantou    &. tkind) id

-- *** Helpers

-- | Simple yaku helper to require some same property from the four mentsu
-- and any pair.
allMentsuOfKind :: MentsuProp -> Yaku ()
allMentsuOfKind tkind = do
    replicateM_ 4 $ anyMentsu tkind
    anyJantou tkind

-- ** Mentsu properties

data MentsuProp = TileTerminal
                | TileSameAs Tile
                | TileSuited
                | TileSameSuit Tile
                | TileSameNumber Tile
                | TileNumber Number
                | TileHonor
                | TileSangenpai
                | TileAnd MentsuProp MentsuProp -- ^ &&
                | TileOr MentsuProp MentsuProp -- ^ ||
                | TileNot MentsuProp -- ^ not
                | TileConcealed
                | MentsuJantou
                | MentsuAnyJantou
                | MentsuShuntsu
                | MentsuKoutsu
                | MentsuKantsu
                | MentsuKoutsuKantsu
                | PropAny -- ^ Match anything

-- | Binary combinations of mentsu porperties.
(&.), (|.) :: MentsuProp -> MentsuProp -> MentsuProp
(&.) = TileAnd
(|.) = TileOr
infixl 1 &., |.

-- | Match a property on a mentsu.
matchProp :: MentsuProp -> Mentsu -> Bool
matchProp tt mentsu
    | (first:_) <- mentsuPai mentsu = case tt of
        MentsuJantou       | isJantou mentsu       -> True
        MentsuAnyJantou    | not $ isJantou mentsu -> True
        MentsuShuntsu      | isShuntsu mentsu      -> True
        MentsuKoutsu       | isKoutsu mentsu       -> True
        MentsuKantsu       | isKantsu mentsu       -> True
        MentsuKoutsuKantsu | isKantsu mentsu || isKoutsu mentsu -> True
        -- XXX: this is incomplete (shuntsu + terminals etc.)
        TileTerminal        -> tileTerminal first
        TileSameAs tile     -> first == tile
        TileSuited          -> tileSuited first
        TileSameSuit tile   -> compareSuit tile first
        TileSameNumber tile -> tileNumber tile      == tileNumber first
        TileNumber n        -> tileNumber first == n
        TileHonor           -> not $ tileSuited first
        TileSangenpai       -> tileSangenpai first
        TileAnd x y         -> matchProp x mentsu && matchProp y mentsu
        TileOr x y          -> matchProp x mentsu || matchProp y mentsu
        TileNot x           -> not $ matchProp x mentsu
        TileConcealed       -> isNothing $ mentsuOpen mentsu
        PropAny             -> True
        _ -> True
    | otherwise = error "ofTileType: empty mentsu"

-- | Tile kinds.
terminal, honor, sangenpai, suited, anyTile, concealed :: MentsuProp
terminal  = TileTerminal
honor     = TileHonor
sangenpai = TileSangenpai
suited    = TileSuited
anyTile   = PropAny
concealed = TileConcealed

sameTile, sameNumber, sameSuit :: Tile -> MentsuProp
sameTile = TileSameAs
sameNumber = TileSameNumber
sameSuit = TileSameSuit

ofNumber :: Number -> MentsuProp
ofNumber = TileNumber

-- | Negation of a MentsuProp.
propNot :: MentsuProp -> MentsuProp
propNot = TileNot

-- * Default yaku

-- ** 4 mentsu + 1 jantou

-- *** Shuntsu based

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
yakuRyanpeikou = concealedHand >> yakuIipeikou >> yakuIipeikou >> return 3

yakuSanshokuDoujin :: Yaku Int
yakuSanshokuDoujin = do
    concealedHandDegrade
    tile  <- anyShuntsu' anyTile
    tile' <- anyShuntsu' (f tile)
    anyShuntsu (f tile' &. f tile)
    return 2
    where
        f tile = sameNumber tile &. propNot (sameSuit tile)

yakuIttsuu :: Yaku Int
yakuIttsuu = do
    concealedHandDegrade
    tile <- anyShuntsu' (ofNumber Ii)
    anyShuntsu (sameSuit tile &. ofNumber Suu)
    anyShuntsu (sameSuit tile &. ofNumber Chii)
    return 2

-- *** Koutsu/kantsu based

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
    anyKoutsuKantsu sangenpai
    anyKoutsuKantsu sangenpai
    anyJantou sangenpai
    anyMentsu (propNot sangenpai)
    return 2

-- *** Tile kind based

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
yakuChanta = do -- TODO this does not notice 7-8-9 Shuntsu!
    anyShuntsu terminal
    replicateM_ 4 $ anyMentsuJantou (terminal |. honor)
    return 2

yakuHonitsu :: Yaku Int
yakuHonitsu = do
    concealedHandDegrade
    anyMentsuJantou honor
    tile <- anyMentsuJantou' suited
    replicateM_ 3 $ anyMentsuJantou (honor |. sameSuit tile)
    return 3

yakuJunchan :: Yaku Int
yakuJunchan = do
    concealedHandDegrade
    allMentsuOfKind terminal -- TODO this does not notice 7-8-9 shuntsu
    return 3

yakuChinitsu :: Yaku Int
yakuChinitsu = do
    concealedHandDegrade
    tile <- anyMentsu' suited
    replicateM_ 3 (anyMentsu $ sameSuit tile)
    anyJantou (sameSuit tile)
    return 6

-- ** Special

yakuChiitoitsu :: Yaku ()
yakuChiitoitsu = undefined -- TODO how does this implement?

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
