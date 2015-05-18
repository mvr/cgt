---
title: A Whirlwind Tour of Combinatorial Games in Haskell
published: March 17, 2014
---

Combinatorial games are an interesting class of games where two
players take turns to make a move, changing the game from one position
to another. In these games, both players have perfect information
about the state of the game and there is no element of chance. In
'normal play', the winner is declared when the other player is unable
to move. A lot of famous strategy games can be analysed as
combinatorial games: chess, go, tic-tac-toe.

The simplest way of thinking of these games is as a set of moves for
the player Left, and a set of moves for the player Right. When a
player chooses an option from their set, this new position can be
considered another game. This gives us a tree-like structure:

> import Prelude hiding ((||))
> import qualified Prelude ((||))
> import Control.Monad
> import Data.List
> import Data.Maybe
> import Data.Ratio
> import Data.Bits
> import Data.Function
>
> data Game = Game { leftMoves :: [Game], rightMoves :: [Game] }

We write { L | R } for the game where Left can choose a move from L,
and Right can choose a move from R. For example, we have the zero
game, where neither player has any moves they can make: zero = { | }

> zero = Game [] []

In this game, if it's Left's turn, he loses. If it's Right's turn, he
loses. So this game encompases the idea of both players having 0 turns
remaining.

Here's the next simplest game, one = { zero | }

> one = Game [zero] []

Now, if it's Left's turn, he can move to the zero game. If it's
Right's turn, he loses straight away. In this game, Left has one
move's advantage on Right. Similarly, we can define two:

> two = Game [one] []

Now Left has a two move advantage on Right. If we want to classify the
different outcomes a game can have, we find there are only four
options:

1. Left can always win, no matter who starts
2. Right can always win, no matter who starts
3. The second player can always win
4. The first player can always win

We will write these four options as G > 0, G < 0, G = 0, G || 0. It's
clear that zero = 0 and one > 0. We can combine these classes as
usual. For example, G >= 0 means Left can always win if he's the
second player, and in G <= 0 Right can always win if he's the second
player.

These two are easy to implement. If G >= 0, Left can always win as
second player and Right has no good opening move. A good opening move
for Right is a position in R that right could win, i.e. a position
R <= 0. The definition is similar for G <= 0, giving mutually recursive
definitions:

> gteqZero :: Game -> Bool
> lteqZero :: Game -> Bool
> gteqZero = not . any lteqZero . rightMoves
> lteqZero = not . any gteqZero . leftMoves

>-- gteqZero zero ==> True
>-- lteqZero zero ==> True
>-- gteqZero one  ==> True
>-- lteqZero one  ==> False

These are guaranteed to terminate, as at every step we are looking at
a smaller game. From these we can easily build the others:

> eqZero g = gteqZero g && lteqZero g
> gtZero g = gteqZero g && not (lteqZero g)
> ltZero g = not (gteqZero g) && lteqZero g
>
> fuzzyZero g = not (gteqZero g) && not (lteqZero g)

The last case is strange. We have a game G where neither G >= 0 nor
G <= 0! This corresponds to G || 0 from above, and we say G is fuzzy to
0. We can find such a game easily: ∗ = { zero | zero }. This game
clearly does not correspond to a number.

> star = Game [zero] [zero]
>
>-- fuzzyZero star ==> True

Now we consider the sum of two games. In the game G + H, a player has
the choice of which component they wish to move in. For example, Left
can choose one of the L moves in G, leaving H the same, or one of the
L moves in H, leaving G the same.

> instance Num Game where
>   g + h = Game left right
>     where left  = map (+ h) (leftMoves g)  ++ map (g +) (leftMoves h)
>           right = map (+ h) (rightMoves g) ++ map (g +) (rightMoves h)

We are also ready to negate games. In -G, Right can make all the moves
Left could, and vice versa. This is just like spinning the board in
chess.

>   negate g = Game (map negate $ rightMoves g) (map negate $ leftMoves g)

With these operations, games form an abelian group. Following the
pattern earlier, converting from integers to games is easy. We have
n = { n-1 | }. If we are given a negative number, we can just negate the
positive game.

>   fromInteger i | i == 0 = zero
>                 | i > 0  = Game [fromInteger (i-1)] []
>                 | i < 0  = negate $ fromInteger (-i)
>
>   g * h = undefined -- This is possible to define for some games
>   abs g = undefined
>   signum g = undefined

This arithmetic now lets us define equality in a natural way. Two
games are equal if their difference is 0.

> instance Eq Game where
>   g == h = eqZero (g - h)

As we would hope, G = G for every game. Let's think about why this
is. G = G is equivalent to G - G = 0 or G + (-G) = 0. If the first
player makes a move in G, the second player can immediately reply with
the same move in -G. No matter what move the first player makes, the
second has a response in the other component. This can continue until
the first player runs out of options and loses, meaning G + (-G) = 0.

We're ready to show 1 + 1 = 2:

>-- one + one == two ==> True
>-- zero + one == one ==> True
>-- two + two == two ==> False

We see arithmetic behaves as you expect. What about ∗ from earlier?

>-- star + star == zero ==> True

This seems strange at first, but makes sense when we think about how
∗ + ∗ would be played. The first player plays in a ∗, moving it to
0. The other player is then free to play in the other ∗, leaving
0 + 0 = 0, so the first player loses. The first player losing is how
we defined G = 0, so indeed we get ∗ + ∗ = 0.

We can also compare games with each other. We have to cheat here
because Ord is intended for total orders, but from ∗ || 0 it is clear
we can't always put an order on two games.

> instance Ord Game where
>   g < h = ltZero (g - h)
>   g > h = gtZero (g - h)
>   g <= h = lteqZero (g - h)
>   g >= h = gteqZero (g - h)
>
> g || h = fuzzyZero (g - h)

Now let's consider the game G = { 0 | 1 }. What do we know about it?

> half = Game [zero] [one]
>-- half > 0 ==> True
>-- half < 1 ==> True

We might guess that this game is 1/2. We can check:

>-- half + half == 1 ==> True

And so it is. We could also find a game representing 1/4:

> fourth = Game [zero] [half]
>-- fourth + fourth + fourth + fourth == 1 ==> True

Continuing like this we can construct every dyadic rational,
i.e. rationals where the denominator is a power of two. The idea is,
2p+1 / 2^n = { p / 2^(n-1) | (p+1) / 2^(n-1) }, so we're constructing
dyadic rationals out of simpler ones. It turns out that every real
number can be written as a game. To see how this is possible, notice
that the dyadic rationals are dense in the reals. We could then
squeeze every number between two infinite sequences of dyadic
rationals. For example,

2/3 = { 0, 1/2, 5/8, 21/32 ... | ... 43/64, 11/16, 3/4, 1 }

The only reals with finite representations are the dyadic rationals,
so we'll stick with those. Again we're going to cheat and use
Fractional, even though games in general do not have well defined
division.

> powerOf2 :: (Integral a) => a -> Bool
> powerOf2 n | n == 0 = True
>            | n == 1 = True
>            | even n = powerOf2 (n `div` 2)
>            | otherwise = False
>
> dyadic :: Rational -> Bool
> dyadic = powerOf2 . denominator
>
> instance Fractional Game where
>   fromRational r | denominator r == 1 = fromInteger (numerator r)
>                  | dyadic r = Game [left] [right]
>                  | otherwise = error "Cannot convert non-dyadic Rational to Game"
>                  where newNumerator = (numerator r - 1) `div` 2
>                        newDenominator = denominator r `div` 2
>                        left  = fromRational (newNumerator % newDenominator)
>                        right = fromRational ((newNumerator + 1) % newDenominator)

Checking that this works:

>-- fromRational (1%4) == fourth ==> True

If you step back, it's incredible that these games, with such a simple
definition, contain all the real numbers and much more besides.

If we're given a game, we might like to know if it represents a
number. The conditions for this are quite easy; a game is a number if
all its Left and Right options are numbers, and also every Left option
is < every Right option.

> isNumber :: Game -> Bool
> isNumber (Game left right) =    all isNumber left
>                              && all isNumber right
>                              && all (\l -> all (l<) right ) left
>-- isNumber zero ==> True
>-- isNumber one ==> True
>-- isNumber (fromRational (3%8)) ==> True
>-- isNumber star ==> False

It's time to go back and look at some strange infinitesimal games.

Let's consider G = { 0 | ∗ }, written ↑. ↑ is clearly positive, as
Left always wins. To see this, note that if Left moves first, he moves
to zero and Right loses. If Right moves first, he has to move to star,
where Left can move to zero and make him lose again. There is of
course an equivalent game, ↓ for Right.

> up   = Game [zero] [star]
> down = Game [star] [zero] -- = -up

>-- up > 0 ==> True
>-- down < 0 ==> True
>-- isNumber up ==> False
>-- isNumber down ==> False

Just how positive is ↑?

>-- up < 1 ==> True
>-- up < fromRational (1%2) ==> True
>-- up < fromRational (1%4) ==> True
>-- up < fromRational (1%8) ==> True
>-- up < fromRational (1%16) ==> True

Not very positive. It turns out ↑ is smaller than every positive
number. We can add ↑ to itself as many times as we like and it will
still be infinitesimally small.

↑ > 0, but how does it compare to ∗?

>-- up > star ==> False
>-- up || star ==> True
>-- down || star ==> True

∗'s fuzziness includes both ↑ and ↓. How about two copies of ↑?

>-- (up + up) > star ==> True
>-- (down + down) < star ==> True

So ↑+↑, written ⇑, is no longer confused with ∗.

So far, the games we've been considering are all infinitesimally close
to an actual number, as ∗ and all multiples of ↑ are infinitesimally
close to 0. This need not be the case in general, as can be seen in
the game G = { 1 | -1 }.

> switch = Game [1] [-1]

This game is known as a switch game, and is written ±1. Both players
are desperate to play in this game, as the result for them is much
better than if the other player makes their move first. Compare this
with G = { -1 | 1 } = 0, where both players would rather not move as
it just makes them one move closer to a loss.

±1 turns out to be fuzzy with all games between -1 and 1.

>-- switch < 2 ==> True
>-- switch || 1 ==> True
>-- switch || 0 ==> True
>-- switch || -1 ==> True
>-- switch > -2 ==> True

This makes sense, as if you add ±1 to any game in that range, the
outcome is still determined by whoever gets to play in ±1 first. For
values outside that range, ±1 isn't enough to tip the scales in the
other player's favour. For example, ±1 + 2 is still a win for Left,
even if Right goes first and plays ±1 to -1.

An important class of games is that of all 'impartial' games. These
are games where both players have the same set of moves they can
make. In other words, 'spinning the board' has no effect and the
result is the same position. One nice example of an impartial game is
the game of Nim.

In the game of Nim, the state of the game is represented as a few
piles of chips. A valid move is one that removes some chips from a
single pile. Say we had the piles [2, 4, 5], then a valid move could
be to [2, 4, 2], reducing the pile of 5 to 2. First let's represent a
single Nim pile as a game. From a pile of size n, either player can
move to any pile of size less than n. Of course, if the pile has size
0, neither player can do anything. That suggests the following
definition.

> nim :: Int -> Game
> nim 0 = zero
> nim n = Game options options
>   where options = map nim [0..n-1]

The value corresponding to 'nim n' is denoted ∗n, and is called a
nimber. All nimbers (except 0) are fuzzy, as the first player can take
the whole pile and win. Now to build a full Nim position, we just sum
up the values of the individual piles.

> nimPiles :: [Int] -> Game
> nimPiles = sum . map nim

It's an amazing fact that every impartial game is equivalent to some
nimber. In particular, the sum of two nimbers is another nimber. We
might hope that this addition works like normal addition, but that's
not the case. This is obvious when adding a nimber to itself, as
impartial games are their own inverses meaning two copies of any
nimber sum to zero. We do have that ∗1 + ∗2 = ∗3:

>-- nim 1 + nim 2 == nim 3 ==> True

But because ∗n = -∗n we can add ∗1 to both sides and get ∗2 = ∗3 + ∗1.

>-- nim 1 + nim 3 == nim 2 ==> True

Nimber-addition turns out to have a XOR like structure, where powers
of two in each summand cancel out.

> nimPlus :: Int -> Int -> Int
> nimPlus a b = a `xor` b
>
>-- 1 `nimPlus` 3 ==> 2

Even more impressive is that you can define a product on nimbers,
meaning impartial games form a field.

One issue we've skimmed over is that there are lots of different ways
of representing a single game. For example, { 1 | } = { 0, 1 | } = 2

>-- Game [one] [] == two ==> True
>-- Game [zero, one] [] == two ==> True

We've really been using equivalence classes of games. For example, the
'zero game' in our abelian group is really the equivalence class of
all games equal to zero.

The games we've been dealing with are all 'short' games; there are
only finitely many positions the game can be in. Thankfully, every
short game has a unique normal form, the simplest representation of
the game. To get to this normal form, two simplifications are used:

The first is removing 'dominated' options. Looking back at { 0, 1 | },
Left has no reason to ever move to 0 when the better move 1 is
available. In general, if Left has A and B as options, and A <= B,
then A can be removed without changing the value of the game. We have
to be careful here, it's not a matter of just choosing the 'maximum'
options, because some moves could be fuzzy with others.

> unbeaten :: (a -> a -> Bool) -> [a] -> [a]
> unbeaten p [] = []
> unbeaten p (x:xs) = if any (p x) rest then
>                        rest
>                     else
>                       x : filter (not . flip p x) rest
>   where rest = unbeaten p xs
>
> removeDominated :: Game -> Game
> removeDominated g = Game left right
>   where left  = unbeaten (<=) (leftMoves g)
>         right = unbeaten (>=) (rightMoves g)

The other way of simplifying games is by removing 'reversible'
moves. If Left has a move where Right's response gives a position
better for Right than the original game, then Left's move is called
reversible. If Left decides to make that move, he must anticipate that
Right will reverse it into something better for him. We can bypass
these and let Left jump straight to what he would do after that.

> lReversible :: Game -> Game -> [Game]
> lReversible g gl = maybe [gl] leftMoves (find (<= g) (rightMoves gl))
>
> rReversible :: Game -> Game -> [Game]
> rReversible g gr = maybe [gr] rightMoves (find (>= g) (leftMoves gr))
>
> anyReversible :: Game -> Bool
> anyReversible g = any (<= g) (concatMap rightMoves (leftMoves g)) Prelude.|| any (>= g) (concatMap leftMoves (rightMoves g))
>
> bypassReversible :: Game -> Game
> bypassReversible g = Game left right
>   where left  = concatMap (lReversible g) (leftMoves g)
>         right = concatMap (rReversible g) (rightMoves g)

Now, to simplify a game, we just combine the two operations, then
apply the simplification to all of the subgames. We need to repeatedly
check for reversible moves, as each round of simplification could
expose new ones.

> simplifyTop :: Game -> Game
> simplifyTop = removeDominated . until (not . anyReversible) (bypassReversible . removeDominated)
>
> simplify :: Game -> Game
> simplify g = Game (map simplify (leftMoves s)) (map simplify (rightMoves s))
>              where s = simplifyTop g

It can be proven that the game given by the two simplifications always
exists and is unique.

Before we can apply what we've learned to a real game, we'd like some
way to easily read off what the value of a game is, if it happens to
correspond to some simple value that we already understand.

Many of the games we find in real play are just the sum of a number, a
multiple of ↑, and a nimber. Because this is so common, we will create
a new type for it:

> data NumberUpStar = NUS { numberPart :: Rational, upPart :: Int, nimberPart :: Int } deriving (Eq, Ord)
>
> nusIsNumber nus = upPart nus == 0 && nimberPart nus == 0

We can leverage the simplification of games to make it easier to
convert from an arbitrary game to a NumberUpStar. For example, in a
simplified game, if Left only has one option and Right has none, we
must be dealing with an integer. Many similar rules, when combined,
will give us the function we require.

> optionsToNUS :: ([NumberUpStar], [NumberUpStar]) -> Maybe NumberUpStar
> -- Zero game
> optionsToNUS ([], []) = Just $ NUS 0 0 0
>
> -- If G = { L | } then L is an integer and G = L + 1
> optionsToNUS ([l], []) = Just $ NUS (lValue + 1) 0 0
>                                 where lValue = numberPart l
>
> -- If G = { | R } then R is an integer and G = R - 1
> optionsToNUS ([], [r]) = Just $ NUS (rValue - 1) 0 0
>                                 where rValue = numberPart r
>
> -- If G = { L | R } and L and R are both numbers, G = average of L and R
> optionsToNUS ([l], [r]) | nusIsNumber l && nusIsNumber r
>                           && numberPart l < numberPart r = Just $ NUS value 0 0
>                                 where value = (numberPart l + numberPart r) / 2
>
> -- Here we are of the form n + { 0 | G }, where G has non-negative ups
> optionsToNUS ([l], [r]) | nusIsNumber l && not (nusIsNumber r)
>                           && numberPart l == numberPart r
>                           && upPart r >= 0
>                             = Just $ NUS (numberPart l) (upPart r + 1) (nimberPart r `nimPlus` 1)
>
> -- Now n + { G | 0 }, where G has non-positive ups
> optionsToNUS ([l], [r]) | not (nusIsNumber l) && nusIsNumber r
>                           && numberPart l == numberPart r
>                           && upPart l <= 0
>                             = Just $ NUS (numberPart r) (upPart l - 1) (nimberPart l `nimPlus` 1)
>
> -- If G = { n, n∗ | n }, G = n↑∗
> optionsToNUS ([l1, l2], [r]) | nusIsNumber l1 && nusIsNumber r
>                                && l1 == r && l2 == NUS (numberPart l1) 0 1
>                                  = Just $ NUS (numberPart l1) 1 1
>
> -- If G = { n | n, n∗ }, G = n↓∗
> optionsToNUS ([l], [r1, r2]) | nusIsNumber l && nusIsNumber r1
>                                && l == r1 && r2 == NUS (numberPart r1) 0 1
>                                  = Just $ NUS (numberPart r1) (-1) 1
>
> -- Last possibility to check, we are looking at G = n + ∗k
> optionsToNUS (l1:ls, r1:rs) | length ls == length rs && nusIsNumber l1
>                               && l1 == r1 && nimberOptions = Just $ NUS (numberPart l1) 0 (length ls + 1)
>   where nimberOptions = all valid (zip3 ls rs [1..])
>         valid (l, r, i) = l == r && numberPart l == numberPart l1
>                           && upPart l == 0 && nimberPart l == i
>
>
>
> optionsToNUS _ = Nothing
>
> nusOptionsFrom :: Game -> Maybe ([NumberUpStar], [NumberUpStar])
> nusOptionsFrom g = do
>   left <- mapM simplifiedToNUS (leftMoves g)
>   right <- mapM simplifiedToNUS (rightMoves g)
>   return (sort left, sort right)
>
> -- Assumes the game given to it is simplified
> simplifiedToNUS :: Game -> Maybe NumberUpStar
> simplifiedToNUS = nusOptionsFrom >=> optionsToNUS

If we have one of these NumberUpStars, it's easy to print out a simple
representation of it. It's standard when dealing with games to write
3↑∗ for 3 + ↑ + ∗. We just need to be careful not to confuse this for
multiplication.

> instance Show NumberUpStar where
>   show (NUS 0 0 0) = "0"
>   show (NUS number up star) = numberShow number ++ upShow up ++ starShow star
>     where numberShow n | n == 0 = ""
>                        | denominator n == 1 = show $ numerator n
>                        | otherwise = "(" ++ show (numerator n) ++ "/" ++ show (denominator n) ++ ")"
>
>           upShow n | n == 0 = ""
>                    | n > 0 = replicate n '↑'
>                    | n < 0 = replicate (-n) '↓'
>
>           starShow n | n == 0 = ""
>                      | n == 1 = "∗"
>                      | otherwise = "∗" ++ show n

>-- show (NUS 0 0 0) ==> "0"
>-- show (NUS 3 2 1) ==> "3↑↑∗"

Now, to show a game, we first try to convert it to a NumberUpStar. If
this fails, we just print its left and right options.

> instance Show Game where
>   show g = string (simplifiedToNUS s)
>     where s = simplify g
>           string (Just nus) = show nus
>           string Nothing = "{ " ++ leftString ++ " | " ++ rightString ++ " }"
>           leftString = intercalate ", " (map show (leftMoves s))
>           rightString = intercalate ", " (map show (rightMoves s))

Finally we have a easy way to identify the games we create. You could
try yourself to combine the games we already have.  Some of the
identities are very surprising!

>-- Game [0] [up] ==> ↑↑∗

Now let's work on analysing a real game. The game of Toads and Frogs
is played on a strip of squares. Each square is either empty, or has a
Toad or a Frog in it. Whenever it's lefT's turn, he can either move a
Toad rightwards into an empty space, or hop over a Frog to the right
of him to land in an empty space. Right's move are identical, but
moving the fRogs in the other direction.

For example, if our board is in the state [ T _ T F _ ], Left could
either move the first Toad like this: [ _  T T F _ ] or hop the second
Toad like this: [ T _ _ F T ].

A standard starting board might look like this [ T T _ _ F F ]. Let's
use what we've developed above to tell us how to play this.

First we will need a type to represent the state of a square and the
board.

> data Square = T | F | E deriving (Eq, Show)
>
> type Board = [Square]

Now, given a board, we want to know what moves are possible for each
player.

> leftTFMoves :: Board -> [Board]
> leftTFMoves (T:E:rest) = ([E,T] ++ rest) : map ([T,E] ++) (leftTFMoves rest)
> leftTFMoves (T:F:E:rest) = ([E,F,T] ++ rest) : map ([T,F,E] ++) (leftTFMoves rest)
> leftTFMoves (x:rest) = map ([x] ++) (leftTFMoves rest)
> leftTFMoves [] = []

To find right's possible moves, we switch the players on the board,
find Left's moves, then switch back.

> switchPlayers :: Board -> Board
> switchPlayers = reverse . map switch
>   where switch T = F
>         switch F = T
>         switch E = E
>
> rightTFMoves :: Board -> [Board]
> rightTFMoves = map switchPlayers . leftTFMoves . switchPlayers

Now it's a simple matter to convert a board to a game.

> boardToGame :: Board -> Game
> boardToGame b = Game (map boardToGame (leftTFMoves b)) (map boardToGame (rightTFMoves b))

We can use this immediately to find the value of our board:

>-- boardToGame [T,T,E,E,F,F] ==> ∗

So if you and a friend are playing on this board, you had better ask
to play first!

Some very strange values can appear as positions in Toads and Frogs:

>-- boardToGame [T,T,T,F,E,F] ==> { { (1/4) | 0 } | 0 }
>-- boardToGame [E,T,T,T,E,F] ==> { 1∗ | 0 }
>-- boardToGame [E,T,T,E,F,F,E] ==> { (1/4) | (-1/4) }

You might recognise the last one as ±(1/4). The next obvious question
is, what's the best move from a given position?

> bestLeftMove :: Board -> (Board, Game)
> bestLeftMove b = maximumBy (compare `on` snd) movesValues
>   where allMoves = leftTFMoves b
>         movesValues = zip allMoves (map boardToGame allMoves)

>-- bestLeftMove [E,T,T,E,F,F,E] ==> ([E,T,E,T,F,F,E], (1/4))

With this up your sleeve, you should be able to beat pretty much
anyone.

===

For a much better written, much more rigorous and much more
entertaining introduction to combinatorial games, see:

Winning Ways for your Mathematical Plays (Academic Press, 1982)
by Berlekamp, Conway and Guy

For an almost unbelievably fast implementation of operations on
combinatorial games, see:

Combinatorial Game Suite: http://cgsuite.sourceforge.net/
by Aaron Siegel
