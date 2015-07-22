import Prelude hiding (all, foldr)
import Control.Applicative
import Data.Foldable
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree
import Text.Printf

boatLimit = 2

data Family = A | B | C deriving (Show, Eq, Ord, Enum, Bounded)

data Role = F | D deriving (Show, Eq, Ord, Enum, Bounded)

data Person = P Role Family deriving (Eq, Ord)

instance Show Person where
  show (P r f) = show r ++ show f

data BoatLocation = Near | Far deriving (Show, Read, Eq, Ord)

data Location = OnShore BoatLocation | OnBoat deriving (Show, Eq)

type NearShore = Set Person
type Boat = Set Person
type FarShore = Set Person

data World = World NearShore Boat FarShore BoatLocation deriving (Eq, Ord)

instance Show World where
  show (World n b f l) = showShore Near n ++ showSep Near l ++ showBoat b ++ showSep Far l ++ showShore Far f
    where showShore Near s = printf "%17s"  $ unwords . map show $ Set.toList s
          showShore Far  s = printf "%-17s" $ unwords . map show $ Set.toList s
          showBoat       s = printf "%-5s"  $ unwords . map show $ Set.toList s
          showSep Far  Near = " ~~~         "
          showSep Near Far  = "         ~~~ "
          showSep _ _       = " | "

data Node = N { world :: World
              , visited :: Set World
              , path :: [World]
              } deriving (Show, Eq)

allPeople = [P r f | r <- enumAll, f <- enumAll]

initialWorld = World (Set.fromList allPeople) Set.empty Set.empty Near

main = putStrLn $ maybe "No solution" (unlines . map show) solution
--main = putStrLn . unlines . map show $ minSolution

solution = path <$> find (\(N (World _ _ f _) _ _) ->
                           f == Set.fromList allPeople) tree

minSolution = minimumBy (compare `on` length) solutions

--solutions = foldr (\(N (World _ _ f _) _ p) acc ->
--              if f == Set.fromList allPeople then p:acc else acc) [] tree

solutions = fst $ foldr (\(N (World _ _ f _) _ p) (acc, min) ->
                if f == Set.fromList allPeople && length p < min
                  then (p:acc, length p)
                  else (acc, min))
              ([], maxBound) tree

maxLength = 71

bSolution = head $ foldr (\(N (World _ _ f _) _ p) acc ->
                if f == Set.fromList allPeople && length p <= maxLength
                  then p:acc else acc)
              [] tree

initialNode = N initialWorld (Set.singleton initialWorld) [initialWorld]

tree = unfoldTree (\(N w v p) -> (N w v p, unvisited w v p)) initialNode
  where
    unvisited w@(World _ _ f _) v p
      | f == Set.fromList allPeople = []
      | otherwise = map (\w' -> N w' (Set.insert w' v) (w':p)) $
                    Set.toList $ legalMoves w `Set.difference` v

legalMoves :: World -> Set World
legalMoves w = Set.filter isWorldLegal worlds
  where worlds = Set.fromList $ moveBoat w : [move p w | p <- allPeople]

locatePerson :: Person -> World -> Location
locatePerson p (World n _ f _) | p `Set.member` n = OnShore Near
                               | p `Set.member` f = OnShore Far
                               | otherwise        = OnBoat

move :: Person -> World -> World
move p w@(World n b f Near) =
  case locatePerson p w of
    OnShore Near -> World (Set.delete p n) (Set.insert p b) f Near
    OnBoat       -> World (Set.insert p n) (Set.delete p b) f Near
    OnShore Far  -> w  -- impossible; person is on different shore from boat
move p w@(World n b f Far) =
  case locatePerson p w of
    OnShore Near -> w  -- impossible; person is on different shore from boat
    OnBoat       -> World n (Set.delete p b) (Set.insert p f) Far
    OnShore Far  -> World n (Set.insert p b) (Set.delete p f) Far

moveBoat :: World -> World
moveBoat w@(World n b f Near) = if Set.size b > 0 then World n b f Far else w
moveBoat w@(World n b f Far)  = if Set.size b > 0 then World n b f Near else w

isWorldLegal :: World -> Bool
isWorldLegal (World n b f l)
  | Set.size b > boatLimit = False
  | otherwise = isSetLegal n && isSetLegal b && isSetLegal f

isSetLegal :: Set Person -> Bool
isSetLegal s =
  all (== False) [P D f `Set.member` s &&
                  P F f `Set.notMember` s &&
                  P F f' `Set.member` s
                  | f <- enumAll, f' <- enumAll, f /= f']

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound .. maxBound]
