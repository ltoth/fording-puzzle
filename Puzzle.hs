{-# LANGUAGE LambdaCase #-}
import Data.Set (Set)
import qualified Data.Set as Set

data Family = A | B | C deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Role = Father | Daughter deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Person = Person Role Family deriving (Show, Read, Eq, Ord)

data Boat = Near | Far deriving (Show, Read, Eq, Ord)

type NearShore = Set Person
type FarShore = Set Person

data World = World NearShore FarShore Boat deriving (Show, Read, Eq)

initialWorld = World allPeople
                     Set.empty
                     Near

allPeople = Set.fromList [Person r f | r <- enumAll, f <- enumAll]

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound .. maxBound]

isDaughterOf :: Person -> Person -> Bool
(Person Daughter f1) `isDaughterOf` (Person Father f2) = f1 == f2
_ `isDaughterOf` _ = False

legalMoves :: World -> [World -> World]
legalMoves w@(World n f b) = [move (Set.singleton s) | s <- Set.toList (allPeopleOnShore b w)]
                         -- ++ [other legal moves]

allPeopleOnShore :: Boat -> World -> Set Person
allPeopleOnShore Near (World n f _) = n
allPeopleOnShore Far  (World n f _) = f

move :: Set Person -> World -> World
move s = \case
            (World n f Near) -> World (Set.difference n s') (Set.union f s') Far
              where s' = Set.intersection n s
            (World n f Far)  -> World (Set.union n s') (Set.difference f s') Near
              where s' = Set.intersection f s
