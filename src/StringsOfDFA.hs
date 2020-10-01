{- | Module containing the function StringsOfDFA. 

      Takes a DFA and an integer number k and returns how many 
      words of size k belong to the language of the given DFA 
      defined by the module DFA given in the assignment
      
      Ana Sofía Gutiérrez-}
module StringsOfDFA (
  -- * Main Function for the recursively defined algorithm
    stringsOfDFA, 
  -- * Main Support functions
  auxStringsOfDFA, stringstoAnyState,
  -- * Secondary supporting functions
  listMultiplication, directWaysStartToAny,
  directWaysAnyij,
  -- * Other supporting functions
  indexAccepting, auxStates, auxMultiplication, aux2Multiplication, 
  auxpartialways, acceptingStates, states, 
  -- * Suggested examples
  main
  ) where

import Data.Map as Map (Map, empty, insertWith, singleton, union, elems, 
                              keys, keysSet)
import Data.Set as Set (Set, empty, insert, elems, findIndex, fromList)
import Data.Map.Lazy ((!))
import Data.List (nub)
-- * Data DFA type
-- | As defined in the given Module of the assignment
-- | Represents a DFA with states of type @s@ and input symbols of type @c@
data DFA s c = MkDFA
  { -- | A unique initial state
    start         :: s
  , {- | The transition function.
         The outer map stores all transitions available from a state,
         inner maps stores the endpoint of a transition using a given symbol -}
    delta         :: Map s (Map c s)
  , -- | A set of final/accepting states
    accepting     :: Set s
  } deriving Show

-- | Creates a new DFA with a starting state @q0@
initDFA :: s -> DFA s a
initDFA q0 = MkDFA q0 Map.empty Set.empty

-- | Adds a new transition from state @q@ to @q'@ with a symbol @c@ to a DFA
trans :: (Ord s, Ord c) => (s, s, c) -> DFA s c -> DFA s c
trans (q, q', c) (MkDFA q0 ts f) = MkDFA q0 ts' f
  where ts' = Map.insertWith Map.union q (Map.singleton c q') ts

-- | Adds a new accepting state @q@ to a DFA
accept :: (Ord a, Ord s) => s -> DFA s a -> DFA s a
accept q (MkDFA q0 ts fs) = MkDFA q0 ts (Set.insert q fs)

{-| First automaton example. The language of this DFA does not contain any
    words.-}
dfa0 :: DFA Int String
dfa0 = trans (0, 0, "(^o^)/") $
       trans (0, 0, "(-_-)") $
       initDFA 0

{-| Second automaton example. This DFA represents the language that only
  recognizes the empty word. -}
dfa1 :: DFA Int String
dfa1 = trans (0, 1, "(^o^)/") $
       trans (0, 1, "(-_-)") $
       trans (1, 1, "(^o^)/") $
       trans (1, 1, "(-_-)") $
       accept 0 $
       initDFA 0

{-| Third automaton example. This DFA represents the language of the regex
    L(dfa2) = (0 + 1 + 2)*. -}
dfa2 :: DFA Int Int
dfa2 = trans (0, 0, 0) $
       trans (0, 0, 1) $
       trans (0, 0, 2) $
       accept 0 $
       initDFA 0

{-| Last automaton example. This DFA represents the language of the regex
    L(dfa3) = abc(abc)*. -}
dfa3 :: DFA Int Char
dfa3 = trans (0, 1, 'a') $ trans (0, 2, 'b') $
       trans (0, 2, 'c') $ trans (1, 2, 'a') $
       trans (1, 3, 'b') $ trans (1, 2, 'c') $
       trans (2, 2, 'a') $ trans (2, 2, 'b') $
       trans (2, 2, 'c') $ trans (3, 2, 'a') $
       trans (3, 2, 'b') $ trans (3, 4, 'c') $
       trans (4, 1, 'a') $ trans (4, 2, 'b') $
       trans (4, 2, 'c') $ accept 4 $ initDFA 0

{-| Number of ways from state 'h' to state 'j' in the DFA without any 
    intermediate transitions. -}
directWaysAnyij :: (Ord s, Ord c) => s -> s -> DFA s c -> Int
directWaysAnyij h j (MkDFA _ ts _) = length $ filter (==j) (Map.elems (ts!h))

-- | List of States in the list '[Maps c s]' that receive transitions.
auxStates :: (Ord s, Ord c) => [Map c s] -> Int -> [s]
auxStates mapa j
  | j> length mapa -1 = []
  | otherwise = Map.elems (mapa!!j) ++ auxStates mapa (j+1)

{-| List of states in the DFA. Uses 'auxStates' to find states that are not 
    starting point from any transition. -}
states :: (Ord s, Ord c) => DFA s c -> [s]
states (MkDFA _ ts _)  = nub ( keys ts ++ auxStates (Map.elems ts) 0 )

{-| List of different ways to reach any state from the /q0/ state.

    Uses 'directWaysAnyij' with the /q0/ accepting state
    of the DFA and, recursively, any other state. -}

-- The 'j' argument must have default value 0. 
directWaysStartToAny :: (Ord s, Ord c) => DFA s c -> Int -> [Int]
directWaysStartToAny (MkDFA q0 ts fs) j   
  | j>=length (states (MkDFA q0 ts fs)) = []
  | otherwise = directWaysAnyij q0 q' (MkDFA q0 ts fs)
              : directWaysStartToAny (MkDFA q0 ts fs) (j+1)
                  where q' = states (MkDFA q0 ts fs)!!j

-- | Returns the list of accepting states in the DFA.
acceptingStates :: (Ord s, Ord c) => DFA s c -> [s]
acceptingStates (MkDFA _ _ fs)  = Set.elems fs

{-| Returns a list of the amount of different transitions from any state
    in the DFA to the given state in the index 'j'.-}

-- The 'h' argument must have default value 0. 
auxpartialways :: (Ord s, Ord c) => DFA s c -> Int -> Int -> [Int]
auxpartialways (MkDFA q0 ts fs) h j
  | h>=length (Map.elems ts)  = []
  | otherwise = directWaysAnyij q q'(MkDFA q0 ts fs)
              : auxpartialways (MkDFA q0 ts fs) (h+1) j
                  where q=states (MkDFA q0 ts fs)!!h
                        q'=states (MkDFA q0 ts fs)!!j

{-| Takes to lists and, treating them as vectors, performns 
    vector multiplication. The dimensions of the lists are consider 
    as 1xn and nx1, respectively. -}

-- The 'j' argument must have default value 0. 
aux2Multiplication :: [Int] -> [Int] -> Int ->Int
aux2Multiplication m v j
  | j>length v-1  = 0
  | otherwise = (m!!j)*(v!!j) + aux2Multiplication m v (j+1)

{-| Finds the second vector v' (as a list) necessary for the recursively defined
    product defined in 'aux2Multiplication'. 
    
    This list, considered as a vector, is the number of ways that reach 
    the 'x' state in the DFA from any other. -}
auxMultiplication ::(Ord s, Ord c) => DFA s c  -> [Int] -> Int -> Int
auxMultiplication dfa v x = aux2Multiplication v' v 0
  where v'= auxpartialways dfa 0 x

{-| Returns a list where each value in the position x is the result of 
    the multiplication of a vector 'v' with the vector of direct ways 
    between any state towards each x state. 

    Each value in the x index of the output is defined in the operation 
    'auxMultiplication'. -}

-- The 'm' argument must have default value 0.  
listMultiplication ::(Ord s, Ord c) => DFA s c  -> [Int] -> Int -> [Int]
listMultiplication dfa v m
  | m>length(states dfa)-1   = []
  | otherwise = auxMultiplication dfa v m : listMultiplication dfa v (m+1)

{-| Returns a list where each value in the position x represents 
    how many words of size 'k' can reach each state x parting from /q0/. -}
stringstoAnyState :: (Ord s, Ord c) => DFA s c -> Int -> [Int]
stringstoAnyState dfa k
  | k==1      = directWaysStartToAny dfa 0
  | otherwise = listMultiplication dfa (stringstoAnyState dfa (k-1)) 0

{-| Returns the indexed position of the accepting states in the list of states
    that are returned by the 'states' function.-}

-- The 'j' argument must have default value 0. 
indexAccepting :: (Ord s, Ord c) => DFA s c -> Int -> [Int]
indexAccepting dfa j
  | j>(length qf -1)  = []
  | otherwise = findIndex (qf!!j) q : indexAccepting dfa (j+1)
                  where qf=acceptingStates dfa
                        q =fromList (states dfa)

{-| Returns the sumation of the values in the output of 'stringstoAnyState'
    that represent how many words of size 'k' reach an accepting state. 
    The index of the accepting states are the output of 'indexAccepting'. -}

-- The 'j' argument must have default value 0.
auxStringsOfDFA :: (Ord s, Ord c) => DFA s c -> Int -> Int -> Int
auxStringsOfDFA dfa k j
  | j>length(acceptingStates dfa)-1  = 0
  | otherwise = anyWay!!qf + auxStringsOfDFA dfa k (j+1)
                where anyWay=stringstoAnyState dfa k
                      qf    =indexAccepting dfa 0!!j

{-| Solution algoritm of the problem: 'stringsOfDFA' 

    * Basic step 'k'=0
    
    Words of length 0 are accepted if the initial state /q0/
    is part of the accepting states of the DFA. 
    
    In this case, if the output of the function 'indexAccepting'
    with the DFA contains the index of the state /q0/ as an 
    accepting state

    If the Input 'k' is different to 0, it executes the function 
    'auxStringsOfDFA'. This function considers a second basic step 
    and the recursion

    * Basic step 'k'=1.

    In this case, the number of accepted words depends on how many 
    direct ways are between the accepting state /q0/ and the 
    accepting states.

    * Recursive step

    'auxStringsOfDFA' also contains the recursive steps for the 
    solution.

    The number S of words of k-length that reach the 'j' state 
    starting off /q0/ are calculated by the formula 
    
    \(S_{q_0\, j}^{k}=\sum_{h}S_{q_0\, h}^{k-1}*S_{h\, j}^{1}\)

    From here, the S values that reach accepting states are added in
    'auxStringsOfDFA'
    -}
stringsOfDFA :: (Ord s, Ord c) => DFA s c -> Int -> Int
stringsOfDFA dfa k 
  | k==0  = if null (indexAccepting dfa 0) || head (indexAccepting dfa 0) /= 0 
            then 0 else 1
  | otherwise = auxStringsOfDFA dfa k 0

{-| Prints the solution of the suggested examples in the assigment calculated 
    with the 'stringsOfDFA' proposed algorithm-}
main :: IO ()
main = do
    --execute tests
    print (stringsOfDFA dfa0 0)
    print (stringsOfDFA dfa1 0)
    print (stringsOfDFA dfa1 10)
    print (stringsOfDFA dfa2 0)
    print (stringsOfDFA dfa2 3)
    print (stringsOfDFA dfa2 9)
    print (stringsOfDFA dfa3 3)
    print (stringsOfDFA dfa3 4)
    print (stringsOfDFA dfa3 12)
