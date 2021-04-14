module MinimaxLib (abMinimax) where

import Debug.Trace (trace)
import System.Random (mkStdGen, RandomGen, next)

shuffle :: Int
        -> [a] 
        -> [a]
shuffle a lst =
  let
    shuffle' :: [Int] -> [a] -> [a]
    shuffle' _ [] = []
    shuffle' [i] xs =  xs
    shuffle' (i:is) xs = 
      let 
        (firsts, rest) = splitAt (i `mod` length xs) xs
      in 
        if null firsts  then
          shuffle' is rest
        else
          last firsts : shuffle' is (init firsts ++ rest)

    randomStream seed = 
      let
        gen = mkStdGen seed
        helper :: RandomGen g 
               => g 
               -> [Int]
        helper g =
          let
            (i,g') = next g
          in
            i:helper g'
      in
        helper gen

    seed = 6969+a
  in
    shuffle' (take (length lst) $randomStream seed) lst


abMinimax :: (Eq a,Show a,Show b) 
          => (a -> Bool)      -- terminal state test
          -> (a -> Int)       -- utility function
          -> (a -> [(b, a)])  -- successors: state to list of (action, state) pairs
          -> Int
          -> a                -- initial state
          -> Maybe b          -- the best action
abMinimax terminalTest utility successors maxDepth state =
  -- swap the -∞ and ∞
  let (v, a, s) = maxValue 
                    terminalTest 
                    utility 
                    successors 
                    maxDepth
                    state 
                    Nothing 
                    (minBound :: Int) 
                    (maxBound :: Int) 
  in
    trace (show s ++ show v ++ "a="++ show a)
    a


maxValue :: Show a
         => (a -> Bool)       -- terminal state test
         -> (a -> Int)        -- utility function
         -> (a -> [(b, a)])   -- successors: state to list of (state, action) pairs
         -> Int               -- depth
         -> a                 -- initial state
         -> Maybe b           -- action
         -> Int               -- alpha
         -> Int               -- beta
         -> (Int, Maybe b, a) -- score, action, state tuple
maxValue terminalTest utility successors depth state action alpha beta =
  if (depth <= 0) || (terminalTest state) then 
    (utility state, action, state)
  else
    let 
      (v, a, s, _, _) = foldr f ((minBound :: Int), action, state, alpha, beta) 
                                (successors state) 
    in
      (v, a, s)
    where 
      {-- f :: (b,a) 
        -> (Int, Maybe b, a, Int, Int) 
        -> (Int, Maybe b, a, Int, Int) --}
      f (a, s) (score, accAct, accState, alpha, beta) =
        let
          alpha' = max score alpha
        in
            if alpha' >= beta then 
              (score, accAct, accState, alpha', beta)
            else
              let (v, a', s') = minValue terminalTest utility successors (depth-1) s (Just a) alpha beta in
              (v, Just a, s', max alpha v, beta)


minValue :: Show a
         => (a -> Bool)       -- terminal state test
         -> (a -> Int)        -- utility function
         -> (a -> [(b, a)])   -- successors: state to list of (state, action) pairs
         -> Int               -- depth
         -> a                 -- initial state
         -> Maybe b           -- move
         -> Int               -- alpha
         -> Int               -- beta
         -> (Int, Maybe b, a) -- score, action, state tuple
minValue terminalTest utility successors depth state action alpha beta =
  if (depth <= 0) || (terminalTest state) then 
    (utility state, action, state)
  else
    let (v, a, s, _, _) = foldr f ((maxBound :: Int), action, state, alpha, beta) (successors state) in
    (v, a, s) 
    where 
      f (a, s) (score, accAct, accState, alpha, beta) =
        let
          beta' = min beta score
        in
            if alpha >= beta' then 
              (score, accAct, accState, alpha, beta)
            else
              let (v, a', s') = maxValue terminalTest utility successors (depth-1) s (Just a) alpha beta in
              (v, Just a, s', alpha, min beta v)
