module MinimaxLib (abMinimax) where

import Data.List (find)
import Debug.Trace (trace)

abMinimax :: (Eq a,Show a) 
          => (a -> Bool)      -- terminal state test
          -> (a -> Int)       -- utility function
          -> (a -> [(b, a)])  -- successors: state to list of (action, state) pairs
          -> a                -- initial state
          -> Maybe b          -- the best action
abMinimax terminalTest utility successors state =
  -- swap the -∞ and ∞
  -- this currently breaks it though
  let (v, a, s) = maxValue 
                    terminalTest 
                    utility 
                    successors 
                    state 
                    Nothing 
                    (minBound :: Int) 
                    (maxBound :: Int) 
  in
    -- trace (show s ++ show v)
    a


maxValue :: Show a
         => (a -> Bool)       -- terminal state test
         -> (a -> Int)        -- utility function
         -> (a -> [(b, a)])   -- successors: state to list of (state, action) pairs
         -> a                 -- initial state
         -> Maybe b           -- action
         -> Int               -- alpha
         -> Int               -- beta
         -> (Int, Maybe b, a) -- score, action, state tuple
maxValue terminalTest utility successors state action alpha beta =
  if terminalTest state then 
    (utility state, action, state)
  else
    let (v, a, s, _, _) = foldr f ((minBound :: Int), action, state, alpha, beta) (successors state) in
      (v, a, s)
    where 
      {-- f :: (b,a) 
        -> (Int, Maybe b, a, Int, Int) 
        -> (Int, Maybe b, a, Int, Int) --}
      f (a, s) (score, accAct, accState, alpha, beta) =
        let
          alpha' = max score alpha
        in
            if False {-- alpha' >= beta --} then 
              (score, accAct, accState, alpha', beta)
            else
              let (v, a', s') = minValue terminalTest utility successors s (Just a) alpha beta in
              (v, a', s', max alpha v, beta)


minValue :: Show a
         => (a -> Bool)       -- terminal state test
         -> (a -> Int)        -- utility function
         -> (a -> [(b, a)])   -- successors: state to list of (state, action) pairs
         -> a                 -- initial state
         -> Maybe b           -- move
         -> Int               -- alpha
         -> Int               -- beta
         -> (Int, Maybe b, a) -- score, action, state tuple
minValue terminalTest utility successors state action alpha beta =
  if terminalTest state then 
    (utility state, action, state)
  else
    let (v, a, s, _, _) = foldr f ((maxBound :: Int), action, state, alpha, beta) (successors state) in
    (v, a, s) 
    where 
      f (a, s) (score, accAct, accState, alpha, beta) =
        let
          beta' = min beta score
        in
            if False {-- alpha >= beta' --} then 
              (score, accAct, accState, alpha, beta)
            else
              let (v, a', s') = maxValue terminalTest utility successors s (Just a) alpha beta in
              (v, a', s', alpha, min beta v)
