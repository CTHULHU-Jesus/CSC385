module MinimaxLib (abMinimax) where

import Debug.Trace (trace)
import Data.Foldable (foldr')


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
    trace (show s ++ "v=" ++ show v ++ "a=" ++ show a)
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
  if (depth <= 0) || terminalTest state then 
    (utility state, action, state)
  else
    let 
      (v, a, s, _, _) = 
        foldr' 
          f 
          (minBound :: Int, action, state, alpha, beta) 
          (successors state) 
    in
      (v, a, s)
    where 
      f (a, s) pre@(score, accAct, accState, alpha, beta) =
        let
          a' = Just a
        in
            if alpha >= beta then 
              pre
            else
              let 
                (v, _, s') = 
                  minValue 
                    terminalTest 
                    utility 
                    successors 
                    (depth-1) 
                    s 
                    a'
                    alpha 
                    beta 
              in
                if score > v then
                  pre
                else
                  (v, a', s', max alpha v, beta)


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
  if (depth <= 0) || terminalTest state then 
    (utility state, action, state)
  else
    let 
      (v, a, s, _, _) = 
        foldr' 
          f 
          (maxBound :: Int, action, state, alpha, beta) 
          (successors state) 
    in
      (v, a, s) 
    where 
      f (a, s) pre@(score, accAct, accState, alpha, beta) =
        let
          a' = Just a
        in
            if alpha >= beta then 
              pre
            else
              let 
                (v, _, s') = 
                  maxValue 
                    terminalTest 
                    utility 
                    successors 
                    (depth-1) 
                    s 
                    a'
                    alpha 
                    beta 
              in
                if score > v then
                  (v, a', s', alpha, min beta v)
                else
                  pre

