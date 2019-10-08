-- Inf2d Assignment 1 2018-2019
-- Matriculation number: s1701688
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = []

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth = 35
-- Why did you choose this number?
-- We consider the maximum depth of the 6x6 grid to be the longest path there iis
-- for the agent to take, without repeating any square in the grid, that is, without
-- visiting a square more than once. Therefore, the maximum depth is just 35 given
-- that the root node (start square) is on level 0 and the node for the longest path
-- will be on the 35th level of a tree.


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] =  []
next branch = [node : branch | node <- nextPositions, validNode node, node `notElem` branch, node `notElem` badNodesList]
             where
                (i, j) = head branch
                -- Possible next moves in the grid for the agent to take
                -- Depending on the order of the nodes we choose to expand, the search path of the robot will be different
                nextPositions = [(i, j+1), (i, j-1), (i+1, j), (i-1, j)]

-- validNode is an auxiliary function



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = curNode == destination -- Use boolean comparison to check if both nodes are equal



-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node -> (Branch -> [Branch]) -> [Branch] -> [Node] -> Maybe Branch
breadthFirstSearch destination next [] exploredList = Nothing -- Base case when no reachable nodes are left to explore
breadthFirstSearch destination next (branch:branches) exploredList
    | checkArrival destination (head branch) = Just branch -- Robot already at destination
    | head branch `elem` exploredList = breadthFirstSearch destination next branches exploredList -- Current node is an old node
    | otherwise = breadthFirstSearch destination next (branches ++ (next branch)) (head branch : exploredList)
    -- Else, expand search branch and add the new nodes to the back of the agenda so that old nodes are expanded before new nodes


-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node -> (Branch -> [Branch]) -> [Branch] -> [Node] -> Maybe Branch
depthFirstSearch destination next [] exploredList = Nothing
depthFirstSearch destination next (branch:branches) exploredList
    | checkArrival destination (head branch) = Just branch -- Destination node
    | head branch `elem` exploredList = depthFirstSearch destination next branches exploredList -- Current node already visited
    | otherwise = depthFirstSearch destination next (next branch ++ branches) (head branch : exploredList)
    -- Else, expand search branch and add the new nodes to the front of the agenda so that new nodes are expanded before old nodes


-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node -> (Branch -> [Branch]) -> [Branch] -> Int -> Maybe Branch
depthLimitedSearch destination next [] d = Nothing
depthLimitedSearch destination next (branch:branches) d
    | checkArrival destination (head branch) = Just branch
    | length branch > d = depthLimitedSearch destination next branches d -- If first branch exceeds the pre-determined depth, search for another branch
    | otherwise = depthLimitedSearch destination next (next branch ++ branches) d -- Since it is depth search, we concatenate the expanded branch to the start


-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.
iterDeepSearch:: Node -> (Branch -> [Branch]) -> Node -> Int -> Maybe Branch
iterDeepSearch destination next initialNode 0 = Nothing
iterDeepSearch destination next initialNode d
    | d > maxDepth = Nothing
    -- In each iteration, start at the root node again. If no path to the goal was found, increase d by one
    | depthLimitedSearch destination next [[initialNode]] d == Nothing = iterDeepSearch destination next initialNode (d+1)
    | otherwise = depthLimitedSearch destination next [[initialNode]] d -- Else, return the path found



-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan::Node->Node->Int
manhattan (px, py) (dx, dy) = abs(dx-px) + abs(dy-py)


-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic [] exploredList = Nothing
bestFirstSearch destination next heuristic (branch:branches) exploredList
    | checkArrival destination (head branch) = Just branch
    | head branch `elem` exploredList = bestFirstSearch destination next heuristic (sortBy (orderingBranchesBFS destination) [b | b <- branches]) exploredList
    | otherwise = bestFirstSearch destination next heuristic (sortBy (orderingBranchesBFS destination) [b | b <- (branches ++ next branch)]) (head branch : exploredList)

-- orderingBranchesBFS is an auxiliary function


-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost [] exploredList = Nothing
aStarSearch destination next heuristic cost (branch:branches) exploredList
    | checkArrival destination (head branch) = Just branch
    | head branch `elem` exploredList = aStarSearch destination next heuristic cost (sortBy (orderingBranchesAS destination) [b | b <- branches]) exploredList
    | otherwise = aStarSearch destination next heuristic cost (sortBy (orderingBranchesAS destination) [b | b <- (branches ++ next branch)]) (head branch : exploredList)

-- orderingBranchesAS is an auxiliary function


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch  -> Int
cost branch = length branch





-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and return -1, else return 0 as draw
eval game
    | terminal game = if checkWin game 1 then 1 else if checkWin game 0 then -1 else 0
    | otherwise     = 0


-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player
    | player == 1 = maxValueMM game player
    | otherwise   = minValueMM game player -- Player 0 chooses a move that minimizes player 1's final score

-- maxValueMM and minValueMM are auxiliary functions


-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

--Optimized version of alphabeta
alphabeta:: Game -> Player -> Int
alphabeta game player | player == 1 = maxValueAB game player (-2, 2) -- Range within -2 and 2
                      | otherwise   = minValueAB game player (-2, 2)

-- minValueAB and maxValueAB are auxiliary functions


-- My first version of the alphabeta, slower version of the algorithm. Follows the pseudocode closely.
-- alphabeta:: Game->Player->Int
-- alphabeta game player
--     | player == 1 = maxValueAB game player
--     | player == 0 = minValueAB game player
--
-- maxValueAB :: Game -> Player -> Int
-- maxValueAB game player = if terminal game then eval game
--                          else forLoopMaxValueAB game (-2) 2 (-2) (moves game 1)
--
-- forLoopMaxValueAB :: Game -> Int -> Int -> Int -> [Game] -> Int
-- forLoopMaxValueAB game alpha beta v [] = v
-- forLoopMaxValueAB game alpha beta v (g:games)
--     | newV >= beta = newV
--     | otherwise = forLoopMaxValueAB game newAlpha beta newV games
--                where newV = max v (minValueAB g 0)
--                      newAlpha = max alpha v
--
--
-- minValueAB :: Game -> Player -> Int
-- minValueAB game player = if terminal game then eval game
--                          else forLoopMinValueAB game (-2) 2 2 (moves game 0)
--
--
-- forLoopMinValueAB :: Game -> Int -> Int -> Int -> [Game] -> Int
-- forLoopMinValueAB game alpha beta v [] = v
-- forLoopMinValueAB game alpha beta v (g:games)
--     | newV <= alpha = newV
--     | otherwise = forLoopMinValueAB game alpha newBeta newV games
--                where newV = min v (maxValueAB g 1)
--                      newBeta = min beta v





-- | Section 5.2 Wild Tic Tac Toe


-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game
    | terminal game = if (checkWin game 1 || checkWin game 0) then 1 else 0 -- Player 1 = x, Player 0 = o
    | otherwise = 0


-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

alphabetaWild:: Game -> Player -> Int
alphabetaWild game player = maxValueABWild game player (-2, 2) -- Human player does the fist move

-- maxValueABWild is an auxiliary function, as well as minValueABWild which is called from maxValueABWild



-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player = undefined



			-- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores

-- SEARCHES
-- Function that checks if the node is within the grid dimensions. Used in the function 'next'
validNode :: Node -> Bool
validNode (i, j) = i >= 1 && i <= gridWidth_search && j >= 1 && j <= gridLength_search


-- Check ordering of the branches. Used in function 'bestFirstSearch'
orderingBranchesBFS:: Node -> Branch -> Branch -> Ordering
orderingBranchesBFS destination branch1 branch2
   | manhattan (head branch1) destination <= manhattan (head branch2) destination = LT -- Use manhattan heuristic to order the branches
   | otherwise = GT


-- Compare the branches and order in increasing order of function f(n). Used in function 'aStarSearch'
orderingBranchesAS :: Node -> Branch -> Branch -> Ordering
orderingBranchesAS destination branch1 branch2
   | manhattan (head branch1) destination + cost branch1 <= manhattan (head branch2) destination + cost branch2 = LT
   | otherwise = GT



-- GAMES
-- Computes the minimax value for the max player. Used in the function 'minimax'
maxValueMM :: Game -> Player -> Int
maxValueMM game player
   | terminal game = eval game
   | otherwise     = maximum ([minValueMM g (switch player) | g <- moves game player])
   -- Else get a list of new game states for every possible move and calculate minimax value

-- Computes the minimax value for the min player. Used in the function 'minimax'
minValueMM :: Game -> Player -> Int
minValueMM game player
   | terminal game = eval game
   | otherwise     = minimum ([maxValueMM g (switch player) | g <- moves game player])


-- Returns utility value for the maximizer player. Used in the function 'alphabeta'
maxValueAB:: Game -> Player -> (Int, Int) -> Int
maxValueAB game player (alpha, beta)
   | terminal game = eval game
   | otherwise     = snd $ foldr (\g (newAlpha, v) -> let newV = max v (minValueAB g 0 (newAlpha, beta)) in
                     -- If state with evaluation greater than beta found, no need to check the remaining nodes of the branch; else, compute values of new alpha and new v
                     if v >= beta then (newAlpha, v) else (max newAlpha newV, newV))
                     (alpha, -2) (moves game player) -- Value of v in maxValueAB is -2

-- Returns utility value for the minimizer player. Used in the function 'alphabeta'
minValueAB:: Game -> Player -> (Int, Int) -> Int
minValueAB game player (alpha, beta)
   | terminal game = eval game
   | otherwise     = snd $ foldr (\g (newBeta, v) -> let newV = min v (maxValueAB g 1 (alpha, newBeta)) in
                     -- If state with minimax value smaller than alpha found, ignore the remaining nodes of this branch.
                     if v <= alpha then (newBeta, v) else (min newBeta newV, newV))
                     (beta, 2) (moves game player)

-- Alphabeta utility value for tic tac toe wild. Used in 'alphabetaWild'
maxValueABWild:: Game -> Player -> (Int, Int) -> Int
maxValueABWild game player (alpha, beta)
   | terminal game = if evalWild game == 1 then (-1) else 0 -- Reward -1 if the last move before the win was made by the computer (min-player)
   | otherwise     = snd $ foldr (\g (newAlpha, v) -> let newV = max v (minValueABWild g player (newAlpha, beta)) in
                     if v >= beta then (newAlpha, v) else (max newAlpha newV, newV))
                     (alpha, -2) (movesWild game player)

-- Returns utility value for tic tac toe wild. Used in 'alphabetaWild'
minValueABWild:: Game -> Player -> (Int, Int) -> Int
minValueABWild game player (alpha, beta)
   | terminal game = if evalWild game == 1 then 1 else 0 -- Reward 1 if last move before win was made by human (max-player)
   | otherwise     = snd $ foldr (\g (newBeta, v) -> let newV = min v (maxValueABWild g player (alpha, newBeta)) in
                     if v <= alpha then (newBeta, v) else (min newBeta newV, newV))
                     (beta, 2) (movesWild game player)
