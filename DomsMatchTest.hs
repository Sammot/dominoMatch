
module DomsMatchTest where
    import DomsMatch

    import Data.Maybe (fromJust, isNothing)

    -- Base functions

    scoreBoard_Test :: [Bool]
    scoreBoard_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            (scoreBoard InitState False,                0),
            (scoreBoard (State (2, 2) (2, 1) []) True,  2),
            (scoreBoard (State (6, 1) (1, 4) []) True,  3),
            (scoreBoard (State (5, 3) (3, 10) []) False,8),
            (scoreBoard (State (2, 4) (4, 0) []) False, 0),
            (scoreBoard (State (0, 0) (0, 0) []) False, 0),
            (scoreBoard (State (6, 6) (2, 3) []) False, 8),
            (scoreBoard (State (3, 3) (3, 3) []) False, 2)
          ] :: [(Int, Int)]

    blocked_Test :: [Bool]
    blocked_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            (blocked [(1,2), (7,3), (8,3)] InitState,              False),
            (blocked [(0,4), (3,9), (8,3)] (State (6,1) (1,5) []), True),
            (blocked []                    (State (6,1) (1,5) []), True),
            (blocked [(0,4), (3,9), (8,3)] (State (3,1) (1,4) []), False),
            (blocked [(0,4), (5,9), (8,3)] (State (6,1) (1,5) []), False),
            (blocked [(6,4), (5,6), (8,3)] (State (6,1) (1,2) []), False),
            (blocked [(0,4), (5,2), (8,3)] (State (2,1) (1,2) []), False)
          ] :: [(Bool, Bool)]

    canPlay_Test :: [Bool]
    canPlay_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            (canPlay (4,2) L InitState,               True),
            (canPlay (5,2) L (State (3,3) (4,5) []),  False),
            (canPlay (5,2) R (State (3,3) (4,5) []),  True),
            (canPlay (2,5) R (State (3,3) (4,5) []),  True),
            (canPlay (2,1) R (State (6,1) (2,3) []),  False),
            (canPlay (2,1) L (State (6,1) (2,3) []),  False)
          ] :: [(Bool, Bool)]

    orientDomino_Test :: [Bool]
    orientDomino_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            (orientDomino 4 L (4,2), (2,4)),
            (orientDomino 4 R (4,2), (4,2)),
            (orientDomino 2 L (4,2), (4,2)),
            (orientDomino 2 R (4,2), (2,4))
            -- (orientDomino 8 R (4,2), (2,4)) The test which raises an exception is done separetly
          ] :: [(Domino, Domino)]

    playDom_Test :: [Bool] --[Maybe Board]
    playDom_Test = map (\(output, expected) -> output == expected) tests -- 
      where
        tests = 
          [
            let
              d = (5,4) :: Domino
              p = P1
              board = playDom p d InitState L
              res = Just (State d d [(d, p, 0)])
              in (board, res),
            let
              d = (5,4) :: Domino
              p = P1
              board = playDom p d (State (3,2) (2,4) []) L
              res = Nothing
              in (board, res),
            let
              d = (2,1) :: Domino
              p = P1
              board = playDom p d (State (2,0) (0,1) [((2,0), P1, 0),((0,1), P2, 1)]) R
              res = Just (State (2,0) (1,2) [((2,0), P1, 0),((0,1), P2, 1),((1,2), P1, 2)])
              in (board, res),
            let
              d = (2,1) :: Domino
              p = P1
              board = playDom p d (State (2,0) (0,1) [((2,0), P1, 0),((0,1), P2, 1)]) L
              res = Just (State (1,2) (0,1) [((1,2), P1, 2),((2,0), P1, 0),((0,1), P2, 1)])
              in (board, res)

          ] :: [(Maybe Board, Maybe Board)]

    possPlays_Test :: [Bool]
    possPlays_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            let 
              (d1, d2) = ((1,2),(3,5))
              hand = [d1,d2]
              in (possPlays hand InitState, (hand, hand)),
            let 
              (d1, d2, d3) = ((1,2),(3,5),(4,1))
              hand = [d1,d2,d3]
              in (possPlays hand (State (1,6) (4,3) []), ([(1,2), (4,1)], [(3,5)])),
            let 
              (d1, d2, d3) = ((6,4),(4,5),(4,1))
              hand = [d1,d2,d3]
              in (possPlays hand (State (6,6) (4,4) []), ([(6,4)],[(6,4),(4,5),(4,1)])),
            let 
              (d1, d2, d3) = ((6,4),(4,5),(4,1))
              hand = [d1,d2,d3]
              in (possPlays hand (State (0,6) (4,0) []), ([], [])),
            let 
              (d1, d2, d3) = ((6,4),(4,5),(4,1))
              hand = [d1,d2,d3]
              in (possPlays hand (State (0,6) (3,1) []), ([], [(4,1)]))

          ] :: [((Hand, Hand), (Hand, Hand))]

    -- My functions

    getAveragePip_Test :: [Bool]
    getAveragePip_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            (getAveragePip [(4, 4), (4, 4), (4, 4)], 4.0),
            (getAveragePip [(6, 5), (5, 4), (6, 4)], 5.0),
            (getAveragePip [(6, 5), (1, 1), (6, 2)], 3.5)
          ] :: [(Float, Float)]

    sequentialHistory_Test :: [Bool]
    sequentialHistory_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            let history = [((1,5), P1, 2), ((5,4), P1, 0), ((4,4), P2, 1)]
            in (sequentialHistory history, [((5,4), L, P1, 0), ((4,4), R, P2, 1), ((1,5), L, P1, 2)]),
            let history = [((5,4), P1, 0), ((4,4), P2, 1), ((4,5), P1, 2)]
            in (sequentialHistory history, [((5,4), L, P1, 0), ((4,4), R, P2, 1), ((4,5), R, P1, 2)]),
            let history = [((0,3), P1, 2), ((3,1), P2, 1), ((1,1), P1, 0)]
            in (sequentialHistory history, [((1,1), L, P1, 0), ((3,1), L, P2, 1), ((0,3), L, P1, 2)])

          ] :: [([(Domino, End, Player, MoveNum)], [(Domino, End, Player, MoveNum)])]

    buildBoardState_Test :: [Bool]
    buildBoardState_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            let board = (State (1,5) (4,4) [((1,5), P1, 2), ((5,4), P1, 0), ((4,4), P2, 1)])
            in ((buildBoardState.sequentialHistory.getHistory) board, board),
            let board = (State (5,4) (4,5) [((5,4), P1, 0), ((4,4), P2, 1), ((4,5), P1, 2)])
            in ((buildBoardState.sequentialHistory.getHistory) board, board),
            let board = (State (0,3) (1,1) [((0,3), P1, 2), ((3,1), P2, 1), ((1,1), P1, 0)])
            in ((buildBoardState.sequentialHistory.getHistory) board, board) 
          ] :: [(Board, Board)]

    opponentKnocks_Test :: [Bool]
    opponentKnocks_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            -- no one has knocked yet
            (opponentKnocks P1 (State (3,4) (4,5) [((3,4), P1, 2), ((4,4), P1, 0), ((4,5), P2, 1)]), []),
            (opponentKnocks P2 (State (3,4) (4,5) [((3,4), P1, 2), ((4,4), P1, 0), ((4,5), P2, 1)]), []),

            -- we both have knocked
            (opponentKnocks P1 (State (0,3) (5,2) [((0,3), P1, 3), ((3,4), P2, 2), ((4,4), P1, 0), ((4,5), P2, 1), ((5,2), P1, 4)]), [5,0]),
            (opponentKnocks P2 (State (0,3) (5,2) [((0,3), P1, 3), ((3,4), P2, 2), ((4,4), P1, 0), ((4,5), P2, 1), ((5,2), P1, 4)]), [5,4]),
            
            -- only one person knocked
            (opponentKnocks P1 (State (0,3) (5,2) [((3,4), P2, 2), ((4,4), P1, 0), ((4,5), P2, 1)]), []),
            (opponentKnocks P2 (State (0,3) (5,2) [((3,4), P2, 2), ((4,4), P1, 0), ((4,5), P2, 1)]), [5,4])
            
          ] :: [([Int], [Int])]

    opponentsPossibleDominoes_Test :: [Bool]
    opponentsPossibleDominoes_Test = tests
      where
        tests = 
          [
            -- What could P2 have if they havent knocked at all
            let
              hand = [(1,1),(1,0),(0,0),(2,2)]
              board = (State (0,3) (5,2) [((0,3), P2, 3), ((3,4), P1, 2), ((4,4), P1, 0), ((4,5), P2, 1), ((5,2), P1, 4)])
              answer = filter (\d -> not ((d `elem` hand) || (d `elem` (map (\(d, p, m) -> d) (getHistory board))))) domSet
            in ((opponentsPossibleDominoes hand board domSet P1) == answer),
            -- What could P2 have if they have knocked
            let
              hand = [(1,1),(1,0),(0,0),(2,2)]
              boardKnocks = (State (0,3) (5,2) [((0,3), P1, 3), ((3,4), P2, 2), ((4,4), P1, 0), ((4,5), P2, 1), ((5,2), P1, 4)])
              answer = filter (\d -> not ((d `elem` hand) || (d `elem` (map (\(d, p, m) -> d) (getHistory boardKnocks))))) domSet
            in (length (opponentsPossibleDominoes hand boardKnocks domSet P1) < length answer),
            -- No dominoes played yet
            let
              hand = [(1,1),(1,0),(0,0),(2,2)]
              answer = filter (\d -> not ((d `elem` hand))) domSet
            in (opponentsPossibleDominoes hand InitState domSet P1) == answer,
            -- No dominoes played, no dominoes in hand
            (opponentsPossibleDominoes [] InitState domSet P1) == domSet
          ] :: [Bool]

    winningDomino_Test :: [Bool]
    winningDomino_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            -- no winning domino
            (winningDomino [(3,0)]                (State (0,1) (5,4) []) 30 33, Nothing),
            -- a winning play with other valid plays
            (winningDomino [(3,0), (5,0), (0,0)]  (State (0,1) (5,4) []) 30 33, Just ((5,0), L)),
            -- multiple winning plays including a double, 1
            (winningDomino [(3,3), (5,0), (0,3)]  (State (0,1) (5,3) []) 30 32, Just ((0,3), L)),
            -- multiple winning plays including a double, 2
            (winningDomino [(3,3), (5,0)]         (State (0,1) (5,3) []) 30 32, Just ((3,3), R))
          ] :: [(Maybe (Domino, End), Maybe (Domino, End))]

    canPlayWithPip_Test :: [Bool]
    canPlayWithPip_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            -- start of the game
            (canPlayWithPip 1 (InitState) (1,0), (True, True)),
            -- start of the game 2
            (canPlayWithPip 0 (InitState) (1,0), (True, True)),
            -- cannot play on either side
            (canPlayWithPip 0 (State (0,3) (4,0) []) (1,0), (False, False)),
            -- can play on both sides
            (canPlayWithPip 1 (State (0,3) (4,0) []) (1,0), (True, True)),
            -- can play only on one side
            (canPlayWithPip 1 (State (0,3) (4,1) []) (1,0), (True, False)),
            -- can play only on one side
            (canPlayWithPip 4 (State (0,3) (4,1) []) (1,4), (False, True))
          ] :: [((Bool, Bool),(Bool, Bool))]

    bestEndToPlay_Test :: [Bool]
    bestEndToPlay_Test = map (\(output, expected) -> output == expected) tests
      where
        tests = 
          [
            -- start of the game (will always prefer L)
            (bestEndToPlay (2,3) InitState, (L, 1)),
            -- two possible ends
            (bestEndToPlay (2,3) (State (3,3) (1,2) []), (R, 3)),
            -- only one possible end
            (bestEndToPlay (2,3) (State (3,3) (0,1) []), (L, 1))
          ] :: [((End, Int), (End, Int))]

    runAllTests :: [(String, [Bool])]
    runAllTests =
      [
        ("scoreBoard_Test",               scoreBoard_Test),
        ("blocked_Test",                  blocked_Test),
        ("canPlay_Test",                  canPlay_Test),
        ("orientDomino_Test",             orientDomino_Test),
        ("playDom_Test",                  playDom_Test),
        ("possPlays_Test",                possPlays_Test),
        ("getAveragePip_Test",            getAveragePip_Test),
        ("sequentialHistory_Test",        sequentialHistory_Test),
        ("buildBoardState_Test",          buildBoardState_Test),
        ("opponentKnocks_Test",           opponentKnocks_Test),
        ("opponentsPossibleDominoes_Test",opponentsPossibleDominoes_Test),
        ("winningDomino_Test",            winningDomino_Test),
        ("canPlayWithPip_Test",           canPlayWithPip_Test),
        ("bestEndToPlay_Test",            bestEndToPlay_Test)
      ]

    printTest :: (String, [Bool]) -> IO ()
    printTest (name, units) = do
      putStrLn name
      putStrLn ("\t" ++ (unitsToString units))
      putStrLn ""
      where
        unitsToString :: [Bool] -> String
        unitsToString units = (foldl (\acc t -> let str = if t then "Pass, " else "Fail, " in acc ++ str) "[ " units) ++ "]"

    integrationTest :: IO ()
    integrationTest = do
      putStrLn ""
      (sequence_ (map printTest failed))
      if (length failed == 0) then putStrLn "All Tests Passing" else putStrLn "Some Tests Failed"
      where 
        tests = runAllTests
        failed = filter (\(_, units) -> length (filter (==False) units) /= 0) tests

    printAllTests :: IO ()
    printAllTests = do 
      sequence_ (map printTest runAllTests) -- for_ (map printTest runAllTests) do print

    -- Player development

    -- Only plays the best domino from its own hand
    smartPlayer_0 :: DomsPlayer
    smartPlayer_0 h b p s = theBestPlay
        where
        possiblePlays = l ++ r where (l, r) = (possPlays h b) :: ([Domino], [Domino])
        orientedPlays = map (\d -> let (e, s) = (bestEndToPlay d b) in (d, e, s)) possiblePlays :: [(Domino, End, Int)]
        
        theBestPlay = (d, e)
            where
            (d, e, s) = foldr (\dom@(d, e, s) acc@(accd, acce, accs) -> if s > accs then dom else acc) (head orientedPlays) orientedPlays

    -- Plays the best domino while also considering what opportunities it gives to the opponent
    smartPlayer_1 :: DomsPlayer
    smartPlayer_1 h b p s = smartPlayer_1' 1 (-0.2) h b p s
    smartPlayer_1' :: Float -> Float -> DomsPlayer
    smartPlayer_1' w1 w2 h b p s = theBestPlay
        where
        possiblePlays = l ++ r where (l, r) = (possPlays h b) :: ([Domino], [Domino])
        orientedPlays = map (\d -> let (e, s) = (bestEndToPlay d b) in (d, e, s)) possiblePlays :: [(Domino, End, Int)]

        bestResponsePlays = map (\(d, e, s) -> bestResponsePlay h b d e p domSet) orientedPlays :: [(Maybe (Domino, End, Int))]

        orientedPlays_WeightedScore = map (\(d, e, s) -> (d, e, (fromIntegral s) * w1)) orientedPlays :: [(Domino, End, Float)]
        bestResponsePlays_WeightedScore = map bestResponsePlaysWeightedScore' bestResponsePlays :: [(Domino, End, Float)]
            where
            bestResponsePlaysWeightedScore' (Just p@(d, e, s)) = (d, e, (fromIntegral s) * w2) :: (Domino, End, Float)
            bestResponsePlaysWeightedScore' Nothing = ((-1, -1), L, 10 * (-w2))
            -- if it messes up the opponent it must be pretty good [THIS SHOULD DEPEND ON THE COMPARED SCORES, COULD BE BAD]

        weightedPlays = map (\((d1, e1, s1), (_, _, s2)) -> (d1, e1, s1 + s2)) (zip orientedPlays_WeightedScore bestResponsePlays_WeightedScore) :: [(Domino, End, Float)]

        theBestPlay = (d, e)
            where
            (d, e, s) = foldr (\dom@(d, e, s) acc@(accd, acce, accs) -> if s > accs then dom else acc) (head weightedPlays) weightedPlays :: (Domino, End, Float)

    steppedRange :: Float -> Float -> Float -> [Float]
    steppedRange start steps end = [x | i <- [0..steps], let x = (i / steps * (end - start)) + start]

    -- attempts a range of values for the second weight value of the player and calculates the win rate with that weight
    testSmartPlayerWeight2 :: [Float] -> [(Float, Float)] --w2 = -0.2
    testSmartPlayerWeight2 range = scores
        where
        -- weight2s = steppedRange (-1) 10 0 :: [Float]
        game = domsMatchAverageWinrate 50 7 61 smartPlayer_0
        scores = map (\w2 -> let outcome = game (smartPlayer_1' 1.0 w2) in (w2, outcome)) range :: [(Float, Float)]

    -- how much p2 wins against p1
    domsMatchAverageWinrate :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> Float
    domsMatchAverageWinrate numGames handSize target p1 p2 = a
        where
        (s1, s2) = domsMatchRandomizedAverage numGames handSize target p1 p2
        a = ((s2 - s1) / (s1 + s2)) * 100

    -- domsMatch games handSize target p1 p2
    domsMatchRandomizedAverage :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> (Float, Float)
    domsMatchRandomizedAverage numGames handSize target p1 p2 = a
      where
        seeds = [0..10] :: [Int]
        games = [x s | s <- seeds, let x = (domsMatch numGames handSize target p1 p2)] :: [Scores]
        (t1, t2) = foldr (\((s1, s2) :: Scores) ((a1, a2) :: Scores) -> (s1 + a1, s2 + a2)) (0, 0) games :: Scores
        l = (fromIntegral (length games)) :: Float
        a = ((fromIntegral t1) / l, (fromIntegral t2) / l)


    smartPlayer_FM :: DomsPlayer
    smartPlayer_FM h b p s = smartPlayer_FM' 1 (-0.2) 2.1 1 61 h b p s

    -- playScoring{weight1, weight2}, winWeight, lossWeight, DomsPlayer, targetScore
    smartPlayer_FM' :: Float -> Float -> Float -> Float -> Int -> DomsPlayer
    smartPlayer_FM' w1 w2 winSensitivity lossSensitivity targetScore h b p s
      -- I am close to winning? -> play a winning domino, if there is no winning play, make the opponent knock
      | selfAlmostWon = if (isNothing winningPlay) then simplePlayer h b p s else (fromJust winningPlay)
      -- any other situation? -> play domino with highest score { = (points gained) * w1 - (best possible opponent score if played) * w2}
      | otherwise = theBestPlay w1 w2 h b p domSet
      where
        -- game progress calculations
        (selfAlmostWon, opponentAlmostWon) = 
          (targScore - selfScore < averagePipsInHand * winSensitivity, 
           targScore - oppScore < averagePipsInOppHand * lossSensitivity) :: (Bool, Bool)
          where
            targScore = fromIntegral targetScore    :: Float
            selfScore = fromIntegral (getScore s p) :: Float
            averagePipsInHand = getAveragePip h     :: Float
            oppScore = fromIntegral (getScore s (opponent p)) :: Float
            averagePipsInOppHand = getAveragePip (opponentsPossibleDominoes h b domSet p) :: Float

        -- strategies calculations: lazy evaluation for storing the results
        winningPlay = winningDomino h b (getScore s p) targetScore :: Maybe (Domino, End)

    simplePlayer_BestDom :: DomsPlayer
    simplePlayer_BestDom [] InitState _ _     = error "No board, no hand = nothing game"
    simplePlayer_BestDom [] (State _ _ _) _ _ = error "Cannot play a domino with no hand"
    simplePlayer_BestDom h b p _ = theBestPlay 1 (-0.2) h b p domSet
