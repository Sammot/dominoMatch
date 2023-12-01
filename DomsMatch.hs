{- 
  DomsMatch: code to play a dominoes match between two players.

  Stub with types provided by Emma Norling (October 2023).

  Completed codebase by Sam Taseff (November 2023).
-}

module DomsMatch where
    import System.Random
    import Data.List
    import Data.Ord (comparing)

    import Data.Maybe (fromJust, isNothing)
    import Data.Set (toList, fromList)

    -- types used in this module
    type Domino = (Int, Int) -- a single domino
    {- Board data type: either an empty board (InitState) or the current state as represented by
        * the left-most domino (such that in the tuple (x,y), x represents the left-most pips)
        * the right-most domino (such that in the tuple (x,y), y represents the right-most pips)
        * the history of moves in the round so far
     -}
    data Board = InitState | State Domino Domino History deriving (Eq, Show)
    {- History should contain the *full* list of dominos played so far, from leftmost to
       rightmost, together with which player played that move and when they played it
     -}
    type History = [(Domino, Player, MoveNum)]
    data Player = P1 | P2 deriving (Eq, Show)
    data End = L | R deriving (Eq, Show)
    type Scores = (Int, Int) -- P1’s score, P2’s score
    type MoveNum = Int
    type Hand = [Domino]
    {- DomsPlayer is a function that given a Hand, Board, Player and Scores will decide
       which domino to play where. The Player information can be used to "remember" which
       moves in the History of the Board were played by self and which by opponent
     -}
    type DomsPlayer = Hand -> Board -> Player -> Scores -> (Domino, End)

    {- domSet: a full set of dominoes, unshuffled -}
    domSet = [ (l,r) | l <- [0..6], r <- [0..l] ]

    {- shuffleDoms: returns a shuffled set of dominoes, given a number generator
       It works by generating a random list of numbers, zipping this list together
       with the ordered set of dominos, sorting the resulting pairs based on the random
       numbers that were generated, then outputting the dominos from the resulting list.
     -}
    shuffleDoms :: StdGen -> [Domino]
    shuffleDoms gen = [ d | (r,d) <- sort (zip (randoms gen :: [Int]) domSet)]

    {- domsMatch: play a match of n games between two players,
        given a seed for the random number generator
       input: number of games to play, number of dominos in hand at start of each game,
              target score for each game, functions to determine the next move for each
              of the players, seed for random number generator
       output: a pair of integers, indicating the number of games won by each player
     -}
    domsMatch :: Int -> Int -> Int -> DomsPlayer -> DomsPlayer -> Int -> (Int, Int)
    domsMatch games handSize target p1 p2 seed
        = domsGames games p1 p2 (mkStdGen seed) (0, 0)
          where
          domsGames 0 _  _  _   wins               = wins
          domsGames n p1 p2 gen (p1_wins, p2_wins)
            = domsGames (n-1) p1 p2 gen2 updatedScore
              where
              updatedScore
                | playGame handSize target p1 p2 (if odd n then P1 else P2) gen1 == P1 = (p1_wins+1,p2_wins)
                | otherwise                                            = (p1_wins, p2_wins+1)
              (gen1, gen2) = split gen
              {- Note: the line above is how you split a single generator to get two generators.
                 Each generator will produce a different set of pseudo-random numbers, but a given
                 seed will always produce the same sets of random numbers.
               -}

    {- playGame: play a single game (where winner is determined by a player reaching
          target exactly) between two players
       input: functions to determine the next move for each of the players, player to have
              first go, random number generator 
       output: the winning player
     -}
    playGame :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> Player
    playGame handSize target p1 p2 firstPlayer gen
        = playGame' p1 p2 firstPlayer gen (0, 0)
          where
          playGame' p1 p2 firstPlayer gen (s1, s2)
            | s1 == target = P1
            | s2 == target = P2
            | otherwise   
                = let
                      newScores = playDomsRound handSize target p1 p2 firstPlayer currentG (s1, s2)
                      (currentG, nextG) = split gen
                  in
                  playGame' p1 p2 (if firstPlayer == P1 then P2 else P1) nextG newScores

    {- playDomsRound: given the starting hand size, two dominos players, the player to go first,
        the score at the start of the round, and the random number generator, returns the score at
        the end of the round.
        To complete a round, turns are played until either one player reaches the target or both
        players are blocked.
     -}
    playDomsRound :: Int -> Int -> DomsPlayer -> DomsPlayer -> Player -> StdGen -> (Int, Int) -> (Int, Int)
    playDomsRound handSize target p1 p2 first gen scores
        = playDomsRound' p1 p2 first (hand1, hand2, InitState, scores)
          where
          -- shuffle the dominoes and generate the initial hands
          shuffled = shuffleDoms gen
          hand1 = take handSize shuffled
          hand2 = take handSize (drop handSize shuffled)
          {- playDomsRound' recursively alternates between each player, keeping track of the game state
             (each player's hand, the board, the scores) until both players are blocked -}
          playDomsRound' p1 p2 turn gameState@(hand1, hand2, board, (score1,score2))
            | (score1 == target) || (score2 == target) || (p1_blocked && p2_blocked) = (score1,score2)
            | turn == P1 && p1_blocked = playDomsRound' p1 p2 P2 gameState
            | turn == P2 && p2_blocked = playDomsRound' p1 p2 P1 gameState
            | turn == P1               = playDomsRound' p1 p2 P2 newGameState
            | otherwise                = playDomsRound' p1 p2 P1 newGameState
              where
              p1_blocked = blocked hand1 board
              p2_blocked = blocked hand2 board
              (domino, end)          -- get next move from appropriate player
                  | turn == P1 = p1 hand1 board turn (score1, score2)
                  | turn == P2 = p2 hand2 board turn (score1, score2)
                                     -- attempt to play this move
              maybeBoard             -- try to play domino at end as returned by the player
                  | turn == P1 && not (elem domino hand1) = Nothing -- can't play a domino you don't have!
                  | turn == P2 && not (elem domino hand2) = Nothing
                  | otherwise = playDom turn domino board end
              newGameState           -- if successful update board state (exit with error otherwise)
                 | maybeBoard == Nothing = error ("Player " ++ show turn ++ " attempted to play an invalid move.")
                 | otherwise             = (newHand1, newHand2, newBoard,
                                              (limitScore score1 newScore1, limitScore score2 newScore2))
              (newHand1, newHand2)   -- remove the domino that was just played
                 | turn == P1 = (hand1\\[domino], hand2)
                 | turn == P2 = (hand1, hand2\\[domino])
              score = scoreBoard newBoard (newHand1 == [] || newHand2 == [])
              (newScore1, newScore2) -- work out updated scores
                 | turn == P1 = (score1+score,score2)
                 | otherwise  = (score1,score2+score)
              limitScore old new     -- make sure new score doesn't exceed target
                 | new > target = old
                 | otherwise    = new
              Just newBoard = maybeBoard -- extract the new board from the Maybe type

    scoreBoard :: Board -> Bool -> Int
    scoreBoard InitState _ = 0
    scoreBoard (State (l1, l2) (r1, r2) _) last
      -- i could check if the history only has one element but that breaks otherwise correct tests that omit the history
      | (l1 == r1) && (l2 == r2)  = (scoreBoard' l1 r2) + final -- only one domino has been played
      | otherwise                 = (scoreBoard' l r)   + final
      where
        final = if last then 1 else 0
        l = if l1 == l2 then l1 + l2 else l1
        r = if r1 == r2 then r1 + r2 else r2

        scoreBoard' l r 
          | tMod5 + tMod3 == 0  = num3s + num5s
          | tMod5 == 0          = num5s
          | tMod3 == 0          = num3s
          | otherwise           = 0
            where
              t = l + r
              tMod3 = t `mod` 3
              num3s = t `div` 3
              tMod5 = t `mod` 5
              num5s = t `div` 5

    blocked :: Hand -> Board -> Bool
    blocked _ InitState = False -- You can place any domino to start the game
    blocked xs (State (a, _) (_, b) _) = not((a `elem` hand') || (b `elem` hand'))
      where
        -- flatten the hand array into pip values
        hand' = foldr (\x acc -> (fst x):(snd x):acc) [] xs :: [Int]

    canPlay :: Domino -> End -> Board -> Bool
    canPlay _ _ InitState = True
    canPlay (x, y) e (State (l, _) (_, r) _)
      | e == L = (x == l) || (y == l)
      | e == R = (x == r) || (y == r)

    orientDomino :: Int -> End -> Domino -> Domino
    orientDomino a d (x, y) -- a is which value you want last, from the perspective of d
      | a == x    = if d == L then (y, x) else (x, y)
      | a == y    = if d == L then (x, y) else (y, x)
      | otherwise = error "Number not on either side of the domino"

    playDom :: Player -> Domino -> Board -> End -> Maybe Board
    playDom p d InitState _ = Just (State d d [(d, p, 0)]) -- First domino, if it is both left and right it is essentially one domino
    playDom p dom@(x, y) board@(State ld@(l, _) rd@(_, r) xs) e 
      | canPlay dom e board = 
        if e == L then
            Just (State oriented' rd history')
        else -- e == R
            Just (State ld oriented' history')
      | otherwise = Nothing
      where
        moveNum = length xs
        oriented' = orientDomino (if e == L then l else r) (if e == L then L else R) dom
        history'  = 
          if e == L then
            [(oriented', p, moveNum)] ++ xs :: History
          else
            xs ++ [(oriented', p, moveNum)] :: History

    possPlays :: Hand -> Board -> (Hand, Hand)
    possPlays [] _ = ([], [])
    possPlays h InitState = (h, h)
    possPlays h s@(State (l, _) (_, r) _) = (a, b)
      where
        a = filter (\d -> canPlay d L s) h :: Hand
        b = filter (\d -> canPlay d R s) h :: Hand

    -- type DomsPlayer = Hand -> Board -> Player -> Scores -> (Domino, End)
    simplePlayer :: DomsPlayer
    simplePlayer [] InitState _ _     = error "No board, no hand = nothing game"
    simplePlayer [] (State _ _ _) _ _ = error "Cannot play a domino with no hand"
    simplePlayer (h:hs) InitState _ _ = (h, L)
    simplePlayer hs s@(State (l, _) (_, r) _) _ _ = if (not (null leftMoves)) then (head leftMoves, L) else (head rightMoves, R)
      where
        (leftMoves, rightMoves) = possPlays hs s

    -- will be ran ~10,000 times = about 200 seconds runtime in GHCi
    -- Plays the best domino while considering what opportunities the opponent gains, and considers who is close to winning and acts accordingly (going for the win / blocking)
    smartPlayer :: DomsPlayer
    smartPlayer h b p s = smartPlayer' 1 (-0.2) 2.1 0.3 61 h b p s

    -- domino scoring weight1, domino scoring weight2, winSensitivity, lossSensitivity, targetScore, {DomsPlayer}
    smartPlayer' :: Float -> Float -> Float -> Float -> Int -> DomsPlayer
    smartPlayer' w1 w2 winSensitivity lossSensitivity targetScore h b p s
      -- I am close to winning? -> play a winning domino, if there is no winning play, make the opponent knock
      | selfAlmostWon = if (isNothing winningPlay) then blockOpponent else (fromJust winningPlay)
      -- are they are close to winning? -> make them knock
      | opponentAlmostWon = blockOpponent 
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
        blockOpponent = blockingPlay h b domSet p :: (Domino, End)
        
    theBestPlay :: Float -> Float -> Hand -> Board -> Player -> [Domino] -> (Domino, End)
    theBestPlay w1 w2 h b p dominoSet = theBestPlay'
      where
        possiblePlays = (l ++ r) :: [Domino] 
          where (l, r) = (possPlays h b)
        -- what is the best end to play each possible play on
        orientedPlays = map (\d -> let (e, s) = (bestEndToPlay d b) in (d, e, s)) possiblePlays :: [(Domino, End, Int)]
        bestResponsePlays = map (\(d, e, s) -> bestResponsePlay h b d e p dominoSet) orientedPlays :: [(Maybe (Domino, End, Int))]

        orientedPlays_WeightedScore = map (\(d, e, s) -> (d, e, (fromIntegral s) * w1)) orientedPlays :: [(Domino, End, Float)]
        bestResponsePlays_WeightedScore = map bestResponsePlaysWeightedScore' bestResponsePlays :: [Float]
          where
            bestResponsePlaysWeightedScore' (Just p@(d, e, s)) = (fromIntegral s) * w2 :: Float
            -- if this play results in no possible play for the opponent, it must be pretty good
            bestResponsePlaysWeightedScore' Nothing = 10 * (-(abs w2))

        -- Each play with its final score
        weightedPlays = map (\((d1, e1, s1), s2) -> (d1, e1, s1 + s2)) (zip orientedPlays_WeightedScore bestResponsePlays_WeightedScore) :: [(Domino, End, Float)]

        theBestPlay' = (d, e) :: (Domino, End)
          where
            (d, e, s) = foldr (\dom@(d, e, s) acc@(accd, acce, accs) -> if s > accs then dom else acc) (head weightedPlays) weightedPlays :: (Domino, End, Float)

    -- if there is a play that can achieve the given targetScore from the current score, this function returns that domino and the end to play it on
    winningDomino :: Hand -> Board -> Int -> Int -> Maybe (Domino, End)
    winningDomino hand board selfScore targetScore
      | length winningPlays == 0 = Nothing
      | otherwise = (Just (transFormPlay (head winningPlays)))
      where
        requiredScore = targetScore - selfScore :: Int
        possiblePlays = possPlays hand board    :: (Hand, Hand)
        leftPlays = map (\d -> (d, fromJust (playDom P1 d board L), L)) (fst possiblePlays)   :: [(Domino, Board, End)]
        rightPlays = map (\d -> (d, fromJust (playDom P1 d board R), R)) (snd possiblePlays)  :: [(Domino, Board, End)]
        plays = leftPlays ++ rightPlays
        winningPlays = filter (\(d, b, e) -> (scoreBoard b False) == requiredScore) plays

        transFormPlay :: (Domino, Board, End) -> (Domino, End)
        transFormPlay (d, b, e) = (d, e)

    -- params: self's hand, the board state, the set of all dominoes, and the self player
    -- returns: the play most likely to block the opponent player
    blockingPlay :: Hand -> Board -> [Domino] -> Player -> (Domino, End)
    blockingPlay hand board dominoSet player = (dom, end)
      where
        (leftPlays, rightPlays) = (possPlays hand board) :: (Hand, Hand)
        allPlays = (addEnd leftPlays L) ++ (addEnd rightPlays R)
          where addEnd ps e = zip ps (repeat e)
          
        numResponsePlays = map (\(dom, e) -> (dom, e, length (possibleResponsePlays hand board dom e player dominoSet))) allPlays :: [(Domino, End, Int)]
        numResponsePlaysSorted = sortBy (\(_, _, l1) (_, _, l2) -> compare l1 l2) numResponsePlays
        (dom, end, _) = head numResponsePlaysSorted

    -- returns: the highest scoring End to play each domino on with the points gained (expects possible plays only)
    bestEndToPlay :: Domino -> Board -> (End, Int)
    bestEndToPlay d b 
      | (ls == rs) && rs == -1 = error "Impossible domino"
      | otherwise = s
      where
        ls = if (canPlay d L b) then (scoreBoard (fromJust (playDom P1 d b L)) False) else -1 :: Int
        rs = if (canPlay d R b) then (scoreBoard (fromJust (playDom P1 d b R)) False) else -1 :: Int
        s = if ls >= rs then (L, ls) else (R, rs)

    -- Takes a pip value and a domino and returns a tuple representing if you can achieve that pip value on the outer left or the right sides of the board with this domino
    canPlayWithPip :: Int -> Board -> Domino -> (Bool, Bool)
    canPlayWithPip pip InitState (l, r) = (same, same)
      where same = (r == pip) || (l == pip)
    canPlayWithPip pip board dom = (left, right)
      where
        canPlayLeft = canPlay dom L board
        canPlayRight = canPlay dom R board
        (Just playedLeft) = playDom P1 dom board L
        (Just playedRight) = playDom P1 dom board R
        left = if canPlayLeft then (fst (leftDom playedLeft)) == pip else False
        right = if canPlayRight then (snd (rightDom playedRight)) == pip else False

    -- takes a history list and flattens it into a chronological list of domino plays from which the game can be replayed to certain states
    -- the first domino played is assumed to have been played on the Left side
    sequentialHistory :: History -> [(Domino, End, Player, MoveNum)]
    sequentialHistory hs = sequentialHistory' hs []
    sequentialHistory' :: History -> [(Domino, End, Player, MoveNum)] -> [(Domino, End, Player, MoveNum)]
    sequentialHistory' hs xs
      | null hs = xs
      | otherwise = sequentialHistory' (filter (/=(fst lastMove)) hs) nextStep
      where
        leftDom@(ld, lp, lm)  = head hs :: (Domino, Player, MoveNum)
        rightDom@(rd, rp, rm) = last hs :: (Domino, Player, MoveNum)
        lastMove = if lm >= rm then (leftDom, L) else (rightDom, R) :: ((Domino, Player, MoveNum), End)

        transFormPlay :: (Domino, Player, MoveNum) -> End -> (Domino, End, Player, MoveNum)
        transFormPlay (d, p, s) e = (d, e, p, s) 

        nextStep = [transFormPlay (fst lastMove) (snd lastMove)] ++ xs :: [(Domino, End, Player, MoveNum)]

    -- takes a sequential list of the game history (assumed to be valid) and builds the board state from it
    buildBoardState :: [(Domino, End, Player, MoveNum)] -> Board
    buildBoardState sequentialHist = foldl' (\acc (d, e, p, m) -> fromJust (playDom p d acc e)) InitState sequentialHist

    -- given the self player and a board, this function returns every pip value the opponent had to knock for
    opponentKnocks :: Player -> Board -> [Int]
    opponentKnocks _ InitState = []
    opponentKnocks p (State _ _ hs) 
      | length hs < 2 = []
      | otherwise = weakPips
      where
        flatHistory = sequentialHistory hs :: [(Domino, End, Player, MoveNum)] 
        knockingMoves = snd (foldl' (\((_, _, pp, pm), knocks) move@(_, _, cp, cm) -> if (cp == pp) && (cp == p) then (move, pm:knocks) else (move, knocks)) (head flatHistory, []) (tail flatHistory)) :: [MoveNum]

        weakPips = foldr (\moveNum acc -> let b = buildBoardState (takeWhile (\(_, _, _, m) -> m /= moveNum + 1) flatHistory) in (snd (rightDom b)):(fst (leftDom b)):acc) [] knockingMoves :: [Int]

    -- params: a hand, a board state, the set of all dominoes, the self player
    -- given a board and your hand, calculates all possible the dominoes the opponent COULD have
    opponentsPossibleDominoes :: Hand -> Board -> [Domino] -> Player -> Hand
    opponentsPossibleDominoes hs b dominoSet p = filter (\x@(l, r) -> not ((l `elem` knocks) || (r `elem` knocks) || (x `elem` hs) || (x `elem` dominoesOnTheBoard))) dominoSet
      where
        history = getHistory b    :: History
        knocks = opponentKnocks p b
        dominoesOnTheBoard = map (\(d, p, m) -> d) history :: Hand

    -- plays the domino, then determines the best possibleOpponentPlay
    bestResponsePlay :: Hand -> Board -> Domino -> End -> Player -> [Domino] -> (Maybe (Domino, End, Int))
    bestResponsePlay h b d e p dominoSet
      | length prp's /= 0 =
        let bestScore = (foldr getBestScore (head prp's) prp's) :: (Domino, End, Int)
        in (Just bestScore)
      | otherwise = Nothing
      where
        prp's = possibleResponsePlays h b d e p dominoSet :: [(Domino, End, Int)]

        -- gets the highest scoring play from a list
        getBestScore :: (Domino, End, Int) -> (Domino, End, Int) -> (Domino, End, Int)
        getBestScore d1@(dom, end1, score) acc@(bestDom, end2, bestScore) = if score > bestScore then d1 else acc

    -- params: a hand, a board, a domino from the hand, an end to play it on, the player to do so, the set of all dominoes
    -- plays the domino, then returns all the possibleOpponentPlays
    possibleResponsePlays :: Hand -> Board -> Domino -> End -> Player -> [Domino] -> [(Domino, End, Int)]
    possibleResponsePlays h b d e p dominoSet = possibleOpponentPlays (filter (\x -> x /= d) h) (fromJust (playDom p d b e)) dominoSet p

    -- Self's hand and the board state -> [The dominoes the opponent COULD play with their end and resultant score]
    possibleOpponentPlays :: Hand -> Board -> [Domino] -> Player -> [(Domino, End, Int)]
    possibleOpponentPlays hs board dominoSet p = rankedPlays
      where
        possibleDominoes = opponentsPossibleDominoes hs board dominoSet p :: Hand

        canPlayTheDomino :: Domino -> Bool
        canPlayTheDomino x = (canPlay x L board) || (canPlay x R board)

        possiblePlays = [(x, fst (bestEndToPlay x board)) | x <- possibleDominoes, canPlayTheDomino x] :: [(Domino, End)]

        -- meant for heuristic scoring of each domino rather than a true score
        dominoScore :: Domino -> End -> Int
        dominoScore x e = scoreBoard (fromJust (playDom P1 x board e)) False

        rankedPlays = map (\(x, e) -> (x, e, (dominoScore x e))) possiblePlays :: [(Domino, End, Int)]

    -- UTILITY FUNCTIONS

    -- getter operations for a 3-tuple
    fst3 :: (a, b, c) -> a
    fst3 (a, _, _) = a
    snd3 :: (a, b, c) -> b
    snd3 (_, b, _) = b
    lst3 :: (a, b, c) -> c
    lst3 (_, _, c) = c

    -- removes all duplicate elements from a list
    removeCopies :: Ord a => [a] -> [a]
    removeCopies = toList.fromList -- to a set then back again

    -- first index in a list that matches the predicate
    indexWhere :: Eq a => (a -> Bool) -> [a] -> Int
    indexWhere f = length.takeWhile (not.f) 

    -- counts all occurances of a value in a list
    occurances :: Eq a => a -> [a] -> Int
    occurances x = length.filter (==x)

    -- GAME UTILITY FUNCTIONS

    -- gets the left domino from a board state
    leftDom :: Board -> Domino
    leftDom InitState = error "No dominoes here"
    leftDom (State l _ _) = l

    -- gets the right domino from a board state
    rightDom :: Board -> Domino
    rightDom InitState = error "No dominoes here"
    rightDom (State _ r _) = r

    -- gets the score for the given player
    getScore :: Scores -> Player -> Int
    getScore (l, _) P1 = l
    getScore (_, r) P2 = r

    -- gets your opponent
    opponent :: Player -> Player
    opponent P1 = P2
    opponent P2 = P1

    -- gets the history from a board state
    getHistory :: Board -> History
    getHistory InitState = []
    getHistory (State _ _ h) = h

    -- gets the average pip value from a list of dominoes
    getAveragePip :: [Domino] -> Float
    getAveragePip [] = error "Empty list"
    getAveragePip ds = (fromIntegral (sum (foldr (\(l, r) acc -> l:r:acc) [] ds))) / (fromIntegral (length ds) * 2)