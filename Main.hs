module Main where
    import DomsMatch

    main :: IO ()
    main = do
        print("A game will be played between the two players defined in DomsMatch.hs")
        print(domsMatch 1000 7 61 simplePlayer smartPlayer 2340)
