# dominoMatch - My functional programming assignment

I completed this piece of work for COM2108, a module in which I studied the functional programming paradigm and learnt Haskell. I had a lot of fun making this and developed a love for list processing!

This program simulates a match of 5s and 3s dominoes, and includes two players - one that randomly plays dominoes and one that employs multiple intelligent strategies to win.

It can be best demonstrated in the GHCi REPL as below:

```haskell
ghci> :load DomsMatch.hs
...
ghci> domsMatch <amount of games> <hand size> <winning score> simplePlayer smartPlayer <random seed value>
ghci> domsMatch 1000 7 61 simplePlayer smartPlayer 4325
(18,982)
```

The output of `domsMatch` will be the wins for each player, you can also pit two of the same player against each other, though that would just mean equal wins.

Compiling and running `Main.hs` will play a game similar to above (but a bit more performant).
