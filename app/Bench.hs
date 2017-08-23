{-# LANGUAGE TypeApplications #-}
import System.Clock
import Data.ByteString.Lazy.Char8 (unpack)
import System.Process.Typed
import System.IO (hFlush, stdout, BufferMode(..), hSetBuffering)
import Control.Concurrent
import Data.Foldable (for_)

timeit :: IO () -> IO Integer
timeit io = do
  start <- getTime Monotonic
  io
  end <- getTime Monotonic

  pure (toNanoSecs (diffTimeSpec end start))

displayTime :: Integral a => a -> [Char]
displayTime t = show @Double (fromIntegral t / 1000 / 1000 / 1000) ++ " s"

spinner :: String -> IO ()
spinner caption = do
  putStr caption
  putStr ":  "

  for_ (cycle "|/-\\") $ \c -> do
    putChar '\b'
    putChar c
    hFlush stdout

    threadDelay (1000 * 100) -- 100ms spinner pause

progress :: String -> IO () -> IO Integer
progress caption io = do
  t <- timeit io
  putStrLn ("\r" ++ caption ++ ": " ++ displayTime t)
  pure t

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  (name, _) <- readProcess_ (proc "stack" ["--no-nix-pure", "exec", "--", "which", "SmallPTHS"])
  putStrLn ""

  -- Bench HS
  hsTimePar <- progress "Haskell (parallel)" $ (runProcess_ (proc (init (unpack name)) ["4", "+RTS", "-N", "-qg"]))
  hsTimeSeq <- progress "Haskell (sequential)" $ (runProcess_ (proc (init (unpack name)) ["4", "+RTS", "-N1"]))

  putStrLn ("Haskell speedup: " ++ show @Double (fromIntegral hsTimeSeq / fromIntegral hsTimePar))
  putStrLn ""

  -- Bench CPP
  -- build
  _ <- progress "Building CPP" $ runProcess_ (proc "make" ["-C", "CPP", "-s"])
  cppTimePar <- progress "CPP (parallel)" $ (runProcess_ (proc "./CPP/smallpt" ["4"]))
  cppTimeSeq <- progress "CPP (sequential)" $ (runProcess_ (setEnv [("OMP_NUM_THREADS", "1")] $ proc "./CPP/smallpt" ["4"]))

  putStrLn ("Cpp speedup: " ++ show @Double (fromIntegral cppTimeSeq / fromIntegral cppTimePar))
  putStrLn ""

  putStrLn "Results:"
  putStrLn ("Haskell sequential is " ++ show @Double (fromIntegral hsTimeSeq / fromIntegral cppTimeSeq) ++ " times slower.")
  putStrLn ("Haskell parallel is " ++ show @Double (fromIntegral hsTimePar / fromIntegral cppTimePar) ++ " times slower.")
