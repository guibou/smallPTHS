import System.Clock
import System.Process

bench :: String -> IO Integer
bench cmd = do
  start <- getTime Monotonic
  callCommand cmd
  end <- getTime Monotonic

  pure (toNanoSecs (diffTimeSpec end start))

main :: IO ()
main = do
  -- build
  callCommand "make -C CPP"
  callCommand "stack build"

  -- Bench CPP
  cppTime <- bench "./CPP/smallpt 4"

  -- Bench HS
  -- This is a bit unfair for haskell because there is approximatly 1s
  -- of overhead from stack
  hsTime <- bench "stack exec SmallPTHS"

  putStrLn "----"
  print cppTime
  print hsTime

  putStrLn ("Haskell is " ++ show (fromIntegral hsTime / fromIntegral cppTime) ++ " times slower.")
