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

  name <- readProcess "stack" ["--no-nix-pure", "exec", "--", "which", "SmallPTHS"] ""

  print name

  -- Bench CPP
  cppTime <- bench "./CPP/smallpt 4"

  -- Bench HS
  hsTime <- bench (init name ++ " 4")

  putStrLn "----"
  print cppTime
  print hsTime

  putStrLn ("Haskell is " ++ show (fromIntegral hsTime / fromIntegral cppTime) ++ " times slower.")
