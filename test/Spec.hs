import System.Process (system)

main :: IO ()
main = do
  _ <- system "./check_baseline.sh ./test/assignment_01 2> /dev/null"
  return ()
