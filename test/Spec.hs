import System.Exit (exitWith)
import System.Process (system)

main :: IO ()
main = do
  exitcode <- system "./check_baseline.sh ./test/baselines 2> /dev/null"
  exitWith exitcode
