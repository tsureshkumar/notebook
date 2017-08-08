import Text.Regex.Posix
import Data.List
import System.Environment
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Maybe
import Data.List.Split hiding (startsWith)

data Commit = Commit { cid :: String,
		 	msg :: String }
instance Show Commit where
	show (Commit x y) = show $ unlines [x, y]

parse :: [String] -> [Commit]
parse xs = map parse' (filter ( (/=0) . length) (splitWhen (=~ "-----.*") xs))
		where
			parse' xs = Commit (head xs) (unlines $ tail xs)

main :: IO ()
main =  do
	args <- getArgs
	let fname = head args
	let filter_greps = head (tail (take 2 args))

	filters <- readFile filter_greps
	let ffs = lines filters

	f <- readFile fname
	let fs = lines f
	let commits = (parse $ fs)

	putStrLn $ unlines $ map show $ filter (\x -> or $ map (flip isInfixOf (msg x)) ffs) commits
