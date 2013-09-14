import Control.Monad
import Data.Char
import System.IO
import Data.Maybe
import GHC.IO.Handle

main = forever $ do
	clone <- hDuplicate stdin
	hSetBuffering clone NoBuffering
	hSetBuffering stdout NoBuffering
	hSetEcho clone False
	input <- hGetContents clone
	let output = mooseConvert input
	putStrLn $ mooseConvert input

mooseConvert :: [Char] -> [Char]
mooseConvert string = do map (charConvert) string

charConvert c = fromMaybe c $ lookup c mooseMap

mooseMap = [('o','ø'),('O','Ø'),('y','i'),('Y','I')]
