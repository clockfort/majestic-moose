import Control.Error
import Control.Monad
import Control.Monad.Trans
import Data.Char
import System.IO
import Data.Maybe
import GHC.IO.Handle

main =	do
	runMaybeT . forever $ do
		clone <- liftIO $ hDuplicate stdin
		liftIO $ hSetBuffering clone NoBuffering
		liftIO $ hSetBuffering stdout NoBuffering
		liftIO $ hSetEcho clone False
		input <- liftIO $ hGetContents clone
		let output = mooseConvert input
		liftIO $ putStrLn $ mooseConvert input
		done <- liftIO $ hIsEOF stdin
		when (done) $ mzero


mooseConvert :: [Char] -> [Char]
mooseConvert string = do map (charConvert) string

charConvert c = fromMaybe c $ lookup c mooseMap

mooseMap = [('o','ø'),('O','Ø'),('y','i'),('Y','I'),('a','å')]
