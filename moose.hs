import Control.Error
import Control.Monad
import Control.Monad.Trans
import Data.Char
import System.IO
import Data.Maybe
import GHC.IO.Handle

main =	runMaybeT . forever $ do
		clone <- liftIO $ hDuplicate stdin
		liftIO $ hSetBuffering clone NoBuffering
		liftIO $ hSetBuffering stdout NoBuffering
		liftIO $ hSetEcho clone False
		input <- liftIO $ hGetContents clone
		liftIO $ putStrLn $ map charConvert input
		done <- liftIO $ hIsEOF stdin
		when done mzero

charConvert c = fromMaybe c $ lookup c mooseMap

mooseMap = [('o','ø'),('O','Ø'),('y','i'),('Y','I'),('a','å')]
