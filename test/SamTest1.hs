module Main
       where 

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Bio.SamTools.LowLevel
import Foreign.Marshal.Array

main :: IO ()
main = do sf <- samOpen "test/test.sam"
          bhp <- samHeaderRead sf
          ntargets <- liftM fromIntegral $ getNTargets bhp
          print ntargets
          names <- getTargetName bhp >>= peekArray ntargets
          lens <- getTargetLen bhp >>= peekArray ntargets
          zipWithM_ printTargLen names lens
          samClose sf
  where printTargLen name len = BS.packCString name >>= \namebs -> do
          BS.putStr namebs
          putChar '\t'
          print len
          