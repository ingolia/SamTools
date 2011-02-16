module Main
       where 

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Bio.SamTools.LowLevel
import Foreign.Marshal.Array
import Foreign.Ptr

main :: IO ()
main = do sf <- samOpen "test/test.sam"
          bhp <- samHeaderRead sf
          ntargets <- liftM fromIntegral $ getNTargets bhp
          print ntargets
--          names <- getTargetName bhp >>= peekArray ntargets
--          lens <- getTargetLen bhp >>= peekArray ntargets
--          zipWithM_ printTargLen names lens
          bf <- bamOpen "test/test.bam" "w"
          bamHeaderWrite bf bhp
          b <- bamInit1
          samRead1 sf bhp b >>= print
          bamWrite1 bf b >>= print
          bamDestroy1 b
          bamClose bf
          samClose sf          
          fin <- sbamOpen "test/test.bam" "rb" nullPtr
          fout <- getSbamHeader fin >>= sbamOpen "test/test-out.sam" "wh" . castPtr
          b' <- bamInit1
          sbamRead fin b'
          sbamWrite fout b'
          bamDestroy1 b'
          sbamClose fin
          sbamClose fout
  where printTargLen name len = BS.packCString name >>= \namebs -> do
          BS.putStr namebs
          putChar '\t'
          print len
          