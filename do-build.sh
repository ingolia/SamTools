#!/bin/bash

( cd samtools && \
  runhaskell Setup.hs clean && \
  runhaskell Setup.hs configure --user -p && \
  runhaskell Setup.hs build && \
  runhaskell Setup.hs haddock && \
  runhaskell Setup.hs install ) && \
( cd samtools-enumerator && \
  runhaskell Setup.hs clean && \
  runhaskell Setup.hs configure --user -p && \
  runhaskell Setup.hs build && \
  runhaskell Setup.hs haddock && \
  runhaskell Setup.hs install ) && \
( cd samtools-iteratee && \
  runhaskell Setup.hs clean && \
  runhaskell Setup.hs configure --user -p -fUtilities && \
  runhaskell Setup.hs build && \
  runhaskell Setup.hs haddock && \
  runhaskell Setup.hs install )