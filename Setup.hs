{-# LANGUAGE CPP #-}

#ifdef MIN_VERSION_cabal_doctest

import Distribution.Extra.Doctest
main = defaultMainWithDoctests "doctests"

#else

import Distribution.Simple
main = defaultMain

#endif
