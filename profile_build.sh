ghc $1 -O2 -threaded -package-db=.cabal-sandbox/x86_64-linux-ghc-7.10.1-packages.conf.d

ghc $1 -prof -O2 -threaded -fprof-auto -package-db=.cabal-sandbox/x86_64-linux-ghc-7.10.1-packages.conf.d -osuf p_o -hisuf p_hi
