Tangaroa
========
Raft with Byzantine Fault Tolerance in Haskell
----------------------------------------------

To build:
 - Install GHC and Cabal: http://www.haskell.org/ghc/ https://www.haskell.org/cabal/ (or use a package manager)
 - `cabal update`
 - `cabal install --only-dependencies`
 - `cabal configure`
 - `cabal build`

See the `bin` directory for example server and client implementations.
The `bft*.sh` or `bftservers.sh` scripts will launch BFT Raft nodes, and `bftclient.sh`
will launch a client to connect to them.

For standard Raft, use `client.sh` and `servers.sh`.
