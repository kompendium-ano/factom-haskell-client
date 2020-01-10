# Haskell Json RPC client for Factom blockchain

[![Build Status](https://travis-ci.com/kompendium-llc/factom-haskell-client.svg?branch=master)](https://travis-ci.com/kompendium-llc/factom-haskell-client)
[![Coverage Status](https://camo.githubusercontent.com/b272af7667e6a2ec5f138e10c1f4cadcff257c0d/68747470733a2f2f636f766572616c6c732e696f2f7265706f732f6775696c6c61756d652d6e617267656f742f6870632d636f766572616c6c732d74657374696e67322f62616467652e706e673f6272616e63683d6d6173746572)](https://coveralls.io/github/kompendium-llc/factom-haskell-client?branch=master)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-rpc-factom/blob/master/LICENSE)
<!-- 
[![Coverage Status](https://coveralls.io/repos/github/kompendium-llc/factom-haskell-client/badge.svg?branch=master)](https://coveralls.io/github/kompendium-llc/factom-haskell-client?branch=master)
-->


A JSON-RPC Haskell client for the Factom protocol. Each response has special ADT(algebraic data type) that automatically converted from JSON response. Using [Remote Monad](https://ku-fpg.github.io/files/Gill-15-RemoteMonad.pdf) pattern multiple request can be batched and executed simulatenously, following more robust approach and reducing usage of expensive RPC calls.

Choosing a batching strategy. There are two strategies:
- `Weak`   - a single command or a single procedure, or
- `Strong` - a sequence of commands, optionally terminated by a procedure

# Installation

You can install package from [Hackage](https://hackage.haskell.org/package/api-rpc-factom) and build with Cabal, but we recommend to use [Stack](https://haskellstack.org) tool. Add to you dependencies in stack.yaml and cabal file dependency `- api-rpc-factom`.

To run and test fromrepository

1. Build with stack
```bash
$ stack build
```
2. Load REPL with stack for evaluation
```
$ stack repl
```

3. execute required methods

## Usage

for basic daemon functionality import with

```haskell
import Factom.RPC.Api
```

for wallet functionality


```haskell
import Factom.RPC.Wallet
```

#### Retreiving a balance
```haskell
-- build communication session
let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)

-- run Remote Monad
h <- send s $ do
         -- run specific events by executing exposed
         h <- reqFactoidBalance "EC2dTBH2Nc9t9Y7RFD3FYMN5ottoPeHdk6xqUWEc6eHVoBPj6CmHx"
         return h
-- show converted ADT
print h
-- or use for special business logic
```

#### Reading Entry Data
```haskell
-- build communication session
let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)

-- run Remote Monad
h <- send s $ do
         -- run specific events by executing exposed
         h <- reqEntryData "61b3d3175f211f3b23b455bb8710fdbcf545cb40da397d9e20b26eca31c389a6"
         return h
-- show converted ADT
print h
-- or use for special business logic
```

#### Writing an Entry
```haskell
-- build communication session
let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)

-- run Remote Monad
(c,c',r) <- send s $ do
         -- run specific events by executing exposed
         c <- reqComposeEntry "48e0c94d00bf14d89ab10044075a370e1f55bcb28b2ff16206d865e192827645"
                              "EC2DKSYyRcNWf7RS963VFYgMExo1824HVeCfQ9PGPmNzwrcmgm2r"
         c'<- reqCommitEntry "00015507C1024BF5C956749FC3EBA4ACC60FD485FB100E601070A44FCCE54FF358D60669854734013B6A27BCCEB6A42D62A3A8D02A6F0D73653215771DE243A63AC048A18B59DA29F4CBD953E6EBE684D693FDCA270CE231783E8ECC62D630F983CD59E559C6253F84D1F54C8E8D8665D493F7B4A4C1864751E3CDEC885A64C2144E0938BF648A00"
         r <- reqRevealEntry "007E18CCC911F057FB111C7570778F6FDC51E189F35A6E6DA683EC2A264443531F000E0005746573745A0005746573745A48656C6C6F20466163746F6D21"
         return (c,c',r)
-- show converted ADT
print h
-- or use for special business logic
```

#### Block Height and Current Minute

```haskell
-- build communication session
let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)

-- run Remote Monad
h <- send s $ do
         -- run specific events by executing exposed
         h <- reqGetHeight
         m <- reqCurrentMinute
         return (h,m)
-- show converted ADT
print h
-- or use for special business logic
```

#### Sending A Transaction

```haskell
-- build communication session
let s = weakSession (traceSendAPI "" $ clientSendAPI endpoint)

-- preate input
let txname        = "test1234"
    inputAddress  = "FA2jK2HcLnRdS94dEcU27rF3meoJfpUcZPSinpb7AwQvPRY6RL1Q"
    inputAmount   = 1000012000
    outputAddress = "FA2yeHMMJR6rpjRYGe3Q4ogThHUmByk3WLhTjQDvPrxDoTYF8BbC"
    outputAmount  = 1000000000

-- run Remote Monad
t <- send s $ do
         -- run specific events by executing exposed
         t <- reqSendTransaction txname inputAddress inputAmount outputAddress outputAmount
         return t
-- show converted ADT
print h
-- or use for special business logic
```

## Testing

You can load REPL with `stack repl` and evaluate functionality with real-time feedback using examples provided above.

## Development

To contribute to the library, clone the repository, create a feature branch and submit a PR to the `devel` branch. Once approved and tested it will be streamlined to `master` i.e upstream.
