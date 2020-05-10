# Haskell Json RPC client for Factom blockchain

![Hackage](https://img.shields.io/hackage/v/api-rpc-factom)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-rpc-factom/blob/master/LICENSE)


A JSON-RPC Haskell client for the Factom protocol. Each response has special ADT(algebraic data type) that automatically converted from JSON response. Using [Remote Monad](https://ku-fpg.github.io/files/Gill-15-RemoteMonad.pdf) pattern multiple request can be batched and executed simulatenously, following more robust approach and reducing usage of expensive RPC calls.

Choosing a batching strategy. There are two strategies:
- `Weak`   - a single command or a single procedure, or
- `Strong` - a sequence of commands, optionally terminated by a procedure

# Installation

You can install package from [Hackage](https://hackage.haskell.org/package/api-rpc-factom) and build with Cabal, but we recommend to use [Stack](https://haskellstack.org) tool. Add to you dependencies in stack.yaml and cabal file dependency `- api-rpc-factom`.

To run and test from repository

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

## Learn
- [Accessing the Factom blockchain from different programming languages](https://medium.com/kompendium-developments/accessing-factom-blockchain-from-different-programming-languages-7f09030efe16)
- [Building simple blockchain game with Factom](https://medium.com/kompendium-developments/accessing-factom-blockchain-from-different-programming-languages-7f09030efe16)

## Contributions

Please, feel free to contribute, fork the repo and submit PR.


Say thanks, send a tip:

- `btc`: 39oVXpsgsyW8ZgzsnX3sV7HLdtXWfT96qN
- `fct`: FA38cwer93mmPw1HxjScLmK1yF9iJTu5P87T2vdkbuLovm2YXyss
- `eth`: 0x9cDBA6bb44772259B3A3fb89cf233A147a720f34
- `xlm`: GD2FROTYXYSNOQQREYXHCXVOYWV556EM6PAHMVUHJNJEUYTUXUSNUZQ3
