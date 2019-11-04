# Haskell Json RPC client for Factom blockchain

[![Build Status](https://travis-ci.com/kompendium-llc/api-rpc-factom.svg?branch=master)](https://travis-ci.com/kompendium-llc/api-rpc-factom)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-rpc-factom/blob/master/LICENSE)

A JSON-RPC Haskell client for the Factom protocol. Each response has special ADT(algebraic data type) that automatically converted from JSON response.

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

```

```

#### Reading Entry Data
```
```
#### Writing an Entry

#### Block Height and Current Minute

#### Sending A Transaction

## Testing

## Development

To contribute to the library, clone the repository, create a feature branch and submit a PR to the develop branch.
