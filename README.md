# Haskell Json RPC client for Factom blockchain

[![Build Status](https://travis-ci.com/kompendium-llc/api-rpc-factom.svg?branch=master)](https://travis-ci.com/kompendium-llc/api-rpc-factom)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-rpc-factom/blob/master/LICENSE)

A json-rpc client for the Factom protocol.

# Installation

You can install package from [Hackage](https://hackage.haskell.org/package/api-rpc-factom) and build with plain Cabal, but we recommend to use [Stack](https://haskellstack.org) ool. Add to you dependencies in stack.yaml and cabal file dependency

```yaml
- api-rpc-factom
```

```bash
$ stack build
```

# Usage

for basic daemon functionality import with

```haskell
import Factom.RPC.Api
```

for wallet functionality


```haskell
import Factom.RPC.Wallet
```

#### Retreiving a balance

#### Reading Entry Data

#### Writing an Entry

#### Block Height and Current Minute

#### Sending A Transaction

## Testing

## Development

To contribute to the library, clone the repository, create a feature branch and submit a PR to the develop branch.
