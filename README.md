# gitson [![Build Status](https://travis-ci.org/myfreeweb/gitson.svg?branch=master)](https://travis-ci.org/myfreeweb/gitson)

A simple document store library for Git + JSON, based on [Aeson].
Uses command line git, at least for now.
No fancy indexes and stuff, but it does what I need right now.
Transactions use [flock], so it's safe even across completely separate programs!

[Aeson]: http://hackage.haskell.org/package/aeson
[flock]: http://hackage.haskell.org/package/flock

## Usage

```haskell
import Gitson
import Gitson.Util (insideDirectory)
import Data.Aeson.TH
import Control.Monad.IO.Class (liftIO)

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing)

main :: IO ()
main = do
  -- Creating a new "database," basically mkdir + git init
  createRepo "./content"

  -- Writing data to a "database" happens in transactions
  -- A transaction is committed after the block is executed
  -- Just like in SQL databases
  transaction "./content" $ do
    -- order: (data)          (key)         (collection)
    saveEntry Thing {val = 1} "first-thing" "content"
    -- Collections are created automatically, like in MongoDB
    liftIO $ putStrLn "Written first-thing"
    -- You have to use liftIO to do IO actions inside of a transaction!
    -- Because a transaction is a monad transformer, WriterT actually

  -- Reading data
  -- (These are normal IO actions, so if you want
  --  to read inside of a transaction, liftIO)
  keys <- listEntries "./content/things"
       -- Just ["first-thing"]
  first-thing <- readEntry "first-thing" "./content/things" :: IO (Maybe Thing)
              -- Just Thing {val = 1}

  -- Reading data, avoiding repetition of the repo path
  insideDirectory "./content" $ do
    keys <- listEntries "/things"
         -- Just ["first-thing"]
    first-thing <- readEntry "first-thing" "/things" :: IO (Maybe Thing)
         -- Just Thing {val = 1}
```

## Development

```bash
# Update to latest version of Cabal.
cabal update
cabal install cabal-install

# Initialize a sandbox and install the package's dependencies.
make install

# Configure & build the package.
make configure build

# Test package.
make test

# Benchmark package.
make bench

# Start a REPL.
make repl

# Generate documentation.
make haddock
```

## License

Copyright 2014 Greg V <floatboth@me.com>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
