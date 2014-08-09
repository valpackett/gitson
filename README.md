# gitson [![Hackage](https://img.shields.io/hackage/v/gitson.svg?style=flat)](https://hackage.haskell.org/package/gitson) [![Build Status](https://img.shields.io/travis/myfreeweb/gitson.svg?style=flat)](https://travis-ci.org/myfreeweb/gitson) [![Coverage Status](https://img.shields.io/coveralls/myfreeweb/gitson.svg?style=flat)](https://coveralls.io/r/myfreeweb/gitson) [![Apache License 2.0](https://img.shields.io/badge/license-Apache%202.0-brightgreen.svg?style=flat)](https://www.tldrlegal.com/l/apache2)

A simple document store library for Git + JSON, based on [Aeson].
Uses command line git, at least for now.
No fancy indexes and stuff, but it does what I need right now.
Transactions use [flock], so it's safe even across completely separate programs!

[Aeson]: http://hackage.haskell.org/package/aeson
[flock]: http://hackage.haskell.org/package/flock

## Usage

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Gitson
import Gitson.Util (insideDirectory)
import Data.Aeson.TH
import Control.Monad.IO.Class (liftIO)

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing) -- there are non-Template ways of doing this, see aeson docs

main :: IO ()
main = do
  -- Creating a new "database," basically mkdir + git init
  createRepo "./content"

  -- Writing data to a "database" happens in transactions
  -- A transaction is committed (write files & git commit)
  -- after the block is executed, just like in SQL databases
  -- Also, transactions are thread-safe
  transaction "./content" $ do
    -- order: (collection) (key        ) (data)
    saveEntry "things"     "first-thing" Thing {val = 1}
    -- Collections are created automatically, like in MongoDB
    liftIO $ putStrLn "Written first-thing"
    -- You have to use liftIO to do IO actions inside of a transaction!
    -- Because a transaction is a monad transformer, WriterT actually

  -- Reading data
  -- (These are normal IO actions, so if you want
  --  to read inside of a transaction, liftIO.
  --  Note: transaction already includes insideDirectory!
  --  Warning: you can't read what you've written in the current transaction!!!
  --  You can only read what's been written before the transaction began.)
  insideDirectory "./content" $ do
    colls <- listCollections
          -- ["things"]
    keys <- listEntryKeys "things"
         -- ["first-thing"]
    first-thing <- readEntry "things" "first-thing" :: IO (Maybe Thing)
         -- Just Thing {val = 1}
    things <- readEntries "things" :: IO [Thing]
           -- [Thing {val = 1}]

  -- Note: insideDirectory is just a function that changes
  -- the current directory, executes an action and changes it back.
  -- You can use reading actions without it, like this:
  keys <- listEntryKeys "./content/things"


  -- And now, some bells and whistles:
  -- Numeric id support
  transaction "./content" $ do
    saveNextEntry "things" "hello-world" Thing {val = 1}
    -- will save to things/000001-hello-world.json
  insideDirectory "./content" $ do
    thing <- readEntryById "things" 1
    same-thing <- readEntryByName "thing" "hello-world"
    -- both will read from things/000001-hello-world.json
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
