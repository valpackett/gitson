# gitson

A simple document store library for Git + JSON, based on [Aeson].
Uses command line git, at least for now.
No fancy indexes and stuff, but it does what I need right now.
Transactions use [flock], so it's safe even across completely separate programs!

[Aeson]: http://hackage.haskell.org/package/aeson
[flock]: http://hackage.haskell.org/package/flock

## Usage

```haskell
import Gitson
import Gitson.Util
import Data.Aeson.TH
import Control.Monad.IO.Class (liftIO)

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing)

main :: IO ()
main = do
  createRepo "./content" -- git init
  transaction "./content" $ do
    saveEntry Thing {val = 1} "first-thing" "content"
    liftIO $ putStrLn "Written first-thing"
    -- This `do` block is not the raw IO monad, it's a WriterT, so we have to use liftIO to do IO actions inside of it!
  -- *After the block ends*, the files are written, committed to git and the lock is released.
  first-thing <- readEntry "first-thing" "./content/things" :: IO (Maybe Thing)
  -- first-thing == Just Thing {val = 1}
  keys <- listEntries "./content/things"
  -- keys == Just ["first-thing"]
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
