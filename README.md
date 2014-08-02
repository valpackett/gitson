# gitson

A simple document store library for Git + JSON, based on [Aeson].
Uses command line git, at least for now.
No fancy indexes and stuff, but it does what I need right now.

[Aeson]: http://hackage.haskell.org/package/aeson

## Usage

```haskell
import Gitson
import Data.Aeson.TH

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing)

main :: IO ()
main = do
  createRepo "content"
  saveToCollection "content/things" "first-thing" Thing {val = 1}
  first-thing <- readFromCollection "content/things" "first-thing" :: IO (Maybe Thing)
  -- first-thing == Just Thing {val = 1}
  keys <- listCollection "content/things"
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

# Analyze coverage.
make hpc
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
