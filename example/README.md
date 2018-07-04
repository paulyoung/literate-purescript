# Example

This code block will be compiled using the dependencies specified via `bower.json`:

```purescript
module Example.One where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Hello sailor!"
```

This code block relies on a locally defined module:

```purescript
module Example.Two where

import Prelude

import Universe (answer)

isAnswer :: Int -> Boolean
isAnswer = (_ == answer)
```

This code block uses something defined in the previous code block:

```purescript
module Example.Three where

import Example.Two (isAnswer)

answerIsFortyTwo :: Boolean
answerIsFortyTwo = isAnswer 42
```
