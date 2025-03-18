# VCR

VCR is a Haskell library for recording and replaying HTTP interactions, making your tests fast, deterministic, and free from real network dependencies. Inspired by [similar libraries in other languages](https://github.com/vcr/vcr), it captures real HTTP requests and responses into a YAML “tape” and replays them as needed.

## Features
 - 📼 HTTP request/response recording to YAML "tapes"
 - 🔁 HTTP response playback on subsequent requests
 - ⏯️ Two playback modes:
   - 🔀 `AnyOrder`: Requests may be replayed in any order
   - 🎞️ `Sequential`: Requests must be replayed in the order in which they were recorded
 - 🔒 Automatic redaction of sensitive information (e.g. the `Authorization` header)
 - 🧶 Thread-safe and non-blocking design
 - ⚡ Suitable for highly concurrent workloads
 - ⚙️ ~Integration with popular Haskell HTTP client libraries~

## Installation
VCR requires a patched version of `http-client`.

Add `assertible/http-client` to your `stack.yaml`:
```yaml
extra-deps:
  - github: assertible/http-client
    commit: assertible-http-client-0.7.17
    subdirs:
      - http-client
```
or `cabal.project`:
```cabal
source-repository-package
  type: git
  location: https://github.com/assertible/http-client
  tag: assertible-http-client-0.7.17
  subdir: http-client
```

Add VCR to your project's `package.yaml`:

```yaml
dependencies:
  - base == 4.*
  - vcr
```

or `.cabal` file:

```cabal
build-depends:
  base == 4.*,
  vcr
```

## Usage

VCR provides the following operations to work with tapes:

- **`VCR.with`:** Replays previously recorded interactions and records new ones.
- **`VCR.record`:** Records new interactions, overwriting any existing ones.
- **`VCR.play`:** Replays previously recorded interactions and fails when a requested interaction is not found on the tape.

### Basic Example

```haskell top
{-# LANGUAGE OverloadedStrings #-}
```

```haskell
import VCR
import Network.HTTP.Simple
import Data.ByteString (ByteString)

get :: [Header] -> Request -> IO (Response ByteString)
get headers url = httpBS $ setRequestHeaders (("User-Agent", "vcr") : headers) url

main :: IO ()
main = do
  VCR.with "tape.yaml" $ do
    response <- get [] "https://api.weather.gov/"
    print (getResponseBody response)
```

In this example, on the first run, the HTTP interaction is recorded. On subsequent runs the response is replayed from the tape.

<details>
<summary>Recorded tape</summary>

```yaml
- request:
    method: GET
    url: https://api.weather.gov/
    headers:
    - name: User-Agent
      value: vcr
    body: ''
  response:
    status:
      code: 200
      message: OK
    headers:
    - name: Server
      value: nginx/1.20.1
    - name: Content-Type
      value: application/json
    body: |-
      {
          "status": "OK"
      }
```
</details>

### Recording-only

```haskell
  VCR.record "tape.yaml" $ do
    -- HTTP requests made here will be recorded
```
<!--
```haskell
    (...)
```
-->

### Playback-only

```haskell
  VCR.play "tape.yaml" $ do
    -- HTTP requests will use recorded responses
```
<!--
```haskell
    (...)
```
-->

## Configuration

- Configure playback behavior using `mode`.
- Customize redaction behavior using `redact`.

### Modes
- **`AnyOrder` (default):** The tape is interpreted as a mapping from requests to responses, allowing responses to be replayed in any order.

  ```haskell
    VCR.with "tape.yaml" { mode = AnyOrder } $ do
      (...)
  ```

- **`Sequential`:** The tape is treated as an ordered list; interactions must be replayed in the recorded order.

  ```haskell
    VCR.with "tape.yaml" { mode = Sequential } $ do
      (...)
  ```

### Redacting Sensitive Information

By default, VCR censors the `Authorization` header by replacing its value with `"********"`. To customize redaction, provide your own `redact` function when creating a tape.

Default redaction example:

```haskell
  VCR.with "redacted-tape.yaml" $ do
    let auth = ("Authorization", "Bearer sk-07bTqwJXhYqDouHt434x6Oo9xYgkNzDd")
    response <- get [auth] "https://api.weather.gov/"
    (...)
```

Custom redaction example:

```haskell top
import WebMock qualified
```
```haskell
  let
    redact :: WebMock.Request -> WebMock.Request
    redact req = req {
      WebMock.requestHeaders = map
        (\ (name, value) -> (name, if name == "X-Secret" then "[REDACTED]" else value))
        (WebMock.requestHeaders req)
    }

  VCR.with "redacted-tape.yaml" { redact } $ do
    let auth = ("X-Secret", "Bearer sk-07bTqwJXhYqDouHt434x6Oo9xYgkNzDd")
    response <- get [auth] "https://api.weather.gov/"
    (...)
```

<details>
<summary>Recorded tape</summary>

```yaml
- request:
    method: GET
    url: https://api.weather.gov/
    headers:
    - name: User-Agent
      value: vcr
    - name: Authorization
      value: '********'
    body: ''
  response:
    status:
      code: 200
      message: OK
    headers:
    - name: Server
      value: nginx/1.20.1
    - name: Content-Type
      value: application/json
    body: |-
      {
          "status": "OK"
      }
- request:
    method: GET
    url: https://api.weather.gov/
    headers:
    - name: User-Agent
      value: vcr
    - name: X-Secret
      value: '[REDACTED]'
    body: ''
  response:
    status:
      code: 200
      message: OK
    headers:
    - name: Server
      value: nginx/1.20.1
    - name: Content-Type
      value: application/json
    body: |-
      {
          "status": "OK"
      }
```
</details>

## Using VCR with Hspec

You can use a VCR tape for:
- a single test (suitable for `Sequential` and `AnyOrder` mode)
- a group of tests (suitable for `AnyOrder` mode only)
- the entire test suite (suitable for `AnyOrder` mode only)

### Using VCR for a single test
You can use VCR for individual test cases.

Example:
```haskell top
import Test.Hspec
```

```haskell
spec :: Spec
spec = do
  it "behaves in a certain way" $ do
    VCR.with "test/fixtures/tape-for-individual-test.yaml" $ do
      (...)
```

### Using VCR for a group of tests

Alternatively, you can use the same VCR tape for a group of tests.

Example:
```haskell
  describe "subject" $ do
    aroundAll_ (VCR.with "test/fixtures/tape-for-subject.yaml") $ do
      it "behaves in a certain way" $ do
       (...)
      it "behaves in a certain other way" $ do
       (...)
```

### Using a single VCR tape for the entire test suite
By defining a [spec hooks](https://hspec.github.io/hspec-discover.html#spec-hooks), it is possible to use the same VCR tape for an entire test suite.

Example:
```haskell ignore hook
{-# LANGUAGE OverloadedStrings #-}
module SpecHook where

import VCR
import Test.Hspec

hook :: Spec -> Spec
hook = aroundAll_ (VCR.with "test/fixtures/vcr-tape.yaml")
```

## Using VCR for highly concurrent workloads

***Note:*** This section applies to `AnyOrder` mode only.

VCR is suitable for highly concurrent workloads.
- Request recording and playback are thread-safe.
- Concurrent requests to *different* resources do not block each other.
- Results for concurrent requests to  the *same* resource are shared.

Example Hspec [spec hook](https://hspec.github.io/hspec-discover.html#spec-hooks) that leverages all available cores and uses the same VCR tape for the entire test suite:
```haskell ignore parallel-hook
{-# LANGUAGE OverloadedStrings #-}
module SpecHook where

import VCR
import Test.Hspec
import GHC.Conc

useAllAvailableCores :: SpecWith a -> SpecWith a
useAllAvailableCores = (runIO (getNumProcessors >>= setNumCapabilities) >>)

hook :: Spec -> Spec
hook = useAllAvailableCores . parallel . aroundAll_ (VCR.with "test/fixtures/vcr-tape.yaml")
```

## API Documentation

~Full API documentation is available on [Hackage](https://hackage.haskell.org/package/vcr).~

<!--
```haskell
(...) :: IO ()
(...) = return ()
```
-->

<!-- vim: :set ts=2 sw=2: -->
