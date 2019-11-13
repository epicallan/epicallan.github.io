---
title: HTTP Requests with Hreq
author: Lukwago Allan
tags: Haskell, Network, HTTP-Client, library
description: An easy to use type-driven Http client Haskell library inspired by Servant-Client.
useShortName: yes
---

## Intro

Hreq is a high-level easy to use type-driven HTTP client library inspired by Servant-Client. Hreq provides an alternative approach to type-safe construction and interpretation of API endpoints for Http client requests.

Hreq should feel very familiar to anyone who has worked with Servant-Server or Servant-Client while feeling more lightweight and minimal.

Find library source code in the link below.

* [epicallan/hreq](@github)

## Motivation

Hreq was motivated by the simplicity and ease of use of [Req](https://github.com/mrkkrp/req) and the type-driven elegance of [Servant-Client](https://github.com/haskell-servant/servant/tree/master/servant-client). I envisioned Hreq as the best possible compromise of both worlds.

The Servant client library was first and foremost designed as a solution to generate API client functions for pre-defined Servant server API structures. So it shines when used in that context. However, this doesn't mean it doesn't work well in isolation; it certainly does albeit at the cost of some boiler-plate.

Hreq, on the other hand, is designed for more general-purpose use. Its approach is thus similar to the one found in the Req library or some of the HTTP client libraries in mainstream programming languages. Hreq's interface is, therefore, more straightforward and less verbose while maintaining good type-level expressiveness and ease of use.

## Implementation

One of Hreq's key features is the flexibility of the API endpoint definitions and ease of interpretation. These features are made possible thanks to the ability to emulate dependent type like language features within modern Haskell via type families, GADTs and other advanced language extensions.

The library core functionality is provided by these two type classes:

- The `HasRequest` class which interprets API endpoints into a `Request` data structure.

- The `HasResponse` class which declares the desired output from an HTTP response.

## Hreq and Servant client feature comparison

Hreq has a lot of similarities and differences with servant client, so it's imperative I list some of the prominent ones.

- Hreq's API structures are more Kind restricted, enabling more type correctness and more straightforward formulation of type-level functions. A drawback of this approach is that Hreq is less extensible than servant-client.

- Hreq provides a default HTTP client manager such that one doesn't have to think about manager configuration. This is in stark contrast with Servant-Client where you have to offer one. It's also possible to over-ride the provided default manager.

- Hreq provides type synonyms for common API type combinators, therefore, making API endpoint definitions much shorter for some cases.

- In Hreq API types are used directly within API functions via Type Application. While in servant-client, API endpoint types are used to create API querying functions. API endpoint types hence have a more first-class treatment in Hreq than in Servant client.

- In Servant-client, valid responses must have a status code between 200 and 300. In Hreq one can configure a range for valid status codes via the HTTP config with 200 to 300 as the default.

- In Hreq, API Request component arguments are provided to API functions through a Heterogeneous list. While in Servant client, arguments are provided to newly auto-created API functions.

- Hreq has an inbuilt retry mechanism via the [Retry](https://hackage.haskell.org/package/retry-0.8.0.0/docs/Control-Retry.html) package that enables retrying of network requests on connection failure. Network IO actions are prone to temporary failures that warrant retrying of the original action. On the other hand, servant-client doesn't support a retry mechanism.

- Servant-client has a native streaming backend via `SourceT` and supports other external streaming backends such as [Conduit](https://github.com/snoyberg/conduit), `Pipes` and `Machines`. On the other hand, Hreq currently supports `streaming` only via `Conduit`. Other streaming backends can easily be added depending on community interest.

- Hreq provides pattern synonyms that make the creation of `BaseUrls` more concise and less error-prone. For example, the `HttpDomain` and `HttpsDomain` synonyms create base URLs from a provided `domain`. The `HttpUrl` and `HttpsUrl` create base URLs from a provided `domain` and `path`. If one needs to provide a custom `port` number, they can fall back to directly using the `BaseUrl` constructor.

## Usage Example

The same code sample can be found in the [Example module](https://github.com/epicallan/hreq/blob/master/example/Main.hs) on Github with the necessary language extensions.

Assume we are making requests against a hypothetical HTTP service providing a JSON user management API.

```haskell
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Hreq.Client

data User = User
 { name :: Text
 , age  :: Int
 } deriving (Show, Generic, FromJSON, ToJSON)

-- User service API URL
baseUrl :: BaseUrl
baseUrl = HttpUrl "example.com" "user"
```

### Simple Get request

Make a Get request obtaining a `User` by a specified `user-name` at <http://example.com/user/:userName>

```haskell
getUserByName :: RunClient m => Text -> m User
getUserByName userName = hreq @(Capture Text :> GetJson User) (userName :. Empty)
```

The `Capture Text :> GetJson User` type within `getUserByName` is an API endpoint type definition.

The API type definition in this instance demands that a heterogeneous list containing a `Text` value is supplied to the `hreq` function.

`userName :. Empty` forms the required heterogeneous list value for the `hreq` function. Finally, the API type states that we will obtain a `JSON User` response output.

### Simple Post request

Make a Post request with Json User data for a request body returning a Json User response at <http://example.com/user>

```haskell
createUser :: RunClient m => User -> m ()
createUser user = hreq @(JsonBody User :> EmptyResponse POST) (user :. Empty)
```

### Get Request with QueryFlag

Make a Get request obtaining all users at API endpoint <http://example.com/user/all?old>

```haskell
getAllUsers :: RunClient m => m [User]
getAllUsers = hreq @("all" :> QueryFlag "old" :> GetJson [User]) Empty
```

### Running api endpoint functions

In the main function; the API endpoint functions run within the`Hreq` monad.
The Hreq monad is an instance of the `RunClient` class and `MonadIO` class.

```haskell
main :: IO ()
main = runHreq baseUrl $ do
 reqUser     <- getUserByName "allan"
 createdUser <- createUser newUser
 allUsers    <- getAllUsers
 -- Delete users with age equal to 20
 hreq @(Capture Int :> EmptyResponse DELETE) (20 :. Empty)
 -- do something with API data
 liftIO $ print (reqUser, createdUser, allUsers)
 where
   newUser :: User
   newUser = User "allan" 12
```

### More Examples

More examples can be found on `hackage` and within `library tests`.
An example showcasing streaming support via conduit can be found within the streaming package's [readme](https://github.com/epicallan/hreq/blob/master/hreq-conduit/README.md) file.

## Conclusion

I hope you get to enjoy hreq. Please reach out through the [project's GitHub issue tracker](https://github.com/epicallan/hreq) if you come across any issues. Happy coding.

## Acknowledgment

Many thanks to [Dmitrii Kovanikov](https://kodimensional.dev/) and [Alvin Kato](https://twitter.com/alvinkatojr) for having reviewed my blog post and given me helpful suggestions.
