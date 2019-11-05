---
title: HTTP Requests with Hreq
author: Lukwago Allan
tags: Haskell, Network, HTTP-Client, library
description: An easy to use type driven Http client Haskell library inspired by Servant-Client.
useShortName: yes
---

## Intro

Hreq is a high-level easy to use type-driven HTTP client library inspired by Servant-Client. Hreq provides an alternative approach to type-safe construction and interpretation of API endpoints for Http client requests.

Hreq should feel very familiar to anyone who has worked with Servant-server or Servant-Client while feeling more lightweight and minimal.

Find library source code in the link below.

* [epicallan/hreq](@github)

## Motivation

Hreq was motivated by the simplicity and ease of use of [Req](https://github.com/mrkkrp/req) and the type-driven elegance of [Servant-Client](https://github.com/haskell-servant/servant/tree/master/servant-client). I envisioned Hreq as the best possible compromise of both worlds.

The Servant client library was first and foremost designed as a solution to generate API client functions for pre-defined Servant server API structures. So it shines when used in that context. However, this doesn't mean it doesn't work well in isolation; it certainly does albeit at the cost of some boiler-plate.

Hreq, on the other hand, was designed for a more general-purpose use. Its approach is thus similar to the one found in the Req library or some of the HTTP client libraries in mainstream programming languages. Hreq's interface is thus more straightforward and less verbose while maintaining good type-level expressiveness and ease of use.

## Comparison between Hreq and Servant client

Hreq shares a lot of similarities and differences with servant client, so it's imperative I list some of the prominent ones.

- Hreq's API structures are more Kind restricted, enabling more type correctness and more straightforward formulation of type-level functions. A drawback of this approach is that Hreq is less extensible than servant-client.

- Hreq provides a default HTTP client manager such that one doesn't have to think about manager configuration. This is in stark contrast with Servant-Client where you have to provide one. It's also possible to over-ride the provided default manager.

- Hreq provides type synonyms for common API type combinators, therefore, making API endpoint definitions much shorter for some cases.

- In Hreq API types are used directly within API functions via Type Application. While in servant-client, API endpoint types are used to create API querying functions. API endpoint types hence have a more first-class treatment in Hreq than in Servant client.

- In Servant-client, valid responses must have a status code between 200 and 300. In Hreq one can configure a range for valid status codes via the HTTP config with 200 to 300 as the default.

- In Hreq, API Request component arguments are provided to API functions through a Heterogeneous list. While in Servant client, arguments are provided to newly auto-created API functions.

- Hreq is still a young project, and it doesn't yet have a good streaming story as Servant-client.

## Usage Example

Find more examples in [this Example module](https://github.com/epicallan/hreq/blob/master/example/Main.hs) of the project on Github

```haskell
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Hreq

data User = User
  { name :: String
  , age  :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

main :: IO ()
main = do
  res <- runHreq baseUrl $ do
    createdUser <- createUser newUser
    myUser      <- getUserByName "allan"
    allUsers    <- hreq @(GetJson [User]) Empty
    return (createdUser, myUser, allUsers)
  print res
  where
    baseUrl :: BaseUrl
    baseUrl = BaseUrl Http "example.com" 80 "user"

    newUser :: User
    newUser = User "Allan" 29

createUser :: RunHttp m => User -> m User
createUser user = hreq @(JsonBody User :> PostJson User) (user :. Empty)

getUserByName :: RunHttp m => String -> m User
getUserByName userName = hreq @(Capture "name" String :> GetJson User) (userName  :. Empty)

```

## Conclusion

I hope you get to enjoy hreq. Please reach out through the [project's github issue tracker](https://github.com/epicallan/hreq) if you come across any issues. Happy coding.
