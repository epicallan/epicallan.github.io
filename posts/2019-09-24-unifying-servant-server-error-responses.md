---
title: Unifying Servant server error responses
author: Lukwago Allan
tags: Haskell, library, Servant
description: Accompanying blog post to Servant Errors wai-middleware Haskell library
useShortName: yes
---

## Intro

The blog post is a discussion about exceptions in Servant and the `servant-errors wai middleware` Haskell library, its purpose and implementation details.

Find library source code in the link below.

* [epicallan/servant-errors](@github)

## Motivation

By default, when your servant server application experiences an internal exception during endpoint route resolution, e.g. request body decode errors. The server response is just plain text with a status code in the HTTP headers.

At the same time, if you don't write custom code to customise error responses for errors thrown within servant route handlers the default response is plain text with an HTTP content-type if set within `ServerError`.

With `servant-errors`  library, you get a single interface to structure and encode your error responses in one place as `JSON` error response or any other preferred form.

```haskell
-- | A typical servant application is usually of this form
main :: IO ()
main = run 8001 (serve proxyApi handlers)

-- | With 'errorMw' from servant-errors library as an error processing middleware
main :: IO ()
main = run 8001
     $ errorMw @JSON @["error", "status"]
     -- ^ Structures error response as JSON objects
     -- with 'error' and 'status' strings as error object field keys
     -- note they can be changed to any other preferred strings.
     $ serve proxyApi handlers
```
<br />

## Servant server Exceptions

____________________________________________

The following sections describe errors one may encounter in a Servant application, their differences and handling
by Servant.

### Servant server error categories

Servant server errors can be thought to belong in three categories.

- A. Internal Exceptions thrown by Servant during API route resolution, e.g. Request body decode failures.
- B. `ServerError` Exceptions thrown within API route handlers by a user.
- C. IO asynchronous and impure synchronous Exceptions either thrown by non-total functions e.g. `head` or external IO failures such as DB connection failures.

### Servant server error handling

Errors of `category A and B` have servant framework handling support, while Errors of `category C` seep through the Servant framework layer to the underlying server running your servant application such as the warp-wai server.

The warp-wai server layer, unfortunately, doesn't have a way of passing `category C` errors to middleware for translation into a `custom error response` as one would expect. It instead offers ways in which we can customise the `warp server Settings` such that one can configure how to log and create server responses for `category C` errors.

The `warp-wai` server's stance is errors of `category C` must be handled at a framework level while Servant maintains that users should write handlers safe from such errors and possibly log and process them them within a warp-wai server settings if preferred.

One can read more [on this GitHub issue](https://github.com/haskell-servant/servant/issues/779) and [this](https://github.com/haskell-servant/servant/issues/1192).
However, the point of this article is not to debate the hows of dealing with errors of `category C` but rather to focus on HTTP server response structure of errors of `category A and B`.

The way Servant handles the formulation of HTTP responses of internal exceptions of category A has 2 major drawbacks in my opinion;

- A. The generated HTTP error response lacks a content type Header, i.e. JSON or Plain Text
- B. There is no way one can customise the response body to alter its form.

The lack of HTTP content headers forces clients to be less restrictive on which HTTP responses they accept, which has its pros and cons. I prefer to explicitly encode that a client only deals with one or two specific content types.

The lack of response body customisation also means clients have to accommodate the Servant HTTP error response body in its plain raw form even when it would have been better to have a custom form such as a JSON object.

### Solution with Servant-Errors middleware library

It would have been preferable if the story for internal exception handling was a bit different, but that means making some changes within internal modules of the Servant API type combinators.
So this leaves us with a warp-wai server middleware solution, that can help us customise specific servant-server responses.

The [servant-errors library](https://github.com/epicallan/servant-errors) does precisely this. The logic is to customise responses with status codes higher than 200 while lacking HTTP content types into user preferred format.

A pleasant outcome of this approach is that it can subsequently format user thrown exceptions within servant route handlers such that they match the internal Servant error responses when they lack an HTTP content-type.
The use of this library can, therefore, enable us to achieve uniform HTTP server error responses across an entire servant application for errors of `category A and B`.

Minimal Complete Sample usage example

```haskell
-- | A greet message data type
newtype Greet = Greet { msg :: Text }
  deriving (Generic, Show)
-- servant application
main :: IO ()
main = run 8001
  $ errorMw @JSON @["error", "status"]
  -- ^ @JSON specifies content type encoding of errors
  -- @["error", "status"] specifies error and code text label in resulting JSON error response
  -- when an empty type level list parameter for 'errorMw' is specified
  -- the 'HasErrorBody' instance defaults it to '@["error", "status"]' for JSON and PlainText instances
  -- hence; errorMw @JSON @'[] == @JSON @["error", "status"]
  $ serve api handler
  where
    handler = return . id
    api = Proxy @(ReqBody '[JSON] Greet :> Post '[JSON] Greet)
```

If a user submits a wrong request body during an HTTP request, he receives a JSON encoded error response such as the one below. Using `servant-errors` library create an improvement in error responses over the default un-customisable raw text response.

```JSON
{
    "status": 400,
    "error": "Error in $: key \"msg\" not present"
}
# The response is JSON encoded and contains an HTTP content-type header plus a status code.
```

Persons Matt also has a good read on [Servant error exceptions](https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html) have a read on his blog

## Acknowledgment

Many thanks to [Dmitrii Kovanikov](https://kodimensional.dev/) for having reviewed my blog post and given me detailed and helpful suggestions.
