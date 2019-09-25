module Epicallan.Social
       ( Social (..)
       , makeSocialContext
       ) where

import Hakyll (Compiler, Context, Item, field, itemBody, listField, makeItem)

data Social = Social
    { sName :: String
    , sLink :: String
    }

socialName, socialLink :: Context Social
socialName = field "socialName" $ pure . sName . itemBody
socialLink = field "socialLink" $ pure . sLink . itemBody

allSocials :: Compiler [Item Social]
allSocials = traverse makeItem
    [ Social "twitter"  "https://twitter.com/epicallanl"
    , Social "github"   "https://github.com/epicallan"
    , Social "reddit"   "https://www.reddit.com/user/epicallanl"
    , Social "linkedin" "https://www.linkedin.com/in/allan-lukwago"
    ]

makeSocialContext :: Context a
makeSocialContext = listField "socials" (socialName <> socialLink) allSocials
