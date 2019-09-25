module Epicallan.Feed
       ( feedCompiler
       ) where


import Hakyll (Compiler, Context, Identifier, Item, Rules, bodyField, compile, create, dateField,
               defaultContext, idRoute, loadAllSnapshots, recentFirst, route)
import Hakyll.Web.Feed (FeedConfiguration (..))


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Lukwago Allan's Blog"
    , feedDescription = "A blog about functional programming anything software \
                        \engineering related that catches my fancy"
    , feedAuthorName  = "Lukwago Allan"
    , feedAuthorEmail = "epicallan.al@gmail.com"
    , feedRoot        = "https://epicallan.github.io"
    }

type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

-- | Context to used for feed posts
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

feedContext :: Context String
feedContext = postCtx <> bodyField "description"

feedCompiler :: Identifier -> FeedRenderer -> Rules ()
feedCompiler feedName renderer = create [feedName] $ do
    route idRoute
    compile $
        loadAllSnapshots "posts/*" "content"
        >>= fmap (take 10) . recentFirst
        >>= renderer feedConfiguration feedContext
