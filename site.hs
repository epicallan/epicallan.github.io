{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

import Hakyll (Compiler, Context, Identifier, Item (..), Pattern, Rules,
               applyAsTemplate, buildTags, compile, compressCssCompiler, constField,
               copyFileCompiler, create, customRoute, dateField, defaultContext,
               defaultHakyllReaderOptions, defaultHakyllWriterOptions, field, fromCapture,
               functionField, getResourceString, getTags, hakyll, idRoute, listField,
               loadAll, loadAndApplyTemplate, lookupString, makeItem, match, metadataRoute,
               pandocCompilerWithTransformM, recentFirst, relativizeUrls, renderPandocWith, route,
               saveSnapshot, setExtension, tagsRules, templateBodyCompiler, toFilePath, (.||.))
import Hakyll.ShortcutLinks (applyAllShortcuts)
import Hakyll.Web.Feed (renderAtom, renderRss)
import System.FilePath (replaceExtension)
import Text.Pandoc.Options (WriterOptions (..))

import Epicallan.Feed (feedCompiler)
import Epicallan.Social (makeSocialContext)


import qualified Data.Text as T
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Walk as Pandoc.Walk

{- HLINT ignore "Use traverseToSnd" -}

main :: IO ()
main = mainHakyll

mainHakyll :: IO ()
mainHakyll = hakyll $ do
    match ("images/**" .||. "fonts/**" .||. "js/*"  .||. "favicon.ico") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Main page
    create ["index.html"] $ do
        route idRoute
        compile $ do
            let ctx = makeSocialContext <> defaultContext
            makeItem ""
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/main.html" ctx
                >>= relativizeUrls

    -- Posts pages
    match "posts/*" $ do
        route $ metadataRoute $ \metadata -> case lookupString "useShortName" metadata of
            Nothing -> setExtension "html"
            Just _  -> customRoute
                $ (`replaceExtension` "html")
                . ("posts/" ++)
                . drop 17
                . toFilePath

        compile $ do
            i   <- getResourceString
            pandoc <- renderPandocWith defaultHakyllReaderOptions withToc i
            let toc = itemBody pandoc
            tgs <- getTags (itemIdentifier i)
            let postTagsCtx = postCtxWithTags tgs <> constField "toc" toc <> makeSocialContext
            customPandocCompiler
                >>= loadAndApplyTemplate "templates/post.html" postTagsCtx
                >>= loadAndApplyTemplate "templates/posts-default.html" postTagsCtx
                >>= saveSnapshot "content"
                >>= relativizeUrls

    -- All posts page
    create ["posts.html"] $ compilePosts "Posts" "templates/posts.html" "posts/*"


    -- build up tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag ptrn -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        compilePosts title "templates/tag.html" ptrn

    feedCompiler "atom.xml" renderAtom
    feedCompiler "rss.xml"  renderRss

    -- Render the 404 page, we don't relativize URL's here.
    create ["404.html"] $ do
        route idRoute
        compile $ makeItem ""
            >>= applyAsTemplate defaultContext
            >>= loadAndApplyTemplate "templates/404.html" defaultContext

    match "templates/*" $ compile templateBodyCompiler

-- | Compose TOC from the markdown.
withToc :: WriterOptions
withToc = defaultHakyllWriterOptions
    { writerTableOfContents = True
    , writerTOCDepth = 4
    , writerTemplate = Just "$toc$"
    }

compilePosts :: String -> Identifier -> Pattern -> Rules ()
compilePosts title page pat = do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pat
        let ids = map itemIdentifier posts
        tagsList <- ordNub . concat <$> traverse getTags ids
        let ctx = postCtxWithTags tagsList
               <> constField "title" title
               <> constField "description" "Epicallan blog"
               <> listField "posts" postCtx (return posts)
               <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate page ctx
            >>= loadAndApplyTemplate "templates/posts-default.html" ctx
            >>= relativizeUrls

-- | Removes the @.html@ suffix in the post URLs.
stripExtension :: Context a
stripExtension = functionField "stripExtension" $ \args _ -> case args of
    [k] -> pure $ maybe k toString (T.stripSuffix ".html" $ toText k)
    _   -> error "relativizeUrl only needs a single argument"

{- | Our own pandoc compiler which adds anchors automatically and uses
@hakyll-shortcut-links@ library for shortcut transformations.
-}
customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
    (applyAllShortcuts . addAnchors)

-- | Modifie a headers to add an extra anchor which links to the header.  This
-- allows you to easily copy an anchor link to a header.
addAnchors :: Pandoc.Pandoc -> Pandoc.Pandoc
addAnchors =
    Pandoc.Walk.walk addAnchor
  where
    addAnchor :: Pandoc.Block -> Pandoc.Block
    addAnchor (Pandoc.Header level attr@(id_, _, _) content) =
        Pandoc.Header level attr $ content ++
            [Pandoc.Link ("", ["anchor"], []) [Pandoc.Str "🔗"] ('#' : id_, "")]
    addAnchor block = block

-- Context to used for posts
postCtx :: Context String
postCtx = stripExtension
    <> dateField "date" "%B %e, %Y"
    <> defaultContext

postCtxWithTags :: [String] -> Context String
postCtxWithTags tags =
    listField "tagsList" (field "tag" $ pure . itemBody) (traverse makeItem tags)
    <> postCtx
