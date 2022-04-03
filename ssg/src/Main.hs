{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


import           Control.Monad                  ( forM_ )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import qualified Data.Text.Slugger             as Slugger
import           Hakyll                         ( Compiler
                                                , Configuration
                                                        ( destinationDirectory
                                                        , ignoreFile
                                                        , previewHost
                                                        , previewPort
                                                        , providerDirectory
                                                        , storeDirectory
                                                        , tmpDirectory
                                                        )
                                                , Context
                                                , FeedConfiguration(..)
                                                , Identifier
                                                , Item(..)
                                                , Metadata
                                                , MonadMetadata(getMetadata)
                                                , Pattern
                                                , Routes
                                                , Rules
                                                , Tags
                                                , applyAsTemplate
                                                , bodyField
                                                , buildCategories
                                                , buildTags
                                                , compile
                                                , compressCss
                                                , compressCssCompiler
                                                , constField
                                                , constRoute
                                                , copyFileCompiler
                                                , create
                                                , dateField
                                                , defaultConfiguration
                                                , defaultContext
                                                , defaultHakyllReaderOptions
                                                , defaultHakyllWriterOptions
                                                , field
                                                , fromCapture
                                                , getResourceBody
                                                , getTags
                                                , hakyllWith
                                                , idRoute
                                                , listField
                                                , listFieldWith
                                                , load
                                                , loadAll
                                                , loadAllSnapshots
                                                , loadAndApplyTemplate
                                                , loadBody
                                                , lookupString
                                                , makeItem
                                                , match
                                                , metadataField
                                                , metadataRoute
                                                , missingField
                                                , pandocCompilerWith
                                                , pathField
                                                , recentFirst
                                                , relativizeUrls
                                                , renderAtom
                                                , renderRss
                                                , replaceAll
                                                , route
                                                , saveSnapshot
                                                , tagsField
                                                , tagsMakeId
                                                , tagsMap
                                                , tagsRules
                                                , templateBodyCompiler
                                                , titleField
                                                , urlField
                                                )
import           Text.Pandoc                    ( Extension
                                                        ( Ext_fenced_code_attributes
                                                        , Ext_footnotes
                                                        , Ext_gfm_auto_identifiers
                                                        , Ext_implicit_header_references
                                                        , Ext_smart
                                                        , Ext_tex_math_dollars
                                                        , Ext_tex_math_double_backslash
                                                        , Ext_tex_math_single_backslash
                                                        )
                                                , Extensions
                                                , HTMLMathMethod(MathJax)
                                                , ReaderOptions
                                                , WriterOptions
                                                        ( writerHTMLMathMethod
                                                        , writerHighlightStyle
                                                        )
                                                , extensionsFromList
                                                , githubMarkdownExtensions
                                                , readerExtensions
                                                , writerExtensions
                                                )
import           Text.Pandoc.Highlighting       ( Style
                                                , haddock
                                                , styleToCss
                                                )
import           Text.Pandoc.Shared             ( ordNub )

--------------------------------------------------------------------------------
-- CONFIG

root :: String
root = "https://leoqiao18.github.io"

siteName :: String
siteName = "Leo Qiao's Blog"

config :: Configuration
config = defaultConfiguration { destinationDirectory = "dist"
                              , ignoreFile           = const False
                              , previewHost          = "127.0.0.1"
                              , previewPort          = 8000
                              , providerDirectory    = "src"
                              , storeDirectory       = "ssg/_cache"
                              , tmpDirectory         = "ssg/_tmp"
                              }

--------------------------------------------------------------------------------
-- BUILD

main :: IO ()
main = hakyllWith config $ do
        forM_
                        [ "CNAME"
                        , "favicon.ico"
                        , "robots.txt"
                        , "_config.yml"
                        , "images/*"
                        , "js/*"
                        , "fonts/*"
                        ]
                $ \f -> match f $ do
                          route idRoute
                          compile copyFileCompiler

        match "css/*" $ do
                route idRoute
                compile compressCssCompiler

        match "posts/*/*" $ do
                let ctx = postCtx

                route $ metadataRoute titleRoute
                compile
                        $   pandocCompilerCustom
                        >>= loadAndApplyTemplate "templates/post.html" ctx
                        >>= saveSnapshot "content"
                        >>= loadAndApplyTemplate "templates/default.html" ctx

        categories <- buildCategories "posts/*/*"
                                      (fromCapture "categories/*.html")

        tagsRules categories $ \categ pat -> do
                -- compileTags tag' pat
                route idRoute
                compile $ do
                        posts <- recentFirst =<< loadAll pat
                        -- let ids = map itemIdentifier posts
                        -- tagsList <- ordNub . concat <$> traverse getTags ids
                        let ctx =
                                    listField "posts" postCtx (return posts)
                                            <> constField "tagName" categ
                                            <> constField "title"   categ
                                            <> constField
                                                       "desc"
                                                       ("Category: " ++ categ)
                                            <> customCtx
                        makeItem ""
                                >>= loadAndApplyTemplate
                                            "templates/tag.html"
                                            ctx
                                >>= loadAndApplyTemplate
                                            "templates/default.html"
                                            ctx
                                >>= relativizeUrls

        tags <- buildTags "posts/*/*" (fromCapture "tags/*.html")

        tagsRules tags $ \tag pat -> do
                -- compileTags tag' pat
                route idRoute
                compile $ do
                        posts <- recentFirst =<< loadAll pat
                        -- let ids = map itemIdentifier posts
                        -- tagsList <- ordNub . concat <$> traverse getTags ids
                        let ctx =
                                    listField "posts" postCtx (return posts)
                                            <> constField "tagName" tag
                                            <> constField "title"   tag
                                            <> constField
                                                       "desc"
                                                       ("Category: " ++ tag)
                                            <> customCtx
                        makeItem ""
                                >>= loadAndApplyTemplate
                                            "templates/tag.html"
                                            ctx
                                >>= loadAndApplyTemplate
                                            "templates/default.html"
                                            ctx
                                >>= relativizeUrls

        match "categories.html" $ do
                route idRoute
                let ctx = defaultCtxWithAllTags categories
                compile $ do
                        getResourceBody
                                >>= applyAsTemplate ctx
                                >>= loadAndApplyTemplate
                                            "templates/default.html"
                                            ctx
                                >>= relativizeUrls

        match "tags.html" $ do
                route idRoute
                let ctx = defaultCtxWithAllTags tags
                compile $ do
                        getResourceBody
                                >>= applyAsTemplate ctx
                                >>= loadAndApplyTemplate
                                            "templates/default.html"
                                            ctx
                                >>= relativizeUrls

        match "writings.html" $ do
                route idRoute
                compile $ do
                        posts <- recentFirst =<< loadAll "posts/*/*"

                        let
                                indexCtx =
                                        listField "posts" postCtx (return posts)
                                                <> customCtx

                        getResourceBody
                                >>= applyAsTemplate indexCtx
                                >>= loadAndApplyTemplate
                                            "templates/default.html"
                                            indexCtx
                                >>= relativizeUrls

        match "index.html" $ do
                route idRoute
                compile $ do
                        posts <- recentFirst =<< loadAll "posts/*/*"

                        let
                                indexCtx =
                                        listField "posts" postCtx (return posts)
                                                <> customCtx

                        getResourceBody
                                >>= applyAsTemplate indexCtx
                                >>= loadAndApplyTemplate
                                            "templates/default.html"
                                            indexCtx
                                >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

        create ["sitemap.xml"] $ do
                route idRoute
                compile $ do
                        posts <- recentFirst =<< loadAll "posts/*/*"

                        let     pages = posts
                                sitemapCtx =
                                        constField "root" root
                                                <> constField "siteName"
                                                              siteName
                                                <> listField
                                                           "pages"
                                                           postCtx
                                                           (return pages)

                        makeItem ("" :: String)
                                >>= loadAndApplyTemplate
                                            "templates/sitemap.xml"
                                            sitemapCtx

        create ["rss.xml"] $ do
                route idRoute
                compile (feedCompiler renderRss)

        create ["atom.xml"] $ do
                route idRoute
                compile (feedCompiler renderAtom)

        create ["css/code.css"] $ do
                route idRoute
                compile (makeStyle pandocHighlightStyle)


--------------------------------------------------------------------------------
-- COMPILER HELPERS

makeStyle :: Style -> Compiler (Item String)
makeStyle = makeItem . compressCss . styleToCss

--------------------------------------------------------------------------------
-- CONTEXT

feedCtx :: Context String
feedCtx = titleCtx <> postCtx <> bodyField "description"

postCtx :: Context String
postCtx =
        constField "root" root
                <> constField "siteName" siteName
                <> dateField "date" "%Y-%m-%d"
                <> defaultContext

titleCtx :: Context String
titleCtx = field "title" updatedTitle

--------------------------------------------------------------------------------
-- TITLE HELPERS

replaceAmp :: String -> String
replaceAmp = replaceAll "&" (const "&amp;")

replaceTitleAmp :: Metadata -> String
replaceTitleAmp = replaceAmp . safeTitle

safeTitle :: Metadata -> String
safeTitle = fromMaybe "no title" . lookupString "title"

updatedTitle :: Item a -> Compiler String
updatedTitle = fmap replaceTitleAmp . getMetadata . itemIdentifier

--------------------------------------------------------------------------------
-- PANDOC

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom = pandocCompilerWith pandocReaderOpts pandocWriterOpts

pandocExtensionsCustom :: Extensions
pandocExtensionsCustom = githubMarkdownExtensions <> extensionsFromList
        [ Ext_fenced_code_attributes
        , Ext_gfm_auto_identifiers
        , Ext_implicit_header_references
        , Ext_smart
        , Ext_footnotes
        , Ext_tex_math_dollars
        , Ext_tex_math_single_backslash
        , Ext_tex_math_double_backslash
        ]

pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
        defaultHakyllReaderOptions { readerExtensions = pandocExtensionsCustom }

pandocWriterOpts :: WriterOptions
pandocWriterOpts = defaultHakyllWriterOptions
        { writerExtensions     = pandocExtensionsCustom
        , writerHighlightStyle = Just pandocHighlightStyle
        , writerHTMLMathMethod = MathJax ""
        }

pandocHighlightStyle :: Style
pandocHighlightStyle = haddock -- https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Highlighting.html

-- FEEDS

type FeedRenderer
        =  FeedConfiguration
        -> Context String
        -> [Item String]
        -> Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
        renderer feedConfiguration feedCtx =<< recentFirst =<< loadAllSnapshots
                "posts/*/*"
                "content"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
        { feedTitle       = "Leo Qiao's Blog"
        , feedDescription = "Leo Qiao's Blog"
        , feedAuthorName  = "Leo Qiao"
        , feedAuthorEmail = "qiaofeitong@hotmail.com"
        , feedRoot        = root
        }

--------------------------------------------------------------------------------
-- CUSTOM ROUTE

getTitleFromMeta :: Metadata -> String
getTitleFromMeta = fromMaybe "no title" . lookupString "title"

fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle =
        T.unpack
                . (`T.append` ".html")
                . Slugger.toSlug
                . T.pack
                . getTitleFromMeta

titleRoute :: Metadata -> Routes
titleRoute = constRoute . fileNameFromTitle

compileTags :: String -> Pattern -> Rules ()
compileTags tag pat = do
        route idRoute
        compile $ do
                -- posts <- recentFirst =<< loadAll "post/*"
                -- let ids = map itemIdentifier posts
                -- tagsList <- ordNub . concat <$> traverse getTags ids
                let ctx =
                            -- listField "tagsList"
                            --           (field "tag" $ pure . itemBody)
                            --           (traverse makeItem tagsList)
                            constField "tagline" tag
                                    <> constField "type" "tag"
                                    <> defaultContext
                makeItem "" >>= loadAndApplyTemplate "templates/tag.html" ctx
-- MARK
        -- match "index.html" $ do
        --         route idRoute
        -- compile $ do
        --         posts <- recentFirst =<< loadAll "posts/*"
        --
        --         let indexCtx =
        --                     listField "posts" postCtx (return posts)
        --                             <> constField "root"     root
        --                             <> constField "siteName" siteName
        --                             <> defaultContext
        --
        --         getResourceBody
        --                 >>= applyAsTemplate indexCtx
        --                 >>= loadAndApplyTemplate
        --                             "templates/default.html"
        --                             indexCtx
        -- match "posts/*" $ do
        --         let ctx = constField "type" "article" <> postCtx
        --
        --         route $ metadataRoute titleRoute
        --         compile
        --                 $   pandocCompilerCustom
        --                 >>= loadAndApplyTemplate "templates/post.html" ctx
        --                 >>= saveSnapshot "content"
        --                 >>= loadAndApplyTemplate "templates/default.html" ctx

-- Add tags to default context, for tag listing
-- From: https://stackoverflow.com/questions/52805193/in-hakyll-how-can-i-generate-a-tags-page 
defaultCtxWithAllTags :: Tags -> Context String
defaultCtxWithAllTags tags =
        listField "allTags" tagsCtx getAllTags <> customCtx
    where
        getAllTags :: Compiler [Item (String, [Identifier])]
        getAllTags = mapM (pure . mkItem) $ tagsMap tags
            where
                mkItem :: (String, [Identifier]) -> Item (String, [Identifier])
                mkItem x@(t, _) = Item (tagsMakeId tags t) x
        tagsCtx :: Context (String, [Identifier])
        tagsCtx =
                listFieldWith "posts" postCtx getPosts
                        <> metadataField
                        <> urlField "url"
                        <> pathField "path"
                        <> titleField "title"
                        <> missingField
            where
                getPosts
                        :: Item (String, [Identifier]) -> Compiler [Item String]
                getPosts (itemBody -> (_, is)) = mapM load is

customCtx :: Context String
customCtx =
        constField "siteName" siteName
                <> constField "root" root
                <> defaultContext

