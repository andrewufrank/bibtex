-----------------------------------------------------------------------------
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework
import Text.BibTeX.Parse
import Text.BibTeX.Entry as T
import qualified Text.BibTeX.Parse as Parse
import qualified Text.BibTeX.Entry as Entry

--import {-@ HTF_TESTS @-} ShakeStartTests
------ must run first because it produces the test values used later
--
----import {-@ HTF_TESTS @-} Lib.FileMgt_test
--
----import {-@ HTF_TESTS @-} Lib.Foundation_test
--    -- writes A : testLayout
--    --  pageFn :: abs pandoc filenames
----import {-@ HTF_TESTS @-} Lib.Pandoc_test
--    -- test_pandoc_pageFn_pageMd_1 - pageFn -> pageMd : MarkdownText
--    -- AK :: MarkdownText -> BE  DocValue
--    -- AK ->AD :: Pandoc
--    -- AD -> AF ::
----import {-@ HTF_TESTS @-} Lib.Bake_test
----import {-@ HTF_TESTS @-} Lib.ReadSettingFile_test
----import {-@ HTF_TESTS @-} Lib.Indexing_test
--import {-@ HTF_TESTS @-} Lib.BibTex_test

--
main ::  IO ()
main =  do  -- the local tests only
     putStrLn "HTF ExampleTest.hs:\n"
     r <- htfMain htf_thisModulesTests
     putStrLn ("HTF end ExampleTesting.hs test:\n" ++ show r)
     return ()
--main =  do  -- with tests in other modules
--    putStrLn "HTF ExampleTest.hs:\n"
--    p <- htfMain htf_importedTests
--    putStrLn ("HTF end ExampleTest.hs test:\n" ++ show p ++ "\nEND HTF ExampleTest")
--    return ()

test_parse1 = do
    bibtxt <- readBibTex "publications/publications.bib"
    bibparsed <- parseBibTex bibtxt
--    assertEqual res1 (take 100 . show $ bibparsed)
    assertEqual res2 (reverse . take 100 . reverse . show $ bibparsed)
res1 = "[Cons {entryType = \"InProceedings\", identifier = \"alfer2002beginning\", fields = [(\"author\",\"Alfer, R"
res2 = " {entryType = \"\", identifier = \"\", fields = []},Cons {entryType = \"\", identifier = \"\", fields = []}]"
-- demonstrates that the comments at end are read as empty bibtex entries


test_parse2 = do    -- to check for [TUid] case
    bibtxt <- readBibTex "publications/tubib.bib"
    bibparsed <- parseBibTex bibtxt
--    assertEqual res1 (take 100 . show $ bibparsed)
    assertEqual resa  ( bibparsed)
resa =   [Cons{entryType = "InProceedings",
          T.identifier = "frank09geo[TU11111]",
          fields =
            [("author", "Frank, Andrew U."),
             ("title", "Geo-Ontologies Are Scale Dependent (abstract only)"),
             ("booktitle",
              "European Geosciences Union, General Assembly 2009, Session Knowledge and Ontologies"),
             ("year", "2009"), ("editor", "Pulkkinen, Tuija"),
             ("url", "http://publik.tuwien.ac.at/files/PubDat-175453.pdf"),
             ("file", "docs/docs4/4698_GeoOntologies_abstarct_EUG_09.pdf"),
             ("groups", "authorAF"), ("keywords", "Onto"), ("owner", "frank"),
             ("timestamp", "2018.11.29")]},
            Cons{entryType = "", T.identifier = "", fields = []}]

test_getGroup = do    -- to check for [TUid] case
    bibtxt <- readBibTex "publications/tubib.bib"
    bibparsed <- parseBibTex bibtxt
    let selected = filterByGroup "authorAF" bibparsed
    assertEqual resg  ( selected)
resg=   [Cons{entryType = "InProceedings",
          T.identifier = "frank09geo[TU11111]",
          fields =
            [("author", "Frank, Andrew U."),
             ("title", "Geo-Ontologies Are Scale Dependent (abstract only)"),
             ("booktitle",
              "European Geosciences Union, General Assembly 2009, Session Knowledge and Ontologies"),
             ("year", "2009"), ("editor", "Pulkkinen, Tuija"),
             ("url", "http://publik.tuwien.ac.at/files/PubDat-175453.pdf"),
             ("file", "docs/docs4/4698_GeoOntologies_abstarct_EUG_09.pdf"),
             ("groups", "authorAF"), ("keywords", "Onto"), ("owner", "frank"),
             ("timestamp", "2018.11.29")]}
        ]

------------  the functions used

readBibTex :: FilePath ->  IO String
-- reads the bibtex file
readBibTex fp = do
    --      bib <- getContents
    bib <- readFile fp
--    putIOwords ["readBibTex", showT bib]
    return bib

parseBibTex :: String -> IO [Entry.T]
parseBibTex bib = case  parse (skippingLeadingSpace   Parse.file) "stdin" bib of
--parseBibTex bib = case Parsec.parse (Parsec.skipMany Parsec.space >> Parse.file) "stdin" bib of
         Left errMsg -> error  (show errMsg)
         Right entries -> return entries

filterByGroup :: String -> [Entry.T] -> [Entry.T]
 -- select the entries in a group
filterByGroup groupname entries = filter (elem groupname .getGroup) entries

getGroup :: Entry.T -> [String]
getGroup e = map snd groupFields
    where
        fs = fields e  :: [(String, String)]
        groupFields = filter (("groups" ==).fst) fs

getBibIdentifier :: [Entry.T] -> [String]
-- extract the bib identifier from the entries
--getBibIdentifier es = map T.entryType es
getBibIdentifier es = map T.identifier es


