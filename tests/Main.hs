{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Monoid
import Data.Maybe (isJust)

import qualified Data.Text as T
import qualified Data.Text.Template as T

import Test.QuickCheck

import Control.Monad.Identity
import Control.Applicative

genTemplate :: Gen T.Template
genTemplate = oneof [ do t <- genText
                         return (T.lit t)
                    , do t1 <- genText
                         t2 <- genText
                         t3 <- genTemplate
                         return (T.lit t1 <> T.placeholder t2 <> t3)
                    , do t1 <- genText
                         t2 <- genTemplate
                         return (T.placeholder t1 <> t2)
                    ]

genTemplateString :: Gen T.Text
genTemplateString = mconcat <$> listOf (oneof [ genText, ("{{" <>) . (<> "}}") <$> genText ])

genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements (['a'..'z'] ++ ['0'..'9']))

main :: IO ()
main = do
  -- Parser and pretty printer are compatible
  quickCheck $ do
    t <- genTemplate
    return $ T.parseTemplate (T.printTemplate t) == t
  quickCheck $ do
    s <- genTemplateString
    return $ T.printTemplate (T.parseTemplate s) == s

  -- printTemplate is a Monoid morphism
  quickCheck $ T.printTemplate mempty == mempty
  quickCheck $ do
    t1 <- genTemplate
    t2 <- genTemplate
    return $ T.printTemplate (t1 <> t2) == T.printTemplate t1 <> T.printTemplate t2
