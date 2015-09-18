{-|
Module      : Data.Text.Template
Description : This module defines a type of text templates and functions for creating 
              and applying them.
Copyright   : (c) DICOM Grid Inc. 2015
License     : MIT
Maintainer  : paf31@cantab.net
Stability   : experimental

Templates can be created in code using the 'lit' and 'placeholder' functions with the
'Monoid' instance, or by parsing a template string:

> import Data.Monoid ((<>))
> 
> t1 = placeholder "lastName" <> lit ", " <> placeholder "firstName"
> t2 = parseTemplate "{{lastName}}, {{firstName}}"

'Template' is a 'Functor', so placeholders can be replaced with text using 'fmap'.

Alternatively, templates can be applied using the 'render' function.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.Template (
  -- * Types
  Template(),
  Name(..), 
  -- * Functions
  -- ** Creating Templates
  lit,
  placeholder,
  parseTemplate,
  -- ** Applying Templates
  render,
  renderTemplate,
  -- ** Lenses
  placeholders
  ) where
    
import Data.Monoid
import Data.Foldable (foldMap)
import Data.Traversable (traverse)
import Control.Monad (ap)

import qualified Data.Text as T

import Control.Applicative
    
data TemplatePart a = Lit T.Text | Placeholder a deriving (Show, Eq, Ord)

instance Functor TemplatePart where
  fmap f (Lit t) = Lit t
  fmap f (Placeholder a) = Placeholder (f a)

-- | A text template
newtype Template a = Template { runTemplate :: [TemplatePart a] } deriving (Show, Eq, Ord)

instance Functor Template where
  fmap f = Template . fmap (fmap f) . runTemplate

instance Applicative Template where
  pure = return
  (<*>) = ap

instance Monad Template where
  return = Template . return . Placeholder
  Template t >>= f = Template (t >>= go)
    where
    go (Lit t) = [Lit t]
    go (Placeholder a) = runTemplate $ f a

instance Monoid (Template a) where
  mempty = Template mempty
  mappend t1 t2 = Template (runTemplate t1 `mappend` runTemplate t2)

-- | Create a 'Template' from a literal string
lit :: T.Text -> Template a
lit = Template . pure . Lit

-- | Create a 'Template' from a placeholder which will be replaced during rendering 
placeholder :: a -> Template a
placeholder = Template . pure . Placeholder

-- | A type for names in 
newtype Name = Name { runName :: T.Text } deriving (Show, Eq, Ord)

-- | Parse a 'Template' from a template string.
--
-- Placeholders are represented using double-curly-braces (@{{ ... }}@) and everything else is considered
-- literal text. 
parseTemplate :: T.Text -> Template Name
parseTemplate = Template . go
  where
  go :: T.Text -> [TemplatePart Name]
  go t | T.null t = []
       | "{{" `T.isPrefixOf` t = let (name, rest) = T.breakOn "}}" (T.drop 2 t)
                                 in Placeholder (Name (T.strip name)) : go (T.drop 2 rest)
       | otherwise = let (text, rest) = T.breakOn "{{" t
                     in Lit text : go rest

-- | Traverse a 'Template', replacing placeholders using the specified function.    
-- 
-- This function is provided for integration with the various 'lens' packages.
placeholders :: forall f a b. (Applicative f) => (a -> f b) -> Template a -> f (Template b)
placeholders f = fmap Template . traverse apply . runTemplate
  where
  apply :: TemplatePart a -> f (TemplatePart b)
  apply (Lit t) = pure (Lit t)
  apply (Placeholder p) = Placeholder <$> f p

-- | Render a template by replacing placeholders.
render :: (a -> T.Text) -> Template a -> T.Text
render f = foldMap go . runTemplate
  where
  go (Lit t) = t
  go (Placeholder a) = f a

-- | Render a 'Template' as a template string.
renderTemplate :: Template Name -> T.Text
renderTemplate = render (("{{" <>) . (<> "}}") . runName)

