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

Templates can be applied using the 'applyTemplate' function:

>>> :set -XOverloadedStrings
>>> let vals = [("firstName", "Haskell"), ("lastName", "Curry")]
>>> applyTemplate (`lookup` vals) t1
Just "Curry, Haskell"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.Template (
  -- * Types
  Template(),
  -- * Functions
  -- ** Creating Templates
  lit,
  placeholder,
  parseTemplate,
  -- ** Applying Templates
  applyTemplate,
  ) where
    
import Data.Monoid
import Data.Foldable (fold)
import Data.Traversable (traverse)

import qualified Data.Text as T

import Control.Applicative
    
data TemplatePart = Lit T.Text | Placeholder T.Text deriving (Show, Eq, Ord)

-- | A text template
newtype Template = Template { runTemplate :: [TemplatePart] } deriving (Show, Eq, Ord)

instance Monoid Template where
  mempty = Template mempty
  mappend t1 t2 = Template (runTemplate t1 `mappend` runTemplate t2)

-- | Create a 'Template' from a literal string
lit :: T.Text -> Template
lit = Template . pure . Lit

-- | Create a 'Template' from a placeholder which will be replaced during rendering 
placeholder :: T.Text -> Template
placeholder = Template . pure . Placeholder

-- | Parse a 'Template' from a template string.
-- |
-- | Placeholders are represented using double-curly-braces (@{{ ... }}@) and everything else is considered
-- | literal text. 
parseTemplate :: T.Text -> Template
parseTemplate = Template . go
  where
  go :: T.Text -> [TemplatePart]
  go t | T.null t = []
       | "{{" `T.isPrefixOf` t = let (name, rest) = T.breakOn "}}" (T.drop 2 t)
                                 in Placeholder (T.strip name) : go (T.drop 2 rest)
       | otherwise = let (text, rest) = T.breakOn "{{" t
                     in Lit text : go rest

-- | Traverse a 'Template', replacing placeholders using the specified function.    
applyTemplate :: forall f. (Applicative f) => (T.Text -> f T.Text) -> Template -> f T.Text
applyTemplate f = fmap fold . traverse apply . runTemplate
  where
  apply :: TemplatePart -> f T.Text
  apply (Lit t) = pure t
  apply (Placeholder p) = f p
