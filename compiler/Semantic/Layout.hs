-- |
-- The two layouts of a semantic tree, as a type-level tag: 'Normal', with
-- declarations as they were written, and 'Group', with declarations in
-- groups of ones that depend on each other, to be checked together. See
-- "Semantic.Connect".
module Semantic.Layout where

data Layout
  = Normal
  | Group

type Normal = 'Normal

type Group = 'Group
