module ExtraElements where

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

extraKeyElements = elements ["key1",
                             "key2",
                             "key3"]

extraTypeElements = elements ["String",
                              "Integer",
                              "Boolean"]


extraTupleList = listOf $ (extraKeyElements >*< extraTypeElements)

