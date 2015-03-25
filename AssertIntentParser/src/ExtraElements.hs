module ExtraElements where

import Test.QuickCheck
import Test.QuickCheck.Instances.Tuple

extraKeyElements = elements ["key1",
                             "key2",
                             "key3"]

extraTypeElements = elements ["string",
                              "boolean",
                              "integer",
                              "long",
                              "float",
                              "URI",
                              "component name",
                              "array of integers",
                              "array of longs",
                              "array of floats"]

extraValueElements = elements ["it is string value",
                               "100",
                               "3.14"]

extraTupleList = listOf $ ((>**<) extraKeyElements extraTypeElements extraValueElements)

