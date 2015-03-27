module ExtraElements where

import Test.QuickCheck
import Test.QuickCheck.Gen
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

extraTupleList = listOf1 $ ((>**<) extraKeyElements extraTypeElements extraValueElements)

keyArbitrary = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

typeArbitrary = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])

valueArbitrary = listOf1 $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['.', '_', '-', '+'])

extraArbitrary = listOf1 $ ((>**<) keyArbitrary typeArbitrary valueArbitrary)


