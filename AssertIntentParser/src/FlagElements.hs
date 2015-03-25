module FlagElements where

import Test.QuickCheck

flagElements = elements ["FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET",
                         "FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS",
                         "FLAG_ACTIVITY_FORWARD_RESULT"]

flagElementsList = listOf $ flagElements