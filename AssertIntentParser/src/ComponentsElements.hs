module ComponentsElements where

import Test.QuickCheck

packageElements = elements ["com.example.android.notepad",
                            "com.enterpriseandroid.androidSecurity",
                            "com.example.cafe"]

classElements = elements ["com.example.android.notepad.NoteEditor",
                          ".NoteEditor",
                          ".MainActivity",
                          ".CafeActivity"]

