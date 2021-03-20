{-# OPTIONS_GHC -fplugin-opt=QualifiedImportsPlugin:Data.Text:Foo #-}

module Test2 where

test2 :: Foo.Text
test2 = Foo.pack "test1"
