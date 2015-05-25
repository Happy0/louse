module Main where

	import  Data.Louse.Remote.Github
	import  Data.Conduit
	import Data.Conduit.Binary
	import qualified Data.Conduit.List as L
	import System.IO
	import Control.Monad.Trans.Resource
	import Github.Auth

	main :: IO ()
	main =
		do
			let github = Github "ornicar" "lila" (Nothing)
			let producer = getIssues github 
			let sink = L.mapM_ (putStrLn . show)
			connect producer sink