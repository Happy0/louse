module Data.Louse.Remote.Repository where

	import Data.Louse.Types
	import Data.Conduit
	import Control.Monad.Trans.Resource

	type Owner = String
	type Repository = String


	class RemoteRepository a where
		{-
			Returns a list of issues from the remote repository as a stream.

			-todo: Consider error handling
		-}
		getIssues :: a -> Maybe Owner -> Repository -> Producer (ResourceT IO) Bug
