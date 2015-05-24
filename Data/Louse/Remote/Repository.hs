module Data.Louse.Remote.Repository where

	import Data.Louse.Types
	import Data.Conduit
	import Control.Monad.Trans.Resource

	class RemoteRepository a where
		{-
			Returns a list of issues from the remote repository as a stream.

			-todo: Consider error handling
		-}
		getIssues :: a -> Producer IO Bug
