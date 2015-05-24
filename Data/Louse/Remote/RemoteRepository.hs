module Data.Louse.RemoteRepository where

type Owner = String
type Repository = String

import Data.Louse.Types

class RemoteRepository where
	{-
		Returns a list of issues from the remote repository as a stream.
	 -}
	getIssues :: Maybe Owner -> Repository -> Producer IO Bug
