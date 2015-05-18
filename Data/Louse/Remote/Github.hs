module Data.Louse.Remote.Github where

    import Data.Louse.Remote.Repository
    import Github.Issues
    import qualified Data.Conduit.List as L
    import qualified Data.List as LS
    import Data.Conduit
    import Control.Monad.Trans.Resource
    import Control.Monad.IO.Class
    import Data.Louse.Types
    import qualified Data.Text as T

    data Github = Github

    instance RemoteRepository Github where

        getIssues github Nothing _ = fail "Github requires that a owner is specified for the getIssues operation"
        getIssues github (Just user) repo = 
                do
                    issues <- liftIO $ issuesForRepo user repo []
                    -- TODO: Stream by paging through results using the github library's [IssueLimitation] param 
                    either (fail . show) (L.sourceList . map issueToBug) issues


    issueToBug :: Issue -> Bug
    issueToBug issue = Bug reporter bugCreationDate bugTitle bugDescription bugOpen bugComments

        where
            -- TODO: API doesn't seem to make it easy to get the e-mail address (perhaps it's not possible for organisations or something)
            -- do we need to change the type for Bug -> Person ?
            reporter = Person (T.pack . githubOwnerLogin . issueUser $ issue) undefined
            bugCreationDate = undefined
            bugTitle = undefined
            bugDescription = undefined
            bugOpen = undefined
            bugComments = undefined

