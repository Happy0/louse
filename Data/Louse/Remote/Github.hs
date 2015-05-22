module Data.Louse.Remote.Github where

    import Data.Louse.Remote.Repository
    import Github.Issues
    import qualified Data.Conduit.List as L
    import qualified Data.List as LS
    import Data.Conduit
    import Control.Monad.Trans.Resource
    import Control.Monad.IO.Class
    import qualified Data.Louse.Types as LT
    import qualified Data.Text as T
    import Data.Maybe
    import Control.Exception
    import Github.Users
    import Github.Issues.Comments
    import qualified Github.Data.Definitions as G

    data Github = Github

    instance RemoteRepository Github where

        getIssues github Nothing _ = fail "Github requires that a owner is specified for the getIssues operation"
        getIssues github (Just user) repo = 
                do
                    issues <- liftIO $ issuesForRepo user repo []
                    -- TODO: Stream by paging through results using the github library's [IssueLimitation] param 
                    either (fail . show) (L.sourceList . map issueToBug) issues


    issueToBug :: Issue -> LT.Bug
    issueToBug issue = LT.Bug reporter bugCreationDate bugTitle bugDescription bugOpen bugComments

        where
            reporter = LT.Person (T.pack . githubOwnerLogin . issueUser $ issue) undefined
            bugCreationDate = fromGithubDate . issueCreatedAt $ issue
            bugTitle = T.pack . issueTitle $ issue
            bugDescription = maybe (T.pack "") (T.pack) $ issueBody issue
            bugOpen = isNothing $ issueClosedBy issue
            bugComments = undefined

            owner = (githubOwnerLogin . issueUser) issue

    getAuthor :: Issue -> IO T.Text
    getAuthor issue = 
        do
            result <- userInfoFor issueOwner
            case result of
                Left err -> (fail . show) err
                Right owner -> return . maybe (T.pack "") T.pack $ detailedOwnerEmail owner

        where
            issueOwner = githubOwnerLogin (issueUser issue)

    getComments :: String -> String -> Int -> IO [LT.Comment]
    getComments user repository issueId = 
        do
            result <- comments  user repository issueId
            case result of
                Left err -> fail . show $ err
                Right githubComments -> return $ map toLouseComment githubComments

    toLouseComment :: G.IssueComment -> LT.Comment
    toLouseComment issueComment = LT.Comment person date text

        where
            -- TODO: Get E-mail addresses
            person = LT.Person (T.pack $ githubOwnerLogin $ issueCommentUser issueComment) ""
            date = fromGithubDate $ issueCommentCreatedAt issueComment
            text = T.pack $ issueCommentBody issueComment




            



