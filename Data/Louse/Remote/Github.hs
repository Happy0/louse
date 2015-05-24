module Data.Louse.Remote.Github(getIssues, Github(Github)) where

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
                    case issues of
                        Left err -> liftIO $ putStrLn "Failed at repo" >> (fail . show) err
                        Right issues -> do
                            let source = L.sourceList issues
                            let conduit = L.mapM (issueToBug user repo)
                            source $= conduit

    issueToBug :: String -> String -> Issue -> IO LT.Bug
    issueToBug user repo issue = 
        do
            githubComments <- getComments user repo (issueNumber issue)
            emailAddress <- getUserEmail owner
            let reporter = LT.Person (T.pack owner) emailAddress

            return $ LT.Bug reporter bugCreationDate bugTitle bugDescription bugOpen githubComments

        where
            bugCreationDate = fromGithubDate . issueCreatedAt $ issue
            bugTitle = T.pack . issueTitle $ issue
            bugDescription = maybe (T.pack "") (T.pack) $ issueBody issue
            bugOpen = isNothing $ issueClosedBy issue

            owner = (githubOwnerLogin . issueUser) issue

    getUserEmail :: String -> IO T.Text
    getUserEmail userName = 
        do
            result <- userInfoFor userName
            case result of
                Left err -> putStrLn "e-mail fail" >> (fail . show) err
                Right owner -> return . maybe (T.pack "") T.pack $ detailedOwnerEmail owner

    getComments :: String -> String -> Int -> IO [LT.Comment]
    getComments user repository issueId = 
        do
            result <- comments  user repository issueId
            case result of
                Left err -> putStrLn ("comments fail " ++ (show issueId)) >> (fail . show) err
                Right githubComments -> sequence $ map toLouseComment githubComments

    toLouseComment :: G.IssueComment -> IO LT.Comment
    toLouseComment issueComment = 
        do
            commentorEmailAddress <- getUserEmail commentorLogin
            let person = LT.Person (T.pack $ commentorLogin) commentorEmailAddress
            return $ LT.Comment person date text

        where
            commentorLogin = githubOwnerLogin $ issueCommentUser issueComment
            date = fromGithubDate $ issueCommentCreatedAt issueComment
            text = T.pack $ issueCommentBody issueComment




            



