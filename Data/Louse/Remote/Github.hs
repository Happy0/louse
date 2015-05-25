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
    import Github.Auth

    type Owner = String
    type Repository = String

    data Github = Github {owner :: Owner, repository :: Repository, authentication :: Maybe GithubAuth } 

    instance RemoteRepository Github where

        getIssues github = 
                do
                    issues <- liftIO $ issuesForRepo' auth user repo []
                    case issues of
                        Left err -> liftIO $ putStrLn "Failed at repo" >> (fail . show) err
                        Right issues -> do
                            let source = L.sourceList issues
                            let conduit = L.mapM (issueToBug github)
                            source $= conduit
                where
                    Github user repo auth = github

    issueToBug :: Github -> Issue -> IO LT.Bug
    issueToBug github issue = 
        do
            githubComments <- getComments github (issueNumber issue)
            emailAddress <- getUserEmail github owner
            let reporter = LT.Person (T.pack owner) emailAddress

            return $ LT.Bug reporter bugCreationDate bugTitle bugDescription bugOpen githubComments

        where
            (Github user repo auth) = github
            bugCreationDate = fromGithubDate . issueCreatedAt $ issue
            bugTitle = T.pack . issueTitle $ issue
            bugDescription = maybe (T.pack "") (T.pack) $ issueBody issue
            bugOpen = isNothing $ issueClosedBy issue

            owner = (githubOwnerLogin . issueUser) issue

    getUserEmail :: Github -> String -> IO T.Text
    getUserEmail (Github _ _ auth) userName = 
        do
            result <- userInfoFor' auth userName
            case result of
                Left err -> putStrLn "e-mail fail" >> (fail . show) err
                Right owner -> return . maybe (T.pack "") T.pack $ detailedOwnerEmail owner

    getComments :: Github -> Int -> IO [LT.Comment]
    getComments github issueId = 
        do
            result <- comments' auth user repository issueId
            case result of
                Left err -> putStrLn ("comments fail " ++ (show issueId)) >> (fail . show) err
                Right githubComments -> sequence $ map (toLouseComment github) githubComments
        where
            Github user repository auth = github

    toLouseComment :: Github -> G.IssueComment -> IO LT.Comment
    toLouseComment github issueComment = 
        do
            commentorEmailAddress <- getUserEmail github commentorLogin
            let person = LT.Person (T.pack $ commentorLogin) commentorEmailAddress
            return $ LT.Comment person date text

        where
            commentorLogin = githubOwnerLogin $ issueCommentUser issueComment
            date = fromGithubDate $ issueCommentCreatedAt issueComment
            text = T.pack $ issueCommentBody issueComment




            



