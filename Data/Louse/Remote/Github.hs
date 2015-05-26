module Data.Louse.Remote.Github(getIssues, makeGithub) where

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
    import qualified Data.Map as M
    import Control.Concurrent.STM.TVar
    import Control.Concurrent.STM
    import Control.Concurrent.Async
    import Control.Monad
    import Data.List.Split

    type Owner = String
    type Repository = String
    type EmailCache = M.Map String T.Text


    data Github = Github {owner :: Owner, repository :: Repository, authentication :: Maybe GithubAuth, emailCache :: TVar EmailCache } 

    instance RemoteRepository Github where

        getIssues github = 
                do
                    issues <- liftIO $ issuesForRepo' auth user repo []
                    case issues of
                        Left err -> liftIO $ putStrLn "Failed at repo" >> (fail . show) err
                        Right issues -> do
                            let source = L.sourceList $ chunk 20 issues
                            let conduit = L.concatMapM (issuesToBugs github)
                            source $= conduit
                where
                    Github user repo auth _ = github

    makeGithub :: Owner -> Repository -> Maybe GithubAuth -> IO Github
    makeGithub owner repository auth = fmap (Github owner repository auth) $ newTVarIO M.empty

    issuesToBugs :: Github -> [Issue] -> IO [LT.Bug]
    issuesToBugs github = mapConcurrently (issueToBug github)

    issueToBug :: Github -> Issue -> IO LT.Bug
    issueToBug github issue = 
        do
            (githubComments, emailAddress) <-
                if issueComments issue > 0
                    then
                     concurrently (getComments github (issueNumber issue)) (getUserEmail github owner)
                    else
                        fmap (\email -> ([], email)) $ getUserEmail github owner

            let reporter = LT.Person (T.pack owner) emailAddress

            return $ LT.Bug reporter bugCreationDate bugTitle bugDescription bugOpen githubComments

        where
            (Github user repo auth _) = github
            bugCreationDate = fromGithubDate . issueCreatedAt $ issue
            bugTitle = T.pack . issueTitle $ issue
            bugDescription = maybe (T.pack "") (T.pack) $ issueBody issue
            bugOpen = isNothing $ issueClosedBy issue

            owner = (githubOwnerLogin . issueUser) issue

    getUserEmail :: Github -> String -> IO T.Text
    getUserEmail (Github _ _ auth cache) userName = 
        do
            cacheResult <- atomically $ do
                userMap <- readTVar cache
                return $ M.lookup userName userMap

            case cacheResult of
                Just cacheHit -> return cacheHit
                Nothing ->
                    do
                        result <- userInfoFor' auth userName
                        case result of
                            Left err -> putStrLn "e-mail fail" >> (fail . show) err
                            Right owner ->
                                atomically $ do
                                    let email = maybe "" T.pack $ detailedOwnerEmail owner
                                    emailCache <- readTVar cache

                                    when (M.notMember userName emailCache) $
                                        writeTVar cache (M.insert userName email emailCache)

                                    return email

    getComments :: Github -> Int -> IO [LT.Comment]
    getComments github issueId = 
        do
            result <- comments' auth user repository issueId
            case result of
                Left err -> putStrLn ("comments fail " ++ (show issueId)) >> (fail . show) err
                Right githubComments -> mapConcurrently (toLouseComment github) githubComments
        where
            Github user repository auth _ = github

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




            



