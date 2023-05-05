{-# language
    BangPatterns
  , BlockArguments
  , ConstraintKinds
  , DataKinds
  , DeriveAnyClass
  , DeriveFoldable
  , DeriveFunctor
  , DeriveGeneric
  , DeriveLift
  , DeriveTraversable
  , DerivingStrategies
  , DerivingVia
  , DuplicateRecordFields
  , EmptyCase
  , ExplicitForAll
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GADTs
  , GeneralisedNewtypeDeriving
  , ImportQualifiedPost
  , InstanceSigs
  , LambdaCase
  , MonoLocalBinds
  , MultiParamTypeClasses
  , MultiWayIf
  , NamedFieldPuns
  , NumericUnderscores
  , OverloadedLabels
  , OverloadedRecordDot
  , OverloadedStrings
  , PackageImports
  , PatternGuards
  , QuasiQuotes
  , RankNTypes
  , RecordWildCards
  , RoleAnnotations
  , ScopedTypeVariables
  , StandaloneDeriving
  , StrictData
  , TemplateHaskell
  , TupleSections
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , ViewPatterns
#-}

module GenSha256Map (main) where

import "Cabal" Distribution.Types.SourceRepo (RepoType(..), KnownRepoType(..))
import "aeson" Data.Aeson (FromJSON)
import "aeson" Data.Aeson qualified as Aeson
import "async" Control.Concurrent.Async (forConcurrently)
import "base" Control.Applicative (optional)
import "base" Control.Monad (forM_, when)
import "base" Data.Char qualified as Char
import "base" Data.Either (partitionEithers)
import "base" Data.Foldable (foldrM)
import "base" Data.List qualified as List
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.List.NonEmpty qualified as NE
import "base" Data.Maybe (fromMaybe, mapMaybe)
import "base" GHC.Generics (Generic)
import "base" Prelude hiding (log)
import "base" System.Exit (ExitCode(..))
import "base" System.Exit (exitFailure)
import "base" System.IO qualified as IO
import "bytestring" Data.ByteString qualified as BS
import "bytestring" Data.ByteString.Builder (Builder)
import "bytestring" Data.ByteString.Builder qualified as Builder
import "bytestring" Data.ByteString.Lazy qualified as LBS
import "cabal-install-parsers" Cabal.Parse as Cabal
import "cabal-install-parsers" Cabal.Project (Project(..))
import "cabal-install-parsers" Cabal.Project qualified as Cabal
import "cabal-install-parsers" Cabal.SourceRepo (SourceRepositoryPackage(..))
import "directory" System.Directory (doesFileExist, doesDirectoryExist)
import "filepath" System.FilePath ((</>))
import "optparse-applicative" Options.Applicative qualified as O
import "process" System.Process (readProcessWithExitCode)
import "text" Data.Text (Text)
import "text" Data.Text qualified as Text
import "text" Data.Text.IO qualified as Text
import "text" Data.Text.Lazy qualified as TextLazy
import "text" Data.Text.Lazy.Builder qualified as TextBuilder

data Sha256Entry = Sha256Entry
  { location :: Text
  , tag :: Text
  , sha256 :: Text
  }
  deriving stock (Eq, Ord)

encodeSha256Entry :: Sha256Entry -> (TextBuilder.Builder, TextBuilder.Builder)
encodeSha256Entry entry =
  let key = mconcat
        [ inQuotes entry.location
        , char '.'
        , inQuotes entry.tag
        ]
      value = mconcat
        [ inQuotes entry.sha256
        ]
  in (key, value)
  where
    char = TextBuilder.singleton
    inQuotes txt = "\"" <> TextBuilder.fromText txt <> "\""

encodeSha256Map :: [Sha256Entry] -> Text
encodeSha256Map entries = build $ mconcat
  [ "{\n"
  , mconcat $ List.intersperse "\n" $ List.map (singleEntry . encodeSha256Entry) entries
  , "}"
  ]
  where
    build = TextLazy.toStrict . TextBuilder.toLazyText

    indents :: TextBuilder.Builder
    indents = "  "

    singleEntry (key, value) = mconcat
      [ indents, key, " =\n"
      , indents, indents, value, ";\n"
      ]

-- We really only need the sha256 field, but extra stuff doesn't hurt
data NixPrefetchGitOutput = NixPrefetchGitOutput
  { url :: Text
  , rev :: Text
  , sha256 :: Text
  , date :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

data NixPrefetchGitError
  = CommandFailed
      { exitCode :: Int
      , stderr :: Text
      }
  | DecodingFailed
      { aesonError :: Text
      , stdout :: Text
      }
  | NonAsciiOutput
      { stdout :: Text
      }

displayNixPrefetchGitError :: NixPrefetchGitError -> Text
displayNixPrefetchGitError = \case
  CommandFailed { exitCode, stderr } -> Text.concat
    [ "nix-prefetch-git failed with exitcode "
    , Text.pack (show exitCode)
    , ". Stderr was:\n  "
    , stderr
    ]
  DecodingFailed { aesonError, stdout } -> Text.concat
    [ "Decoding the output of nix-prefetch-git failed.\n"
    , "Aeson error was:\n  "
    , aesonError, "\n"
    , "Stdout was:\n  "
    , stdout
    ]
  NonAsciiOutput { stdout } -> Text.concat
    [ "Stdout of nix-prefetch-git contained non-ascii.\n"
    , "Stdout was:\n  "
    , stdout
    ]

--nixPrefetchUrl :: [Text] -> Text -> IO (Either NixPrefetchUrlError NixPrefetchUrlOutput)
--nixPrefetchUrl = undefined

nixPrefetchGit :: [Text] -> Text -> IO (Either NixPrefetchGitError NixPrefetchGitOutput)
nixPrefetchGit extraArgs prefetchUrl = do
  (exitCode, stdout, stderr) <- do
    readProcessWithExitCode "nix-prefetch-git" (List.map Text.unpack extraArgs ++ [Text.unpack prefetchUrl]) ""
  case exitCode of
    ExitFailure e -> do
      pure (Left (CommandFailed e (Text.pack stderr)))
    ExitSuccess -> do
      case stringToAscii stdout of
        Nothing -> do
          pure (Left (NonAsciiOutput (Text.pack stdout)))
        Just npgOutput -> do
          case Aeson.eitherDecode' npgOutput of
            Left aesonErr -> do
              pure (Left (DecodingFailed (Text.pack aesonErr) (Text.pack stdout)))
            Right npg -> do
              pure (Right npg)

buildSha256Map :: [Srp] -> IO ([Error], [Sha256Entry], [Srp])
buildSha256Map srps0 = do
  (errors, entriesAndSrps) <- partitionEithers <$> forConcurrently srps0 getPackageEntry
  let (entries, srps1) = List.unzip entriesAndSrps
  pure (errors, entries, srps1)

data Error
  = NixPrefetchGit NixPrefetchGitError
  | UnsupportedRepoType (Either KnownRepoType Text)
  | MissingTag { location :: Text }
  | EmptyGrouping

main :: IO ()
main = do
  cabalProjectLocation <- getCabalProjectLocation
  (cabalProjectFile, cabalProjectContents) <- getCabalProject cabalProjectLocation

  case Cabal.parseProject cabalProjectFile cabalProjectContents of
    Left parseErr -> do
      log (Text.pack (Cabal.renderParseError parseErr))
      exitFailure
    Right project -> do
      srps <- do
        case fanIn project.prjSourceRepos of
          Left err -> do
            log (displayError err)
            exitFailure
          Right srps -> do
            pure srps
      (errors, entries, srpsWithSha) <- buildSha256Map srps

      when (not (null errors)) $ do
        log "The following (non-fatal) errors occurred: "
        forM_ errors (\err -> log (displayError err))

      output (encodeSha256Map entries)

      output (encodeCabalProject srpsWithSha)

displayError :: Error -> Text
displayError = \case
  NixPrefetchGit npge ->
    displayNixPrefetchGitError npge
  UnsupportedRepoType rt -> case rt of
    Left krt -> Text.concat
      [ "Unsupported (known) repo type: "
      , Text.pack (show krt)
      ]
    Right urt -> Text.concat
      [ "Unsupported (unknown) repo type: "
      , urt
      ]
  MissingTag { location } -> Text.concat
    [ "Missing tag on ", location
    ]
  EmptyGrouping -> Text.concat
    [ "There was an internal error when performing fanIn. Please report this."
    ]

getPackageEntry :: Srp -> IO (Either Error (Sha256Entry, Srp))
getPackageEntry srp = do
  case srp.typ of
    KnownRepoType krt -> case krt of
      Git -> do
        let revArgs = ["--rev", srp.tag]
        -- Text.hPutStrLn IO.stderr $ "Running nix-prefetch-git on " <> srp.location <> " @ " <> srp.tag
        e <- nixPrefetchGit revArgs srp.location
        case e of
          Left err -> do
            pure (Left (NixPrefetchGit err))
          Right npg -> do
            pure (Right ((Sha256Entry {
              location = srp.location,
              tag = srp.tag,
              sha256 = npg.sha256,
              ..
            }), srp { sha256 = Just npg.sha256 }))
      otherKrt -> do
        pure (Left (UnsupportedRepoType (Left otherKrt)))
    OtherRepoType urt -> do
      pure (Left (UnsupportedRepoType (Right (Text.pack urt))))

data CabalProjectLocation
  = LocalFs
      { path :: FilePath
      }
  | GitRepo
      { _url :: Text
      , _subdir :: Maybe FilePath
      , _cabalProjectFileName :: Maybe FilePath
      }

defaultCabalProjectLocation :: CabalProjectLocation
defaultCabalProjectLocation = LocalFs "."

-- TODO: this works, but the help text is really messed up
getCabalProjectLocation :: IO CabalProjectLocation
getCabalProjectLocation = do
  let cabalProjectFileNameParser = optional
        (O.strOption
          (O.long "file-name" <> O.metavar "PATH" <> O.help "The file name of the cabal.project to look for")
        )

  let gitParser = GitRepo
        <$> O.strOption
              (O.long "url" <> O.metavar "URL" <> O.help "Git URL")
        <*> optional (O.strOption
              (O.long "subdir" <> O.metavar "PATH" <> O.help "subdirectory to look for the cabal.project in"))
        <*> cabalProjectFileNameParser

  let localFsParser = LocalFs
        <$> O.strOption
              (O.long "path" <> O.metavar "PATH" <> O.help "local path to look for cabal.project. If the argument supplied is a directory, it will look for `cabal.project`; otherwise, if the argument supplied is a file, it will use that as the cabal.project. Default to the working directory." <> O.value ".")

  let parser = O.optional $ O.hsubparser
        (  O.command "git" (O.info gitParser (O.progDesc "Find the cabal.project in a git repo"))
        <> O.command "local" (O.info localFsParser (O.progDesc "Find the cabal.project on the local filesystem"))
        )

  mloc <- O.execParser (O.info (parser O.<**> O.helper) (O.fullDesc <> O.progDesc "Generate a Haskell.Nix sha256map from a cabal.project" <> O.header "gen-sha256map"))
  pure $ fromMaybe defaultCabalProjectLocation mloc

data FileStatus
  = IsDirectory
  | IsFile
  | DoesNotExist

getFileStatus :: FilePath -> IO FileStatus
getFileStatus fp = do
  is_directory <- doesDirectoryExist fp
  is_file <- doesFileExist fp
  if | is_directory -> pure IsDirectory
     | is_file -> pure IsFile
     | otherwise -> pure DoesNotExist

-- TODO: make this actually do something
getCabalProject :: CabalProjectLocation -> IO (FilePath, BS.ByteString)
getCabalProject = \case
  LocalFs { path } -> do
    path_fs <- getFileStatus path
    projectFile <- case path_fs of
      IsDirectory -> do
        let proj = path </> "cabal.project"
        log $ "cabal project filename not specified; assuming " <> Text.pack proj
        pure proj
      IsFile -> do
        let proj = path
        log $ "cabal project filename specified; looking at " <> Text.pack path
        pure proj
      DoesNotExist -> do
        log $ "The supplied path, \"" <> Text.pack path <> "\" does not exist."
        exitFailure
    projectContent <- BS.readFile projectFile
    pure (projectFile, projectContent)
  -- TODO: make this do the right thing
  GitRepo{} -> do
    log "Git repos currently unsupported; It is recommended to clone the repo."
    exitFailure
    --let cabalProjectFile = "cabal.project"
    --cabalProjectContents <- BS.readFile cabalProjectFile
    --pure (cabalProjectFile, cabalProjectContents)

stringToAscii :: String -> Maybe LBS.ByteString
stringToAscii = fmap build . foldrM go mempty
  where
    build :: Builder -> LBS.ByteString
    build = Builder.toLazyByteString

    go :: Char -> Builder -> Maybe Builder
    go c b
      | Char.isAscii c = pure (Builder.char7 c <> b)
      | otherwise = Nothing

fanIn :: [SourceRepositoryPackage Maybe] -> Either Error [Srp]
fanIn = traverse combine . List.groupBy isSameSansSubdir
  where
    -- they have been grouped, so we assume they're all equivalent
    -- sans subdir
    combine :: [SourceRepositoryPackage Maybe] -> Either Error Srp
    combine [] = Left EmptyGrouping
    combine srps@(srp : _) = do
      tag <- case srp.srpTag of
        Nothing -> do
          Left $ MissingTag
            { location = Text.pack srp.srpLocation
            }
        Just tag -> do
          pure $ Text.pack tag
      pure $ Srp
        { typ = srp.srpType
        , location = addDotGit $ Text.pack srp.srpLocation
        , tag
        , sha256 = Nothing
        , subdirs = NE.nonEmpty (gatherSubdirs srps)
        }

    gatherSubdirs :: [SourceRepositoryPackage Maybe] -> [FilePath]
    gatherSubdirs = mapMaybe (\srp -> srp.srpSubdir)

    isSameSansSubdir :: SourceRepositoryPackage f -> SourceRepositoryPackage f -> Bool
    isSameSansSubdir srp1 srp2 =
      List.and
        [ srp1.srpType == srp2.srpType
        , srp1.srpLocation == srp2.srpLocation
        , srp1.srpTag == srp2.srpTag
        , srp1.srpBranch == srp2.srpBranch
        ]

-- Less annoying SourceRepositoryPackage
data Srp = Srp
  { typ :: RepoType
  , location :: Text
  , tag :: Text
  , sha256 :: Maybe Text
  , subdirs :: Maybe (NonEmpty FilePath)
  }

addDotGit :: Text -> Text
addDotGit txt
  | ".git" `Text.isSuffixOf` txt = txt
  | otherwise = txt <> ".git"

-- encode Srps to a cabal.project snippet of source-repository-projects
encodeCabalProject :: [Srp] -> Text
-- TODO: make this use text builder
encodeCabalProject = Text.unlines . List.map encodeSrp
  where
    encodeRepoType :: RepoType -> Text
    encodeRepoType = \case
      KnownRepoType krt -> Text.pack (List.map Char.toLower (show krt))
      OtherRepoType ort -> Text.pack (List.map Char.toLower ort)

    encodeSrp :: Srp -> Text
    encodeSrp srp =
      let mainBody = Text.unlines
            [ "source-repository-package"
            , "  type: " <> encodeRepoType srp.typ
            , "  location: " <> srp.location
            , "  tag: " <> srp.tag
            , maybe "" (\sha -> "  --sha256: " <> sha) srp.sha256
            ]
          subdirs = case srp.subdirs of
            Nothing -> Text.empty
            Just sds -> Text.unlines
              [ "  subdir:"
              , Text.unlines (NE.toList (NE.map (\path -> "    " <> Text.pack path) sds))
              ]
      in mainBody <> subdirs

log :: Text -> IO ()
log = Text.hPutStrLn IO.stderr

output :: Text -> IO ()
output = Text.hPutStrLn IO.stdout
