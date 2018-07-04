module Main where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr, for_, intercalate)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.NonEmpty (nes)
import Data.String.NonEmpty as NonEmptyString
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (error, log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdir, readdir, readTextFile, stat, writeTextFile) as FS
import Node.FS.Stats (isFile) as FS
import Node.Platform (Platform(..))
import Node.Process as Process
import Pathy.Name (Name(..), extension)
import Pathy.Parser (parseAbsDir, posixParser)
import Pathy.Path (dir, dir', file', (</>))
import Pathy.Printer (Printer, posixPrinter, printPath, windowsPrinter)
import Pathy.Sandboxed (sandbox)
import Text.Markdown.SlamDown (Block(..), CodeBlockType(..), SlamDown, SlamDownP(..))
import Text.Markdown.SlamDown.Parser (parseMd)

type Module =
  { name :: String
  , dirs :: Array String
  , fileName :: String
  , contents :: String
  }

type State = Aff (Either String (Array Module))

printer :: Printer
printer
  | Process.platform == Just Win32 = windowsPrinter
  | otherwise = posixPrinter

-- TOD0
-- * Recursively find files
-- * Don't discard errors
main :: Effect (Fiber Unit)
main = launchAff do
  cwdString <- liftEffect Process.cwd

  let
    cwd = parseAbsDir posixParser $ cwdString <> "/"
    genSrcDir = dir (SProxy :: SProxy "generated-src")
    sandboxedDir = join $ sandbox <$> cwd <@> genSrcDir
    printedDir = printPath printer <$> sandboxedDir

  for_ printedDir \dirPath -> do
    dirExists <- FS.exists dirPath
    unless dirExists $ FS.mkdir dirPath

    contents <- FS.readdir "."
    modules <- Array.foldr step (pure $ Right []) contents

    case modules of
      Left e -> liftEffect do
        error e
        Process.exit 1
      Right ms -> do
        for_ ms \m -> do
          liftEffect $ log $ "Writing " <> m.name

          for_ m.dirs \d -> do
            let
              printedSubDir = do
                d' <- NonEmptyString.fromString d
                let subDir = genSrcDir </> (dir' $ Name d')
                sandboxedSubDir <- join $ sandbox <$> cwd <@> subDir
                pure $ printPath printer sandboxedSubDir

            for_ printedSubDir \subDirPath -> do
              subDirExists <- FS.exists subDirPath
              unless subDirExists $ FS.mkdir subDirPath

          let
            printedFilePath :: Maybe String
            printedFilePath = do
              fn <- NonEmptyString.fromString m.fileName
              let filePart = file' $ Name fn

              path <- m.dirs # flip foldr (pure filePart) \dirPart state -> do
                state' <- state
                name <- NonEmptyString.fromString dirPart
                let dirName = dir' $ Name name
                pure $ dirName </> state'

              sandboxedPath <- join $ sandbox <$> cwd <@> genSrcDir </> path
              pure $ printPath printer sandboxedPath

          for_ printedFilePath \filePath ->
            FS.writeTextFile UTF8 filePath m.contents

        liftEffect $ Process.exit 0

  where

  step :: String -> State -> State
  step file state = do
    prevState <- state
    stat <- FS.stat file

    let
      mdFile
        | FS.isFile stat == false = Nothing
        | Just f <- NonEmptyString.fromString file
        , Just ext <- extension $ Name f
        , ext == nes (SProxy :: SProxy "md")
          = Just file
        | otherwise = Nothing

    maybe state (map (map identity) <<< toModules) mdFile

  toModules :: String -> State
  toModules file = do
    text <- FS.readTextFile UTF8 file

    let
      md :: Either String SlamDown
      md = parseMd text

      modules = md # map \(SlamDown blocks) ->
        blocks # flip foldr [] \block state ->
          case block of
            CodeBlock (Fenced false "purescript") lines -> fromMaybe state do
              firstLine <- List.head lines
              moduleName <- String.split (Pattern " ") firstLine !! 1
              parts <- Array.unsnoc $ String.split (Pattern ".") moduleName
              dirs <- Array.unsnoc parts.init

              let
                moduleRep =
                  { name: moduleName
                  , dirs: parts.init
                  , fileName: parts.last <> ".purs"
                  , contents: intercalate "\n" lines
                  }

              pure $ [ moduleRep ] <> state
            _ ->
              state

    pure modules
