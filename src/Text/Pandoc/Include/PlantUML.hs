{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Pandoc.Include.PlantUML where


import Data.Maybe

import Text.Pandoc hiding (lookupEnv)
import Text.Pandoc.Shared
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (callCommand, readProcess)
import Text.Pandoc.Include.Script (writeScriptResult,extractSource)
import Text.Pandoc.Include.Common
import Text.RawString.QQ
import System.Environment

includePlantUML :: Block -> IO Block
-----------------------------------------Library----------------------------------------
includePlantUML cb@(CodeBlock (label, classes@("uml":_), opts0) text) = do
  (script,opts) <- extractSource text opts0
  let fileName = T.unpack $ fromMaybe "uml" $ lookup "src" opts
      s = case lookup "src" opts0 of
            Nothing -> genPlantUML text
            Just t -> t
      captionText = case lookup "src" opts of
                  Nothing -> fromMaybe "" $ lookup "caption" opts0
                  Just _ -> text
  captionP <- runIO $ readMarkdown mdOption captionText
  let caption = case captionP of
                  Right (Pandoc _ c) -> blocksToInlines c
                  _ -> [Str "ERROR: PlantUML: Failed to parse caption"]
  TIO.writeFile ("_build/temp/plantuml.txt") s
  envPLANTUML_JAR <- lookupEnv "PLANTUML_JAR"
  case envPLANTUML_JAR of
    Nothing -> error "ERROR: PlantUML: environment variable PLANTUML_JAR is needed"
    Just p -> do
      callCommand $ "java -jar "<>p<>" -eps:text _build/temp/plantuml.txt"
      callCommand $ "mv _build/temp/plantuml.eps _build/auto/" <> fileName <> ".eps"
  let lowerPartText = case lookup "src" opts of
                        Nothing -> text
                        Just f -> T.unlines $ ["//////////////"<>f<> ":", script]
  let upperPart = if elem "show" classes then [CodeBlock nullAttr lowerPartText ]
                                         else []
  let lowerPart = [Para [Image (label,[],opts) caption (T.pack fileName,label) ]]
  return $ Div nullAttr $ upperPart <> lowerPart
includePlantUML cb = return cb

genPlantUML s = T.unlines ["@startuml", s, "@enduml"]

doInclude :: Block -> IO Block
doInclude (CodeBlock (label, ["uml"], opts) md) = do
  let fileName = fromMaybe "plantuml" $ lookup "src" opts
      s = genPlantUML md
  TIO.writeFile "_build/temp/plant.uml" s
  envPLANTUML_JAR <- lookupEnv "PLANTUML_JAR"
  case envPLANTUML_JAR of
    Nothing -> error "ERROR: PlantUML: environment variable PLANTUML_JAR is needed"
    Just p -> do
      callCommand $ "java -jar "<>p<>" -eps:text _build/temp/plant.uml"
      callCommand $ "mv _build/temp/plant.eps _build/auto/" <> T.unpack fileName <> ".eps"
  let caption = case lookup "caption" opts of
                  Nothing -> []
                  Just a -> [Str a]
  return $ Para [Image (label,[],opts) caption (fileName,label)]

doInclude b = pure b

