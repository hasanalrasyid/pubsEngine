{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Pandoc.Include.PlantUML where


import Data.Maybe

import Text.Pandoc hiding (lookupEnv)
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (callCommand, readProcess)
import Text.Pandoc.Include.Script (writeScriptResult,extractSource)
import Text.Pandoc.Include.Common
import System.Environment

includePlantUMLBlock :: Block -> IO Block
-----------------------------------------Library----------------------------------------
includePlantUMLBlock cb@(CodeBlock (label, classes@("uml":_), opts0) text) = do
  (opts,script,image) <- includePlantUML label classes opts0 text
  let lowerPartText = case lookup "src" opts of
                        Nothing -> text
                        Just f -> T.unlines $ ["//////////////"<>f<> ":", script]
  let upperPart = if elem "show" classes then [CodeBlock nullAttr lowerPartText ]
                                         else []
  let lowerPart = [Para [image]]
  return $ Div nullAttr $ upperPart <> lowerPart
includePlantUMLBlock cb = walkM includePlantUMLInline cb

includePlantUMLInline cb@(Code (label, classes@("uml":_), opts0) text) = do
  (_,_,image) <- includePlantUML label classes opts0 text
  pure image
includePlantUMLInline cb@(Image (label, classes@("uml":_), opts0) caption _) = do
  (_,_,(Image attr _ target)) <- includePlantUML label classes opts0 ""
  pure $ Image attr caption target
includePlantUMLInline cb = pure cb

genPlantUML s = T.unlines ["@startuml", s, "@enduml"]

includePlantUML label classes opts0 text = do
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
  return $ (opts, script, Image (label,[],opts) caption (T.pack fileName,label))
