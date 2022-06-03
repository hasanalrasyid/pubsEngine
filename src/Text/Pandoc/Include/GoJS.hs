{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Text.Pandoc.Include.GoJS where


import Data.Maybe

import Text.Pandoc
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (callCommand, readProcess)
import Text.Pandoc.Include.Script (writeScriptResult,extractSource)

import Text.RawString.QQ

includeGoJS :: Block -> IO Block
-----------------------------------------Library----------------------------------------
includeGoJS cb@(CodeBlock (label, classes@("gojs":_), opts0) text) = do
  (script,opts) <- extractSource text opts0
  let fileName = T.unpack $ fromMaybe "gojs" $ lookup "file" opts
      s = genGoJS script
  TIO.writeFile ("_build/temp/index.js") s
  nodePath <- fmap (head . lines) $ readProcess "zsh" [] $ "npm config get prefix"
  --res <- readProcess "zsh" [] $ "NODE_PATH=/home/aku/.nvm/versions/node/v10.24.0/lib/node_modules:\$NODE_PATH node _build/temp/index.js"
  callCommand $ "NODE_PATH="<> nodePath <> "/lib/node_modules:$NODE_PATH node _build/temp/index.js"
  callCommand $ "pdfcrop _build/temp/goJSTemp.pdf _build/auto/" <> fileName <> ".pdf"
  let lowerPartText = case lookup "src" opts of
                        Nothing -> text
                        Just f -> T.unlines $ ["//////////////"<>f<> ":", script]
  let upperPart = if elem "show" classes then [CodeBlock nullAttr lowerPartText ]
                                         else []
  lowerPart <- writeScriptResult "" (label,["script","golang","img"],opts)
  return $ Div nullAttr $ upperPart <> [lowerPart]
includeGoJS cb = return cb

genGoJS s = goJSHead <> s <> goJSTail

goJSHead = [r|#!/usr/bin/env node

const puppeteer = require('puppeteer');
const fs = require('fs');

var PDFDocument = require('pdfkit'),
    SVGtoPDF = require('svg-to-pdfkit');

(async () => {
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--disable-setuid-sandbox'],
    headless: true
    // headless: false,
    // devtools: true
  });

  const page = await browser.newPage();
  page.on('console', (msg) => console.log(msg.text()));

  page.setContent('<div id="myDiagramDiv"></div>');

  await page.addScriptTag({
  url: 'https://unpkg.com/gojs'
     //url: 'https://gojs.net/2.0.14/release/go.js'
    // path: 'node_modules/gojs/release/go.js'
  });

  //console.log(r.test);
  const results = await page.evaluate( () => {
    var $ = go.GraphObject.make;

    |]
goJSTail = [r|

    const getIMG = () => {
      return new Promise((resolve, reject) => {
        myDiagram.makeImageData({
          background: 'red',
          size: new go.Size(300, NaN),
          callback: function(data) {
            resolve(data);
          }
        });
      })
    }

    async function fetchIMG() {
      return await getIMG();
    }

    const getSVG = () => {
        return new Promise((resolve, reject) => {
          myDiagram.makeSVG({
            size: new go.Size(300, NaN),
            callback: function(svgdata) {
              let svgstr = new XMLSerializer().serializeToString(svgdata);
              //console.log(svgstr)
              resolve(svgstr)
            }
          });
        })
    }

    async function fetchSVG() {
      return await getSVG();
    }

    return fetchSVG();
  });

  const doc = new PDFDocument();
  doc.pipe(fs.createWriteStream('_build/temp/goJSTemp.pdf'));
  SVGtoPDF(doc, results, 0, 0);
  doc.end();

fs.writeFileSync('_build/temp/goJSTemp.svg', results);

 await browser.close();
})();
    |]


  {-
    var myDiagram = $(go.Diagram, "myDiagramDiv",
      {
        "animationManager.isEnabled": false,
        "undoManager.isEnabled": true  // enable undo & redo
      });

      // define a simple Node template
    myDiagram.nodeTemplate =
        $(go.Node, "Auto",  // the Shape will go around the TextBlock
          $(go.Shape, "RoundedRectangle", { strokeWidth: 0 },
            // Shape.fill is bound to Node.data.color
            new go.Binding("fill", "color")),
          $(go.TextBlock,
            { margin: 8 },  // some room around the text
            // TextBlock.text is bound to Node.data.key
            new go.Binding("text", "key"))
        );

      // create the model data that will be represented by Nodes and Links
      myDiagram.model = new go.GraphLinksModel(
        [
          { key: "Alpha", color: "lightblue" },
          { key: "Beta", color: "orange" },
          { key: "Gamma", color: "lightgreen" },
          { key: "Delta", color: "pink" }
        ],
        [
          { from: "Alpha", to: "Beta" },
          { from: "Alpha", to: "Gamma" },
          { from: "Beta", to: "Beta" },
          { from: "Gamma", to: "Delta" },
          { from: "Delta", to: "Alpha" }
        ]);
-}
