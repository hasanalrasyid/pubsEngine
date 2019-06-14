#!/bin/bash

cat - > /tmp/diagrams-pandoc.tmp


cd xxxLocxxx
cat xxxLocxxx/app/template.hs /tmp/diagrams-pandoc.tmp > /tmp/diagrams-pandoc.hs

chmod +x /tmp/diagrams-pandoc.hs

/tmp/diagrams-pandoc.hs
rm -f /tmp/diagrams-pandoc*
