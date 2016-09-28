#!/bin/sh
cat $1 | curl -s -X POST -H "Content-Type:text/plain" --data-binary @- http://localhost:9000/plantuml/png
