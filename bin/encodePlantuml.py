#!/usr/bin/python
import sys
import plantuml

pl = plantuml.deflate_and_encode(sys.stdin.read())
print(pl)
