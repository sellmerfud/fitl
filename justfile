
[private]
default:
  @just --list --unsorted --justfile {{justfile()}}
  
# Show current version number
showvers:
  #! /usr/bin/env python3
  import re
  with open('build.sbt') as f:
    contents = f.read()
    match re.search(r'^\s*version\s*:=\s*"([^"]+)"', contents, re.MULTILINE):
      case None:
        print("Cannot determine current version!")
      case match:
        print(f"{match[1]}")

# Package and release a new version
@release *ARGS:
  python3 scripts/release.py {{ARGS}}



