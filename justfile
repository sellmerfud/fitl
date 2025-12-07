
[private]
default:
  @just --list --unsorted --justfile {{justfile()}}
  
# Show current version number
@showvers:
  grep '^\s*version' build.sbt

# Package and release a new version
@release *ARGS:
  python3 scripts/release.py {{ARGS}}



