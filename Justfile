#!/usr/bin/env -S just --justfile

default:
    @just --list

# Build Haddock tarball for Hackage
docs:
    stack haddock --haddock-for-hackage

[private]
check-token:
    @test -n "${HACKAGE_KEY:-}" || { echo "HACKAGE_KEY is not set" >&2; exit 1; }

# Upload package and docs as a Hackage candidate
candidate: check-token docs
    stack upload --candidate --test-tarball .
    stack upload --candidate --documentation .

# Publish package and docs to Hackage
release: check-token docs
    stack upload --test-tarball .
    stack upload --documentation .
