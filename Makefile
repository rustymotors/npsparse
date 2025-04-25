# Makefile for building and running the Haskell app

.PHONY: all build run clean

# Default target
all: build

# Build the Haskell project
build:
	cabal build

# Run the Haskell app
run:
	cabal run npsparse

# Clean the build artifacts
clean:
	cabal clean