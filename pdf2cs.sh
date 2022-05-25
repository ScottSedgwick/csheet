#!/bin/bash
cabal exec pdf -- -p $1.dndb.pdf > $1.json
cabal exec cexe $1