#!/bin/bash

make -C haskell/
python -m CGIHTTPServer 8000
