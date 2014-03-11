#!/usr/bin/env python

from __future__ import print_function

import os
import cgi
from subprocess import Popen, PIPE, STDOUT

# # Java
# SCRIPTDIR = 'javaprolog'
# SCRIPT = ['/usr/bin/java', '-cp', 'json-simple-1.1.1.jar:gnuprologjava-0.2.6.jar:.', 'Shrdlite']

# # SWI Prolog
# SCRIPTDIR = 'javaprolog'
# SCRIPT = ['/usr/local/bin/swipl', '-q', '-g', 'main,halt', '-t', 'halt(1)', '-s', 'shrdlite.pl']

# # Haskell
# SCRIPTDIR = 'haskell'
# SCRIPT = ['/usr/bin/runhaskell', 'Shrdlite.hs']

# Python
SCRIPTDIR = 'python'
SCRIPT = ['/usr/bin/python', 'shrdlite.py']

while not os.path.isdir(SCRIPTDIR):
    SCRIPTDIR = os.path.join("..", SCRIPTDIR)

print('Content-type:text/plain')
print()

try:
    form = cgi.FieldStorage()
    data = form.getfirst('data')
    script = Popen(SCRIPT, cwd=SCRIPTDIR, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    out, err = script.communicate(data)

    print(out)
    if err:
        raise Exception(err)

except:
    import sys, traceback
    print(traceback.format_exc())
    sys.exit(1)
