#! /usr/bin/env python

# Reads two lists of filenames from the files specified on the command line.
# These two lists should be the same length. Renames files listed in the first
# list as the filenames in the corresponding position in the second list.

import sys
import os

onames = open(sys.argv[1], "read").readlines()
nnames = open(sys.argv[2], "read").readlines()

if len(onames) != len(nnames):
    print "len onames = %d, len nnames = %d" % (len(onames), len(nnames))
    print "The lists of filenames must match in length."
    sys.exit(1)

for idx in range(len(onames)):
    oname = onames[idx][:-1]
    nname = nnames[idx][:-1]

    if oname != nname:
        print "%s --> %s" % (oname, nname)
        try:
            os.rename(oname, nname)
        except os.error as exc:
            print "failed", exc
