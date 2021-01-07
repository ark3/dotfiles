#! /usr/bin/env python

usage = """
Usage: renamer OLDNAMES NEWNAMES

Reads two lists of filenames from the files specified on the command line.
These two lists should be the same length. Renames files listed in the first
list as the filenames in the corresponding position in the second list.
"""

import sys
import os

if len(sys.argv) != 3:
    print(usage)
    exit(3)

onames = open(sys.argv[1]).readlines()
nnames = open(sys.argv[2]).readlines()

if len(onames) != len(nnames):
    print(f"Found {len(onames)} entries in {sys.argv[1]}")
    print(f"Found {len(nnames)} entries in {sys.argv[2]}")
    print("The lists of filenames must match in length.")
    exit(1)

for oname, nname in zip(onames, nnames):
    oname = oname[:-1]
    nname = nname[:-1]
    if oname and nname and oname != nname:
        print(f"{oname} --> {nname}")
        try:
            os.rename(oname, nname)
        except os.error as exc:
            print(f"failed {exc}")
