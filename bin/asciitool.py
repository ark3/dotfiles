#!/usr/bin/env python
# Help convert UTF8 to ASCII

import os
import sys

map = {
    "\xe2\x80\x93": "-",      # n dash
    "\xe2\x80\x94": "--",     # m dash
    "\xe2\x80\x99": "'",
    "\xe2\x80\x9c": '"',
    "\xe2\x80\x9d": '"',
    "\xe2\x80\xa6": "...",
    "\xe2\x80\xaf": " ",      # odd sort of space?
    "\xe2\x80\xa8": " ",      # thin space?
    "\xc2\xa0":     " ",      # another weird space?
    "\xc3\xa9":     "e",      # e.g., protege
    "\xc2\xb7":     "*",      # Bullet
}


def main():
    filename = sys.argv[1]
    in_data = open(filename).read()

    data = in_data
    for key, value in map.viewitems():
        data = data.replace(key, value)

    try:
        data.encode("US-ASCII")
    except UnicodeDecodeError as exc:
        problem = data[exc.start - 10:exc.end + 10]
        print repr(problem)
        for ch in problem:
            print "%3d %s" % (ord(ch), repr(ch))
        return

    if data == in_data:
        print "No changes"
        return

    os.rename(filename, filename + ".orig")
    with open(filename, "wb") as outf:
        outf.write(data)


if __name__ == "__main__":
    main()
