# Compare directories

from filecmp import dircmp
import sys, time

dira = sys.argv[1]
dirb = sys.argv[2]

stack = [dircmp(dira, dirb)]
count = 0

while stack:
    d = stack.pop()
    if d.left_only:
        print "Only in", d.left, d.left_only
    if d.right_only:
        print "Only in", d.right, d.right_only
    if d.diff_files:
        print "Files differ in", d.left, d.right, d.diff_files
    if d.common_funny:
        print "File type mismatch in", d.left, d.right, d.common_funny
    if d.funny_files:
        print "File stat problems", d.left, d.right, d.funny_files
    stack.extend(val for key, val in reversed(sorted(d.subdirs.items())))
    count += 1
    if not (count % 1000): print time.ctime(), "Working on", d.left, d.right
