#!/usr/bin/python

import sqlite3
import csv
import glob
import os
import sys

class Stats:
    def __init__(self):
        self.nprojects = 0
        self.nobs = 0

stats = Stats()

dir = sys.argv[1]

DBNAME=dir + '/.output.sqlite3'
if os.path.exists(DBNAME):
    os.remove(DBNAME)
conn = sqlite3.connect(DBNAME)
conn.text_factory = str

cols = []

def main(path):
    print "Importing", path,
    filename = os.path.basename(path)
    project, _ = os.path.splitext(filename)

    with open(path) as f:
        csvf = csv.reader(f, delimiter=',', quotechar="\"")
        nrows = 0
        for row in csvf:
            if nrows == 0:
                if len(row) > 0:
                    if len(cols) == 0:
                        cols.extend(row)
                        s = ",".join(map(lambda c: c + " text", cols))
                        print "[Columns:", s, "]",
                        conn.execute('create table obs (project text,' + s + ')')
                        conn.commit()
                    else:
                        assert row == cols
                else:
                    print "[Empty project]",
            else:
                assert len(row) == len(cols), str(row) + ": " + str(len(row))
                s = ",".join(cols)
                t = [project]
                t.extend(row)
                conn.execute('insert into obs (project, '+s+') values (? '+',?'*len(row)+')', t)
                stats.nobs += 1
            
            nrows += 1

        print "[Rows imported " + str(nrows - 1) + "]"

    conn.commit()

for path in glob.iglob(dir + '/*.csv'):
    main(path)
    stats.nprojects += 1

print "Total files/projects imported:", stats.nprojects
print "Total obs/rows imported:", stats.nobs
