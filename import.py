#!/usr/bin/python

import sqlite3
import csv
import glob
import os
import json

class Stats:
    def __init__(self):
        self.nrepos = 0
        self.ncasts = 0
        self.nonSnapshots = 0

stats = Stats()

DBNAME='output.sqlite3'
if os.path.exists(DBNAME):
    os.remove(DBNAME)
conn = sqlite3.connect(DBNAME)
conn.text_factory = str

def getSnapshotKey(key):
    with open("query-results.json") as f:
        text = f.read()
        js = json.loads(text)
        runs = js['data']['runs']
        for run in runs:
            if run['snapshotKey'] == 'dist-' + str(key) + '-1524814812150':
                return run['snapshotKey']
        return None

def main(path):
    parts = path.split('/')

    assert parts[0] == '.', parts[0]
    assert parts[1] == "query-results", parts[1]

    host = parts[2]
    user = parts[3]
    repo = parts[4]
    id = parts[5]

    assert parts[6] == "output.csv", parts[6]

    rk = id.split(':')
    sk = getSnapshotKey(rk[1])
    print sk
    if sk is None:
        stats.nonSnapshots += 1

    conn.execute('''
    insert into repos (host, user, repo, internalid) values (?, ?, ?, ?) 
    ''', (host, user, repo, id))

    repoid = conn.execute('select last_insert_rowid()').fetchone()[0]

    with open(path) as f:
        csvf = csv.reader(f, delimiter=',', quotechar='"')
        first = True
        for row in csvf:
            if not first:
                assert len(row) == 7, len(row)
                assert row[0] == "(...)...", row[0]
                # print row
                file = row[1][len("file:///opt/src/"):]
                filep = file.split(':')
                file = filep[0]
                line = filep[1]
                if sk is not None:
                    link = 'https://lgtm.com/projects/g/' + user + '/' + repo + '/snapshot/' + sk + '/files/' + file.decode('utf8') + '?#L' + line
                else:
                    link = ""
                conn.execute('''
                insert into casts (repoid, file, s, t, pattern, link) values (?, ?, ?, ?, ?, ?)
                ''', (repoid, row[1], row[2], row[4], row[6], link))
                stats.ncasts += 1

            first = False

    conn.commit()

conn.execute('''
create table repos (
  repoid integer primary key,
  host text,
  user text,
  repo text,
  internalid text
)
''')

conn.execute('''
create table casts (
  repoid integer references repos(repoid),
  file text,
  s text,
  t text,
  pattern text,
  link text
)
''')

conn.commit()

for path in glob.iglob('./*/*/*/*/*/output.csv'):
    stats.nrepos += 1
    print stats.nrepos, "Importing", path,
    main(path)

print "Total files/repos imported:", stats.nrepos
print "Total casts rows imported:", stats.ncasts
print "Total nonSnapshots:", stats.nonSnapshots
