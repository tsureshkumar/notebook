import csv
import os,sys
import json


with open(sys.argv[1], "r") as f:
    rdr = csv.reader(f)
    all = [r for r in rdr]
    header, content = all[0], all[1:]

for row in content:
   j = {}
   for i in range(0,len(row)):
       if header[i] == '_raw':
           j[header[i]] = json.loads(row[i])
       else:
           j[header[i]] = row[i]
   print(json.dumps(j))



