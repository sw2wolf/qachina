import urllib2
import json

url = "http://192.168.1.2:8000/handle"
json_dict = { 'args' : "f1(S)", 'method' : 'eval', 'password' : 'blah' }
jdata = json.dumps( json_dict )
headers = {}
headers['Content-Type'] = 'application/json'
print jdata
req = urllib2.Request(url, jdata, headers)
res = urllib2.urlopen(req)
for line in res:
    print json.loads(line)
