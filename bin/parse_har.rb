require 'rubygems';
require 'json';
j = JSON.parse(IO.read(ARGV[0]))
e = j['log']['entries']
puts e.size;
e.size.times {|i|
   r = e[i]['request']
   resp = e[i]['response']
   printf "%s %s\n", r["method"], r["url"]
   printf "cookies:\n"
   c = r["headers"].find {|m| m["name"] == "Cookie"}
   c && c["value"].split(";").map {|m| puts m; }
   val = resp["headers"].find {|m| (m["name"] == "Via") }
   printf "%s %s\n", resp["status"],val ? val["value"] : ""
}
