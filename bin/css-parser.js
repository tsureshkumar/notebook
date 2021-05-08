const fs = require('fs');
const data = fs.readFileSync(0, 'utf-8').trim();

const keywords = ['connect-src', 'object-src','style-src', 'default-src', 'script-src', 'frame-ancestors'];
const newl = "\n";
const tab = "   ";

function parseDirective(text, indent="") {
    if(!text) return "";
    let val = text.split(" ");
    let res = "";
    for(i=0; i<val.length; i++) {
        if(i==0) res += indent + val[i].trim() + newl;
        else res += indent+tab+val[i].trim() + newl;
    }
    res += indent + tab + ";";
    return res;
}

function parseDirectives(text) { 
    let res =  text.trim().split(';').map(x => parseDirective(x.trim(), tab))
    return res.join("\n");
}

function parseHeader(text) {
    let re = /^([a-zA-Z\\-]+)\s*:\s*(.*)/;
    let found = text.match(re);
    let v = text;
    if(found) {
        let [all,k,v2] = found;
        console.log(k + ":");
        v = v2;
    }
    console.log(parseDirectives(v.trim()));
}

parseHeader(data);

