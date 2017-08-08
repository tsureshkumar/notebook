/*
  JDownloader server
   
   Useful for operating on the urls in browser. like write the urls to file
   
*/

var express = require("express");
var app = express();

app.get('/flash/', function(req, res){res.send('JDownloader'); });
app.get('/flash/add', function(req, res){
	console.log(req.query.urls);
	res.send('success'); 
});

app.listen(9666);
