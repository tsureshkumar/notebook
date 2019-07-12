// Generated by CoffeeScript 2.4.0
(function() {
  //sudo pacman -S coffee-script
  //sudo npm install -g rx rx-node jsonwebtoken
  //sudo npm install -g rx rx-node jsonwebtoken
  var jwt, rx, rxn;

  jwt = require('jsonwebtoken');

  // install rxjs rxjs-es rx-node
  rx = require('rx');

  rxn = require('rx-node');

  rxn.fromStream(process.stdin).buffer(() => {
    return rx.Observable.timer(125);
  }).subscribe((x) => {
    var decoded, token;
    token = x.toString('utf8');
    //console.log(token)
    decoded = jwt.decode(token, {
      complete: true
    });
    return console.log(JSON.stringify(decoded));
  });

}).call(this);
