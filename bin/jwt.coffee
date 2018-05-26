#sudo pacman -S coffee-script
#sudo npm install -g rx rx-node jsonwebtoken
#sudo npm install -g rx rx-node jsonwebtoken

jwt = require('jsonwebtoken')
# install rxjs rxjs-es rx-node
rx = require('rx')
rxn = require('rx-node')

rxn.fromStream(process.stdin).buffer(() => rx.Observable.timer(125))
        .subscribe((x) => 
            token = x.toString('utf8')
            #console.log(token)
            decoded = jwt.decode(token, {complete: true})
            console.log(JSON.stringify(decoded))
            );


