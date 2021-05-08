//sudo npm install -g rx rx-node node-jose 
//key should be set in env variale of form 
//export key='{"kty":"oct","use":"enc","kid":"a5f38bfd-6a5f-405d-80a2-351504499068","k":"i1SYD-Z2tInjdasdogah"}'
// only one key is supported

jose = require('node-jose')
jwt = require('jsonwebtoken')
rx = require('rx')
rxn = require('rx-node')

rxn.fromStream(process.stdin).buffer(() => rx.Observable.timer(125))
    .subscribe((x) =>  {
        token = x.toString('utf8');
        jose.JWK.asKeyStore([JSON.parse(process.env.key1)]).
            then(res => {
                var key = res.get(0);
                //var key = res.get("a5f38bfd-6a5f-405d-80a2-351504499068");
                jose.JWE.createDecrypt(key).decrypt(token).then(json => {
                    const newres = (json.plaintext.toString());
                    //console.log(newres);
                    //for signed jwt
                    decoded = jwt.decode(newres, {complete: true});
                    console.log(JSON.stringify(decoded));
                }).catch((e) => console.log(e));
            });
    });


