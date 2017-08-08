http = require("https")
argv = require('optimist').argv
log = require ('winston')

if argv.d
        log.level = argv.d

token = argv.t

if (!token) 
        readline = require('readline');
        
        google = require('googleapis');
        OAuth2Client = google.auth.OAuth2;
        plus = google.plus('v1');

        #// Client ID and client secret are available at
        #// https://code.google.com/apis/console
        CLIENT_ID = 'CLIENT_ID_FROM_GOOGLE';
        CLIENT_SECRET = 'CLIENT_SECRET';
        REDIRECT_URL = 'urn:ietf:wg:oauth:2.0:oob';

        oauth2Client = new OAuth2Client(CLIENT_ID, CLIENT_SECRET, REDIRECT_URL);

        rl = readline.createInterface({
          input: process.stdin,
          output: process.stdout
        });

        getAccessToken = (oauth2Client, callback)  ->
                #// generate consent page url
                url = oauth2Client.generateAuthUrl {
                    access_type: 'offline', #// will return a refresh token
                    scope: ['https://www.googleapis.com/auth/plus.me','https://www.googleapis.com/auth/fitness.activity.read', 'https://www.googleapis.com/auth/fitness.body.read','https://www.googleapis.com/auth/fitness.location.read'] #// can be a space-delimited string or an array of scopes
                }

                console.log('Visit the url: ', url);
                rl.question 'Enter the code here:', (code) -> 
                        #// request access token
                        console.log("===============================================" + code);
                        oauth2Client.getToken code, (err, tokens) -> 
                                #// set tokens to the client
                                #// TODO: tokens should be set by OAuth2 client.
                                oauth2Client.setCredentials(tokens);
                                console.log(tokens)
                                callback(tokens.access_token);

        #// retrieve an access token
        #getAccessToken oauth2Client, () -> 
                #// retrieve user profile
         #       plus.people.get { userId: 'me', auth: oauth2Client }, (err, profile) -> 
         #           if (err) 
         #               console.log('An error occured', err);
         #               return
         #           console.log(profile.displayName, ':', profile.tagline);


getWeightSeries = (token, ts, cb) ->
        http.get {
                host:'www.googleapis.com',
                path: '/fitness/v1/users/me/dataSources/raw:com.google.weight:com.google.android.apps.fitness:user_input/datasets/000000000-' + ts
                headers: {Authorization: 'Bearer ' + token}
                },
                (res) ->
                        data = ''
                        res.on 'data', (chunk) ->
                                data += chunk.toString()
                        res.on 'end', () ->
                                cb(data)


now = (new Date).getTime() * 1000 * 1000 # nanoseconds
#https://www.googleapis.com/fitness/v1/users/me/dataSources/raw:com.google.weight:com.google.android.apps.fitness:user_input/datasets/000000000-1440688144000000000

logWeight = (token) -> 
        getWeightSeries token, now, (d) ->
                log.debug(d)
                j = JSON.parse(d)
                if j.error
                        log.error j.error.message
                else
                        log.debug(j.point)
                        j.point.map (i) ->
                                console.log("weight on " + new Date(parseInt(i.endTimeNanos)/(1000 * 1000)) + " is " + i.value[0].fpVal)
        
        
if (token)
        logWeight token
else
        getAccessToken oauth2Client, (token) -> logWeight token
