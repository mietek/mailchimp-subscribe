_mailchimp-subscribe_
=====================

Haskell/[Scotty](https://github.com/scotty-web/scotty) web back-end for handling [MailChimp](http://mailchimp.com/) subscription requests.


Usage
-----

### Deploying with [Halcyon](http://halcyon.sh/)

With Halcyon installed:

```
$ halcyon deploy https://github.com/mietek/mailchimp-subscribe
$ export MAILCHIMP_API_KEY=...
$ export MAILCHIMP_LIST_ID=...
$ export WEBSITE_URL=...
$ PORT=8080 mailchimp-subscribe
```

- [Learn more](http://halcyon.sh/examples/#mailchimp-subscribe)


### Deploying with [Haskell on Heroku](http://haskellonheroku.com/)

Ready to deploy to Heroku in two clicks.

[![Deploy](https://www.herokucdn.com/deploy/button.png)](https://heroku.com/deploy?template=https://github.com/mietek/mailchimp-subscribe/)

Alternatively, with Heroku Toolbelt installed:

```
$ git clone https://github.com/mietek/mailchimp-subscribe
$ cd mailchimp-subscribe
$ heroku create -b https://github.com/mietek/haskell-on-heroku
$ heroku config:set MAILCHIMP_API_KEY=...
$ heroku config:set MAILCHIMP_LIST_ID=...
$ heroku config:set WEBSITE_URL=...
$ git push heroku master
$ heroku ps:scale web=1
$ heroku open
```

- [Deploy to Heroku](https://heroku.com/deploy?template=https://github.com/mietek/mailchimp-subscribe/)
- [Learn more](http://haskellonheroku.com/examples/#mailchimp-subscribe)


About
-----

Made by [Miëtek Bak](http://mietek.io/). Published under the [MIT X11 license](http://mietek.io/license/).
