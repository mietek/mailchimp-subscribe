_mailchimp-subscribe_
=====================

[MailChimp](http://mailchimp.com/) subscription request handler, built with [Scotty](https://github.com/scotty-web/scotty/).  Intended to support custom signup forms.


Usage
-----

### Deploying with [Halcyon](https://halcyon.sh/)

```
$ halcyon deploy https://github.com/mietek/mailchimp-subscribe
$ export MAILCHIMP_API_KEY=...
$ export MAILCHIMP_LIST_ID=...
$ export WEBSITE_URL=...
$ PORT=8080 mailchimp-subscribe
```


### Deploying with [Haskell on Heroku](https://haskellonheroku.com/)

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/mietek/mailchimp-subscribe)


About
-----

Made by [Miëtek Bak](https://mietek.io/).  Published under the [MIT X11 license](https://mietek.io/license/).
