-------------------------------------------------------------------------------

This project is no longer maintained.

-------------------------------------------------------------------------------


_mailchimp-subscribe_
=====================

[MailChimp](http://mailchimp.com/) subscription request handler, built with [Scotty](https://github.com/scotty-web/scotty).  Intended to support custom signup forms.


Usage
-----

Listens for HTTP `POST` requests at `/subscribe`.  All requests are forwarded to the MailChimp [`/lists/subscribe`](https://apidocs.mailchimp.com/api/2.0/lists/subscribe.php) endpoint.

| Query parameter   | Description
| :---------------- | :----------
| `name`            | Subscriber name.  Required.
| `email_address`   | Subscriber email address.  Required.
| `list_id`         | Mailing list identifier.  Required, unless `MAILCHIMP_LIST_ID` is set.
| `email_type`      | Either `html` or `text`.
| `double_optin`    | Send a double opt-in confirmation message.
| `update_existing` | Accept already existing email addresses.
| `send_welcome`    | If double opt-in is off, send the list welcome message.


### Configuration

Authentication credentials and defaults can be configured by setting environment variables.

| Environment variable        | Description
| :-------------------------- | :----------
| `MAILCHIMP_API_KEY`         | Authentication token.  Required.
| `MAILCHIMP_LIST_ID`         | Default for `list_id`.
| `MAILCHIMP_EMAIL_TYPE`      | Default for `email_type`.
| `MAILCHIMP_DOUBLE_OPTIN`    | Default for `double_optin`.
| `MAILCHIMP_UPDATE_EXISTING` | Default for `update_existing`.
| `MAILCHIMP_SEND_WELCOME`    | Default for `send_welcome`.
| `PORT`                      | HTTP listening port.  Defaults to `8080`.


### Deployment

Installs in seconds on most Linux and OS X machines, using [Halcyon](https://halcyon.sh/).

```
$ halcyon install https://github.com/mietek/mailchimp-subscribe
$ export MAILCHIMP_API_KEY=…
$ mailchimp-subscribe
```


#### Deploying to Heroku

Ready to deploy in one click to the [Heroku](https://heroku.com/) web application platform, using [Haskell on Heroku](https://haskellonheroku.com/).

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/mietek/mailchimp-subscribe)

Clicking the button is equivalent to executing the following commands:

```
$ git clone https://github.com/mietek/mailchimp-subscribe
$ cd mailchimp-subscribe
$ heroku create -b https://github.com/mietek/haskell-on-heroku
$ heroku config:set MAILCHIMP_API_KEY=…
$ git push heroku master
$ heroku ps:scale web=1
$ heroku open
```


About
-----

Made by [Miëtek Bak](https://mietek.io/).  Published under the BSD license.
