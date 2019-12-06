# IMPORTANT
`master` branch is for Mollie API v1. If you want v2 support, we have a beta client in branch [`feature/v2`](https://github.com/paramander/mollie-api-haskell/tree/feature/v2). You can find the v2 beta on hackage http://hackage.haskell.org/package/mollie-api-haskell-2.0.0.0

# Mollie API client for Haskell

Accepting [iDEAL](https://www.mollie.com/ideal/), [Bancontact/Mister Cash](https://www.mollie.com/mistercash/), [SOFORT Banking](https://www.mollie.com/sofort/), [Creditcard](https://www.mollie.com/creditcard/), [SEPA Bank transfer](https://www.mollie.com/banktransfer), [SEPA Direct debit](https://www.mollie.com/directdebit/), [Bitcoin](https://www.mollie.com/bitcoin/), [PayPal](https://www.mollie.com/paypal/), [Belfius Direct Net](https://www.mollie.com/belfiusdirectnet/) and [paysafecard](https://www.mollie.com/paysafecard/) online payments without fixed monthly costs or any punishing registration procedures. Just use the Mollie API to receive payments directly on your website or easily refund transactions to your customers.

## Notice

Because of deployment issues with applications using this library the mollie root certificate check is disabled, the requests are however still preformed with secure connections! For more information or questions go to issue [#2](https://github.com/paramander/mollie-api-haskell/issues/2).

## Requirements

To use the Mollie API client, the following things are required:

+ Get yourself a free [Mollie account](https://www.mollie.com/en/signup). No sign up costs.
+ Create a new [Website profile](https://www.mollie.com/dashboard/) to generate API keys (live and test mode) and setup your webhook.
+ Now you're ready to use the Mollie API client in test mode.
+ In order to accept payments in live mode, payment methods must be activated in your account.
+ Up-to-date OpenSSL

## How to receive payments

To successfully receive a payment, these steps should be implemented:

1. Use the Mollie API client to create a payment with the requested amount, description and optionally, a payment method. It is important to specify a unique redirect URL where the customer is supposed to return to after the payment is completed.

2. Immediately after the payment is completed, our platform will send an asynchronous request to the configured webhook to allow the payment details to be retrieved, so you know when exactly to start processing the customer's order.

3. The customer returns, and should be satisfied to see that the order was paid and is now being processed.

## API documentation
If you wish to learn more about our API, please visit the [Mollie Developer Portal](https://www.mollie.com/developer/). API Documentation is available in both Dutch and English.

## License
[BSD3](https://opensource.org/licenses/BSD-3-Clause). Copyright (c) 2016, [Paramander](https://www.paramander.com).

## Support

+ [More information about iDEAL via Mollie](https://www.mollie.com/ideal/)
+ [More information about credit card via Mollie](https://www.mollie.com/creditcard/)
+ [More information about Bancontact/Mister Cash via Mollie](https://www.mollie.com/mistercash/)
+ [More information about SOFORT Banking via Mollie](https://www.mollie.com/sofort/)
+ [More information about SEPA Bank transfer via Mollie](https://www.mollie.com/banktransfer/)
+ [More information about SEPA Direct debit via Mollie](https://www.mollie.com/directdebit/)
+ [More information about Bitcoin via Mollie](https://www.mollie.com/bitcoin/)
+ [More information about PayPal via Mollie](https://www.mollie.com/paypal/)
+ [More information about Belfius Direct Net via Mollie](https://www.mollie.com/belfiusdirectnet/)
+ [More information about paysafecard via Mollie](https://www.mollie.com/paysafecard/)
