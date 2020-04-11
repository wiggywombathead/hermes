# Hermes developer diary

## Setting up environment

[Setting up Parenscript and AJAX][jeaye]

[Parenscript tutorial][parenscript]

[Prediction market implementation][implementation]

[jeaye]: https://blog.jeaye.com/2015/09/27/parenscript-ajax/
[parenscript]: https://common-lisp.net/project/parenscript/tutorial.html
[implementation]: https://github.com/JohnNay/predMarket

## Market mechanism

### Scoring Rule Market

Scoring rules are measures of the accuracy of probabilistic predictions and are
applicable to tasks where probabilities must be assigned to a set of mutually
exclusive outcomes.

Prediction markets can be implemented using a Continuous Double Auction (CDA),
which is a mechanism of matching buyers and sellers of a stock. The market
maker keeps an order book that tracks bids and asks. For example, Alice says
she wants to buy a share of stock A for $5. Bob owns a share of stock B and is
willing to sell it for $5. The former is recorded as a bid, while the latter as
an ask. If the bid and ask for two traders match, as they do for Alice and Bob,
then the trade is executed. CDAs are used in traditional stock markets like the
NYSE.

An issue with a CDA in a prediction market is that liquidity can be a problem.
Most prediction markets have far fewer participants than a stock exchange. If I
make a bid for $5 but there is no one selling that stock for that price, the
trade cannot be made. The market has low/poor liquidity.

This problem is alleviated by using an automated market maker, which acts as
the house and assumes the opposite side of all trades. This ensures
participants are always able to make a trade, thus "making" the market. This is
typically not used in real-money markets since always taking the opposite side
to a trade could result in losses for the "house", but they are used in
play-money markets where losses are less detrimental.

To make the market the platform sets a price for each stock, set using a market
scoring rule (MSR). This produces the price for the stock and cost for a trade.
The most common market scoring rule is the Logarithmic Market Scoring Rule
(LMSR), created by Robin Hanson.

### Logarithmic Market Scoring Rule

#### Pricing a stock

Suppose we have a market with two stocks of which there are `q_1` outstanding
shares for one and `q_2` outstanding shares for the other. The number of
outstanding shares is the number of issued shares minus the number of shares in
the company's treasury. The price of a stock is given by:

```
price = e^(q_1 / b) / (e^(q_1 / b) + e^(q_2 / b))
```

The parameter `b` is an arbitrary constant, the "liquidity parameter". A
smaller value will result in higher price responsiveness (purchasing a small
number of shares increases the price by a lot), while a larger number will
result in the price being "sticky" and the price will change less for a fixed
dollar trading amount, and large number of shares will need to be purchased to
change the price significantly. For smaller markets with less activity (as ours
will likely be), a smaller `b` is appropriate.

The LSMR can be can be extended to account for any number of stocks:

```
price(i) = e^(q_i / b) / (sum_j (e^(q_j / b)))
```

We may replace "number of stocks" with "number of outcomes for the event". For
a binary outcome, `q1` tracks the number of shares bought on the YES outcome
and `q2` tracks the number of shares bought on the NO outcome.

#### Costing a trade

Determining the cost of a given trade is done using:

```
cost = b * ln(e^(q_1 / b) + e^(q_2 / b))
```

The amount that a trader must pay to acquire the shares is the difference
between the cost before the trade and the cost after the trade.

#### Our market

In our market, an agent wishing to purchase `q' - q` shares of a security pays
`C(q') - C(q)`. The cost function is:

```
C_b(q) = b log(1 + e^(q/b))
```

The price function only applies if buying a miniscule (i.e. infinitesimal)
quantity of shares; as soon as traders start buying, the price immediately goes
up. Hence we only use the cost function to determine how much an agent is to
pay for some quantity of securities. For completeness, the price function is:

```
p(q) = e^(q/b) / (1 + e^(q/b))
```

Suppose there are `q = 10` outstanding shares of stock A of which Alice wants
to buy `q' - q = 7` shares. Let `b = 4`. So Alice will pay `C(17) - C(10) = 10
* log((1+e^(17/10))/(1+e))`.

### Other crowdsourced forecasting tools

Another common crowdsourced forecasting tool is an Opinion Pool, in which
participants give probabilistic estimates for the likelihood of events
occurring. Opinion Pool platforms take these estimates and aggregate them to
generate a consensus. The algorithm for aggregating the forecasts into this
consensus can be as simple as taking the mean, but can be much more complex.

