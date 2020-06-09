# Cassie developer diary

## Setting up environment

[Setting up Parenscript and AJAX][jeaye]

[Parenscript tutorial][parenscript]

[Prediction market implementation][implementation]

[jeaye]: https://blog.jeaye.com/2015/09/27/parenscript-ajax/
[parenscript]: https://common-lisp.net/project/parenscript/tutorial.html
[implementation]: https://github.com/JohnNay/predMarket

## Scoring Rule Markets

Information taken from [CultivateLabs][cultivate-labs] and [David Pennock's
blog][oddhead].

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

#### Quoting a stock's price

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

The LMSR can be can be extended to account for any number of stocks:

```
price(i) = e^(q_i / b) / (sum_j (e^(q_j / b)))
```

We may replace "number of stocks" with "number of outcomes for the event". For
a binary outcome, `q1` tracks the number of shares bought on the YES outcome
and `q2` tracks the number of shares bought on the NO outcome.

#### Pricing a trade

Determining the cost of a given trade is done using:

```
cost = b * ln(e^(q_1 / b) + e^(q_2 / b))
```

The amount that a trader must pay to acquire the shares is the difference
between the cost before the trade and the cost after the trade: an agent
wishing to purchase `q' - q` shares of a security pays `C(q') - C(q)`. The cost
function is:

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
to buy `q' - q = 7` shares. Let `b = 4`. So Alice will pay `C(17) - C(10) = 4
log((1+e^(17/10))/(1+e))`.

### Other crowdsourced forecasting tools

Another common crowdsourced forecasting tool is an Opinion Pool, in which
participants give probabilistic estimates for the likelihood of events
occurring. Opinion Pool platforms take these estimates and aggregate them to
generate a consensus. The algorithm for aggregating the forecasts into this
consensus can be as simple as taking the mean, but can be much more complex.

[cultivate-labs]: https://www.cultivatelabs.com/prediction-markets-guide/what-are-the-different-types-of-prediction-markets
[oddhead]: http://blog.oddhead.com/2006/10/30/implementing-hansons-market-maker/

## Market mechanism

We implement the crowdsourced outcome-determination mechanism from [this
paper][CODiPM]

The market in its entirety is:
- Market stage
  - set up prediction market for event `X` using a market scoring rule
  - allow agents to trade in the market, charging `fp` for a security bought at
    price `p` and charging `f(1-p)` for a security sold at price `p`
  - market closes, trading stops
- Arbitration stage
  - each arbiter reports a signal x that is 1 if the event occurs, else 0
  - each arbiter is randomly assigned another arbiter and paid according to the
	1/prior with midpoint mechanism
  - the outcome of the market and the payoff of each share sold is set to the
	fraction of arbiters that reported x = 1

For the market stage we implement the Logarithmic Market Scoring Rule as
discussed.

For the arbitration stage, we need to compute a random pairing of the arbiters
for a given security such that no arbiter is paired to himself. This is simple
enough, we just shuffle the list of arbiters, loop over the first n/2 players
and randomly assign them to players in the second half.

As an aside, initially I equated computed a random matching between arbiters to
computing a random derangement between them -- a permutation where no element
remains in its original position (this would be the case if the pairing
relation was not symmetric I think). I implemented this
[algorithm][random-derangement-algorithm] from Martinez, Panholzer, and
Prodinger, and while it is now (likely) useless for the rest of the project, it
was a good exercise.

[CODiPM]: https://arxiv.org/abs/1612.04885
[random-derangement-algorithm]: https://epubs.siam.org/doi/pdf/10.1137/1.9781611972986.7

## 05/06/2020
**Important**: name changed from Hermes to Cassie -- more fitting.

## 09/06/2020
Got Mito set up now and created the tables that will be needed. Pleased with
the `with-open-database` macro, even if it is very simple. Now I will need to
implement the ability to create and store bets, display their associated
information, and eventually be able to trade them between agents.
