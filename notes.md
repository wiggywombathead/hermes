# supervisor meeting log

## 2020/02/04
- Met with Matthias to discuss overarching theme of project. Having read around
  the literature somewhat there is little material out there on distributed
  mechanism design for combinatorial auctions, hence to do a project on this
  would require a *lot* of new work on my part.

- Instead pitched the idea of a project on combinatorial auctions with two main
  themes: firstly, a platform to simulate and benchmark different auctions and
  play around with the various parameters; and secondly, a platform on which to
  host auctions, whereby the system decides on the appropriate mechanism to run
  based on the desired metric (revenue, social welfare, etc.), the bids
  submitted (submodular, additive, etc.), and (perhaps) lower bound on the
  approximation factor.

- He recommended I consider writing a web application -- this is the most
  natural platform on which to host such a system, I would say.

- An important factor to consider is the mechanics of submitting bids. In
  single item cases and cases with very few items on offers this is not a
  problem, but the number of bids is obviously exponential in the number of
  items.

### TODO
- Language of implementation
- Usability: how input bids
- Type of application
