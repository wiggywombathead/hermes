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

## 2020/02/11
- We discussed the todos of last week.

- On the subject of collecting bids, I suggested two main ways in which to do
  so: firstly, allow the bidders to be k-minded, meaning they submit k bids,
  one for each subset they would like to receive, and effectively 0 for every
  other subset; secondly, collect a single bid for each item and have the
  program combine these in complex ways. 

- Regarding this second point, Matthias suggested looking at Yahoo's
  "predictalot" program, which was a program on which to host prediction
  markets. These allow users to bid on outcomes of various evetns, e.g. the
  winner of the presidential election, and combines them. Hence this is (at
  least superficially) related to combinatorial auctions. Since people are
  betting their money, they also give an indication as to how people "really
  feel" about certain events, which is interesting.

- I agreed that a web application is the most natural platform on which to host
  such an auction. I expressed my desire to implement it in Lisp, not because
  it is a functional language, but just because I want to learn Lisp.

### TODO
- Do my research proposal on this topic and ask Matthias for any help
- Look into predictalot and blog posts on it for inspiration regarding
  combining bids in complex ways
