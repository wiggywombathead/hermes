# TODO
- [ ] Final features
  - [x] Rewrite javascript using Parenscript
  - [ ] Automated closing of markets (when all arbiters have reported)
    - [x] set timer on each page load to trigger when soonest deadline expires
	- [x] As soon as final arbiter reports an outcome, close the market and pay
	  shareholders. NOTE: always after at least two reports and at least some
	  proportion of userbase has reported (proportion could diminish as
	  userbase grows?)
  - [ ] Gather signal priors from reporting history (not asking explicitly)
    - Produces an "average signal reliability" for each arbiter: probably good?
  - [x] Compute k correctly
  - [ ] Compute f correctly
  - [ ] Thought: only allow user to be arbiter if they have a position in
	*some* market? -- would stop users just voting on outcomes (guaranteed
	profits)

- [ ] Website
  - [x] Basic pages in Hunchentoot
  - [ ] User interface
  - [x] User registration
  - [x] User login
  - [ ] Make new bets
    - [x] Only logged-in users
	- [x] Initial shares/first dibs
	- [ ] Create markets under categories
	- [x] Remember to implement trading fees
  - [ ] Find a way to point "/" to "/index" (hunchentoot)
  - [x] Portfolio page
    - [x] Trade from portfolio page

- [ ] Database
  - [x] Set up Mito
  - [x] Design database schema
  - [x] Clean up connect/disconnect interface?
    - `with-open-database` macro
  - [ ] Trade histories table
  - [ ] Improve/macro-ise the `get-`, `get-` interface for retrieving items
	from `user-securities`

- [ ] Trading
  - [x] Implement Logarithmic Market Scoring Rule
  - [x] Ability to trade between agents
  - [ ] Trading fees
    - [x] Pay to central "bank"
	- [x] Fix liquidation transactions: no fee on liquidation but only _up to_
	  the amount they liquidate (i.e. selling back all they own and then more,
	  there should be a fee on the portion that they short)
  - [x] Where does the money go? Create entry in `user` table for the bank, and
	use the funds transferred here to pay winnings?
	- [x] Finish `pay-banker` function
	- [ ] Idea: keep track of money and fees paid to the bank separately?
  - [ ] What happens when the price approaches 0/1? Test.
  - [ ] What to do about bank's balance going negative? Start with float?
  - [ ] Separate into own interface to clean up

- [ ] Arbitration
  - [x] 1/prior midpoint mechanism
    - [x] Retrieve closing price and use as prior signal probability `mu`
  - [x] 1/prior with midpoint mechanism
    - [ ] compute signal priors using history rather than ask for explicitly
  - [ ] How to get arbitrators?
    - [x] ~~Anyone can arbitrate: deal with odd number of arbiters~~ **MUST NOT BE ABLE TO OPT IN**
	- [ ] Arbiters randomly (pre-?) selected: how long to wait for response

- [ ] Documentation
  - [ ] Development diary notes
  - [x] Presentation
  - [x] Interim Report

- [ ] General fixes
  - [ ] Get rid of annoying `returning from unknown block nilBlock warning from
	Parenscript
  - [x] Make macro for Parenscript ensuring fields are nonempty
  - [ ] Find a (better?) way to remove `d0` from LISP doubles when inserting
	into database
  - [x] Consider merging `first-dibs` and `buy-or-sell-security` into one
	function (i.e. `trade-security` uses same backend as `create-market`)
	- No point: too different
  - [x] Clean up date/deadline formatting -- `format-timestring`
