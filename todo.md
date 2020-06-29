# TODO
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
  - [ ] 1/prior midpoint mechanism
    - [x] Retrieve closing price and use as prior signal probability `mu`
  - [ ] How to get arbitrators?
    - [ ] Anyone can arbitrate: deal with odd number of arbiters
	- [ ] Arbiters randomly (pre-?) selected: how long to wait

- [ ] Documentation
  - [ ] Development diary notes
  - [x] Presentation
  - [ ] Interim Report

- [ ] General fixes
  - [ ] Get rid of annoying `returning from unknown block nilBlock warning
  - [x] Make macro for Parenscript ensuring fields are nonempty
  - [ ] Find a (better?) way to remove `d0` from LISP doubles when inserting
	into database
  - [x] Consider merging `first-dibs` and `buy-or-sell-security` into one
	function (i.e. `trade-security` uses same backend as `create-market`)
	- No point: too different
  - [x] Clean up date/deadline formatting -- `format-timestring`
