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

- [ ] Database
  - [x] Set up Mito
  - [x] Design database schema
  - [x] Clean up connect/disconnect interface?
    - `with-open-database` macro
  - [ ] Trade histories table

- [ ] Trading
  - [x] Implement Logarithmic Market Scoring Rule
  - [x] Ability to trade between agents
  - [ ] Trading fees
    - [ ] Pay to central "bank"
  - [ ] Where does the money go? Create entry in `user` table for the bank, and
	use the funds transferred here to pay winnings?
  - [ ] What happens when the price approaches 0/1? Test.

- [ ] Arbitration
  - [ ] 1/prior midpoint mechanism
  - [ ] How to get arbitrators?

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
  - [ ] Clean up date/deadline formatting
