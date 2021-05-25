# load_libraries ----------------------------------------------------------

library("jrvFinance", character.only = TRUE)
library("BondValuation", character.only = TRUE)
library("GUIDE", character.only = TRUE)
library("irr", character.only = TRUE)
library("data.table", character.only = TRUE)
library("xts", character.only = TRUE)
library("stringi", character.only = TRUE)

# set variable ------------------------------------------------------------



# jrvFinance --------------------------------------------------------------

npv(cf = c(100, 250, 300),
    rate = 5e-2)

650 * (1 - 0.05)^3
581 * (1 + 0.05)^3
557 * (1 + 0.05)^3

annuity.fv(rate = 0.05,
           n.periods = 3,
           instalment = )

npv(cf = c(1, 3, 2),
    rate = 10e-2,
    cf.t = c(0.3, 1.9, 2.5))

irr(c(-600, 300, 400))
irr(c(-600, 100, 400))

irr(cf = c(-1e5, 0, 0, 0, 0, 161051))
irr(cf = c(-100, 15, 15, 15, 15, 84.475))

