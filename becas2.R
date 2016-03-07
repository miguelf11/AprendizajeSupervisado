

dat <- read.csv("data/minable.csv")


#delete unnecessary features

dat$jReprobadas <- NULL
dat$dHabitacion <- NULL
dat$sugerencias <- NULL
dat$aEconomica <- NULL
dat$cDireccion <- NULL
dat$oSolicitudes <- NULL
dat$pReside <- NULL


