library(devtools)
library(roxygen2)
library(here)

package_dir = here()
document()

setwd("..")

# run automated tests
#check()
#devtools::test()

# try to install package
install("TA2tools")

detach("package:TA2tools", unload=T)
remove.packages("TA2tools")

devtools::install_github("", force=T)
library(TA2tools)
help(package="TA2tools")
