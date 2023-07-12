### This file runs the initial set up of R Selenium.

## A brief explanation:
## binman::list_versions() function is used by the workflow.
## This function tries to access a directory that doesn't exist initially and so it throws an error.
## This script creates the directory by using rsDriver and setting chromever="latest".
## After doing so, binman::list_versions() should work without error.

# Load RSelenium

presPackages <- rownames(installed.packages())
if(!("RSelenium" %in% presPackages)){
  install.packages("RSelenium")
}

library(RSelenium)

# Create the binman directory (this will take a few minutes (about 3 min) and you will see output).
driver <- rsDriver(browser = "chrome", chromever="latest")

# Check the directory exists by displaying available drivers for RSelenium
okDrivers <- binman::list_versions("chromedriver")
for(i in 1:length(okDrivers)){
  cat("For ", names(okDrivers)[[i]], ":\n   ", sep="")
  cat(okDrivers[[i]], sep="\n   ")
}

# Restart RStudio.
print("Check that the above output contains your version of Chrome (ignoring the numbers after the last decimals).")
print("Please restart RStudio before running the workflow.")


