# TODO: Add comment
# 
# Author:  Brad
# File:    Monte_Carlo_Test1.R
# Version: 1.0
# Date:    09.07.2014
# Purpose: Test Monte Carlo
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

# Clear workspace
rm(list = ls(all = TRUE))
rm(list = ls(all.names = TRUE))

# Limit History to not exceed 500 lines
Sys.setenv(R_HISTSIZE = 500)

repo <- c("http://cran.us.r-project.org")
options(repos = structure(repo))
options(install.packages.check.source = FALSE)

# String as factors is False -- used for read.csv
options(StringsAsFactors = FALSE)

# Default maxprint option
options(max.print = 500)
# options(max.print=99999)

# Memory limit
#memory.limit(size = 8183)

#Remove scientific notation if digits less than 100
options("scipen"=100)

#Uknown Strings
#unknowns_strings <- c("",".",NA,"na","n/a","n\a","NA","N/A","N\\A","<NA>","null","NULL",NULL,"nan","NaN",NaN,
#                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",NA_character_,
#                      "NA_character_",NA_real_,"NA_real_")
unknowns_strings <- c(" ","\n","",".","n/a","na","NA",NA,"<NA>","null","NULL",NULL,"nan","NaN",NaN,Inf,
                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                      NA_character_,"NA_character_",NA_real_,"NA_real_")

# Set location (1=HOME,2=WORK,3=LAPTOP,4=CORALSEA FROM HOME,5=CORALSEA FROM WORK,6=CORALSEA FROM LAPTOP)
Location <- 1

if (Location == 1) {
  
  #input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4",winslash="\\", mustWork=TRUE)
  #function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)    
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)  
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\", mustWork=TRUE)   
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\", mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 6) {
  
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\", mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else {
  
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("fGarch","LaplacesDemon","metRology","plyr","sn")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)

### NOTE - LaplacesDemon not on CRAN
### http://www.bayesian-inference.com/softwaredownload
### install.packages(pkgs="C:/Users/S.Brad/Downloads/LaplacesDemon_14.06.23.tar.gz", repos=NULL, type="source")


###############################################################################
cat("SECTION: TEST PARAMETERS", "\n")
###############################################################################

num_obs <- 60
location <- 0.1
scale <- 0.15
skewness <- 0
kurtosis <- 3

###############################################################################
cat("SECTION: SCALED T-DISTRIBUTION (SN PACKAGE) ", "\n")
###############################################################################

### n = number of draws
### xi = location parameter
### omega = scale parameter
### alpha = delta parameter (skewness; non-centrality parameter)
### nu = shape parameter (kurtosis; degrees of freedom)

mc_dat1 <- sn::rst(n=num_obs, xi=location, omega=scale, alpha=skewness, nu=kurtosis)
mc_dat1 <- as.numeric(mc_dat1)

###############################################################################
cat("SECTION: SCALED T-DISTRIBUTION (METROLOGY PACKAGE) ", "\n")
###############################################################################

### n = number of draws
### mean = location parameter
### sd = scale parameter
### ncp = delta parameter (skewness; non-centrality parameter)
### df = shape parameter (kurtosis; degrees of freedom)

mc_dat2 <- metRology::rt.scaled(n=num_obs, mean=location, sd=scale, ncp=skewness, df=kurtosis)
mc_dat2 <- as.numeric(mc_dat2)

###############################################################################
cat("SECTION: SCALED T-DISTRIBUTION (FGARCH PACKAGE) ", "\n")
###############################################################################

### n = number of draws
### mean = location parameter
### sd = scale parameter
### xi = delta parameter (skewness; non-centrality parameter)
### nu = shape parameter (kurtosis; degrees of freedom)

### NOTE - skewness not working for 0

mc_dat3 <- fGarch::rsstd(n = num_obs,mean=location,sd=scale,nu=kurtosis)
mc_dat3 <- as.numeric(mc_dat3)

###############################################################################
cat("SECTION: SCALED T-DISTRIBUTION (LAPLACESDEMON PACKAGE) ", "\n")
###############################################################################

### n = number of draws
### mu = location parameter
### sigma = scale parameter
### nu = shape parameter (kurtosis; degrees of freedom)

mc_dat4 <- LaplacesDemon::rst(n=num_obs, mu=location, sigma=scale, nu=kurtosis)
mc_dat4 <- as.numeric(mc_dat4)



