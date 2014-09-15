# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Screens_Combination.R
# Version: 1.0
# Date: 09.8.2014
# Purpose: Compute descriptive statistics
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
unknowns_strings <- c(" ","\n","",".","n/a","na","NA",NA,"<NA>","null","NULL",NULL,"nan","NaN",NaN,Inf,
                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                      NA_character_,"NA_character_",NA_real_,"NA_real_")

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK,5=CORALSEA FROM LAPTOP) 
Location <- 1


if (Location == 1) {
  input_directory <- normalizePath("F:/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Fund_Strategies/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)

###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("compare","cwhmisc","data.table","descr","fastmatch","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","leaps","lmtest","markdown","memisc","mitools",
                       "pander","pbapply","PerformanceAnalytics","plm","plyr","psych","quantreg","R.oo","R2wd",
                       "reporttools","reshape2","rms","RSQLite","sandwich","sqldf","stargazer","stringr",
                       "texreg","taRifx","UsingR","xtable","zoo")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

#crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="")
#mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")
#msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="")
#similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")
descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")
data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="")

###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "fund_id"

data_prescreen <- read.csv(file=paste(output_directory,"data_prescreen",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

data_screens <- read.csv(file=paste(output_directory,"data_screens",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

###############################################################################
cat("CLEAN PRESCREEN", "\n")
###############################################################################

### Make sure funds have atleast 24 months of returns
data_prescreen_firm_counts <- count(data_prescreen, c(identifier))
data_prescreen_firm_keep <- data_prescreen_firm_counts[data_prescreen_firm_counts[,"freq"]>=24,]
data_prescreen_firm_keep <- data_prescreen_firm_keep[!is.na(data_prescreen_firm_keep[,c(identifier)]),]
row.names(data_prescreen_firm_keep) <- seq(nrow(data_prescreen_firm_keep))

data_trim <- data_prescreen[(data_prescreen[,c(identifier)] %in% data_prescreen_firm_keep[,c(identifier)]),]
row.names(data_trim) <- seq(nrow(data_trim))

#rm(data_prescreen,data_prescreen_firm_counts,data_prescreen_firm_keep)


###############################################################################
cat("CLEAN SCREENS", "\n")
###############################################################################

data_screens_cols_99 <- colnames(data_screens)[grep("99", colnames(data_screens))]
data_screens_99 <- data.frame(data_screens[,c(identifier,data_screens_cols_99)],quality_score_99=NA,stringsAsFactors=FALSE)
data_screens_99[,"quality_score_99"] <- rowSums(data_screens_99[,data_screens_cols_99], na.rm = TRUE)

data_screens_cols_95 <- colnames(data_screens)[grep("95", colnames(data_screens))]
data_screens_95 <- data.frame(data_screens[,c(identifier,data_screens_cols_95)],quality_score_95=NA,stringsAsFactors=FALSE)
data_screens_95[,"quality_score_95"] <- rowSums(data_screens_95[,data_screens_cols_95], na.rm = TRUE)

data_screens_cols_90 <- colnames(data_screens)[grep("90", colnames(data_screens))]
data_screens_90 <- data.frame(data_screens[,c(identifier,data_screens_cols_90)],quality_score_90=NA,stringsAsFactors=FALSE)
data_screens_90[,"quality_score_90"] <- rowSums(data_screens_90[,data_screens_cols_90], na.rm = TRUE)


###############################################################################
cat("MERGE DATA", "\n")
###############################################################################

data_screens_merge0 <- data_screens_99
data_screens_merge1 <- cbind(data_screens_merge0,data_screens_95[,colnames(data_screens_95)[!(colnames(data_screens_95) %in% identifier)]])
data_screens_merge2 <- cbind(data_screens_merge1,data_screens_90[,colnames(data_screens_90)[!(colnames(data_screens_90) %in% identifier)]])


data_all <- merge(data_trim,  data_screens_merge2, 
                  by.x=c(identifier), by.y=c(identifier), 
                  all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_screens_merge0,data_screens_merge1,data_screens_merge2)
#rm(data_trim,data_screens_99,data_screens_95,data_screens_90)


###############################################################################
cat("SUMMARY", "\n")
###############################################################################

data_all_stats <- unique(data_all[,c(identifier,"quality_score_90")])

max(data_all_stats[,"quality_score_90"])
min(data_all_stats[,"quality_score_90"])
mean(data_all_stats[,"quality_score_90"])
median(data_all_stats[,"quality_score_90"])
t(quantile(data_all_stats[,"quality_score_90"],c(0.01,0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95,0.99)))

