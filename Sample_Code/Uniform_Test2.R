# TODO: Add comment
# 
# Author:  Brad
# File:    Uniform_Test2.R
# Version: 1.0
# Date:    09.07.2014
# Purpose: Uniform Test
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

#source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_parse.R",sep="\\"),echo=FALSE)


source(file=paste(function_directory,"functions_misreporting_screens.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("plyr")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
cat("SECTION: CREATE DATA ", "\n")
###############################################################################

identifier <- "fund_id"
analysis_col <- "Monthly_Ret"


uniform_data1 <- data.frame(temp_id=0,
                            Monthly_Ret=c(seq(1,10,1),1,seq(1,10,1),2,seq(2,10,1),3,3,seq(3,10,1)),
                            stringsAsFactors=FALSE)

uniform_data2 <- data.frame(temp_id=1,
                            Monthly_Ret=c(seq(1,10,1),10,10,seq(1,9,1),9,9,9,seq(1,8,1),8,seq(1,7,1)),
                            stringsAsFactors=FALSE)

uniform_data <- data.frame(rbind(uniform_data1,uniform_data2),Monthly_Ret_Percent=NA, stringsAsFactors=FALSE)
colnames(uniform_data)[match("temp_id",names(uniform_data))] <- identifier

uniform_data[,"Monthly_Ret"] <- uniform_data[,"Monthly_Ret"]/10000
uniform_data[,"Monthly_Ret_Percent"] <- uniform_data[,"Monthly_Ret"]*100

rm(uniform_data1,uniform_data2)


###############################################################################
cat("SECTION: DEFINE PARAMETERS - UNIFORM TEST", "\n")
###############################################################################

uniform_screen_execute <- function(data,ret_col,graph){
  
  # data <- x
  # ret_col <- analysis_col
  # graph <- FALSE
  
  screen_uniform <- data.frame(data,Ret_Digits=NA,stringsAsFactors=FALSE)
  
  #screen_uniform[,"Ret_Digits"] <- uniform_screen_LHS_digits(screen_uniform[,analysis_col],1)
  screen_uniform[,"Ret_Digits"] <- uniform_screen_RHS_digits(screen_uniform[,analysis_col],1,4)
  #screen_uniform[,"Ret_Digits"] <- uniform_screen_RHS_digits(screen_uniform[,"Monthly_Ret_Percent"],1,2)
  
  ### Expand Digits
  
  screen_uniform_all <- uniform_screen_expand_digits(z=screen_uniform,test_col="Ret_Digits")
  
  #rm(screen_uniform)
  
  ### Compute Stats
  screen_uniform_all_stats <- uniform_screen_probs(y=screen_uniform_all,test_col="Ret_Digits")
  #rm(screen_uniform_all)
  
  ### Goodness of Fit test
  screen_uniform_gof <- uniform_screen_gof(w=screen_uniform_all_stats,test_col="Ret_Digits")
  
  ### Graph
  
  if(graph){
    
    screen_uniform_graph_temp  <- uniform_screen_graph(g=screen_uniform_all_stats,test_col="Ret_Digits")
    
    ggplot(screen_uniform_graph_temp, aes(x = fd, y = prob_actual)) + geom_bar(stat = "identity", fill = "blue") +
      geom_line(aes(x = fd, y = prob_theoretical, size = 0.1)) + geom_point(aes(x = fd, y = prob_theoretical, color = "red", size = 1)) +
      theme_bw() + scale_x_continuous(breaks = seq(min(screen_uniform_graph_temp[,"fd"]):max(screen_uniform_graph_temp[,"fd"])))
  }
  
  rm(screen_uniform_all_stats)
  
  return(screen_uniform_gof)
  
}


###############################################################################
cat("SECTION: UNIFORM TEST", "\n")
###############################################################################

data_s5_5_gof <- ddply(.data=uniform_data, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s5[data_s5[,identifier]==0,c(data_trim_id_full_cols,analysis_col)]
  # x <- uniform_data[uniform_data[,identifier]==0,]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  aa <- uniform_screen_execute(data=x,ret_col=analysis_col,graph=FALSE)
  
  return(aa)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "none", .inform = FALSE, .drop = TRUE)


### Create Uniform Flags

data_s5_5_gof_full <- data.frame(data_s5_5_gof,
                                 uniform_percent_99=NA, uniform_percent_95=NA, uniform_percent_90=NA, 
                                 stringsAsFactors=FALSE)
rm(data_s5_5_gof)

data_s5_5_gof_full[,"uniform_percent_99"] <- ifelse(data_s5_5_gof_full[,"pval_uniform"]<=0.01,1,0)
data_s5_5_gof_full[,"uniform_percent_95"] <- ifelse(data_s5_5_gof_full[,"pval_uniform"]<=0.05,1,0)
data_s5_5_gof_full[,"uniform_percent_90"] <- ifelse(data_s5_5_gof_full[,"pval_uniform"]<=0.10,1,0)

data_s5_5_final <- data_s5_5_gof_full[,c(identifier, "uniform_percent_99", "uniform_percent_95", "uniform_percent_90")]

rm(data_s5_5_gof_full)
gc()
