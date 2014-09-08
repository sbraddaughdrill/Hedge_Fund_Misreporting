# TODO: Add comment
# 
# Author:  Brad
# File:    Zero_and_Negative_Return_Test2.R
# Version: 1.0
# Date:    09.07.2014
# Purpose: Zero and Negative Return Test Test
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


x_flag_cols <- c("flag_pos","flag_zero","flag_neg")

identifier <- "fund_id"
analysis_col <- "Monthly_Ret"


x_data1 <- data.frame(temp_id=0,
                      Monthly_Ret=c(seq(1,10,1),1,seq(1,10,1),2,seq(2,10,1),3,3,seq(3,10,1)),
                      stringsAsFactors=FALSE)

x_data2 <- data.frame(temp_id=1,
                      Monthly_Ret=c(seq(1,10,1),10,10,seq(1,9,1),9,9,9,seq(1,8,1),8,seq(1,7,1)),
                      stringsAsFactors=FALSE)

x_data <- data.frame(rbind(x_data1,x_data2),Monthly_Ret_Percent=NA, stringsAsFactors=FALSE)
colnames(x_data)[match("temp_id",names(x_data))] <- identifier

x_data[,"Monthly_Ret"] <- x_data[,"Monthly_Ret"]/10000
x_data[,"Monthly_Ret_Percent"] <- x_data[,"Monthly_Ret"]*100

rm(x_data1,x_data2)


#x_flag_cols <- c("flag_pos","flag_zero","flag_neg")

x_df <- data.frame(x_data,
                   #matrix(NA, ncol=length(x_flag_cols), nrow=length(x_data), dimnames=list(c(), x_flag_cols)),
                   stringsAsFactors=FALSE)


rm(x_data)


###############################################################################
cat("SECTION: DEFINE PARAMETERS - ZERO AND NEGATIVE TEST", "\n")
###############################################################################


zero_neg_screen_execute <- function(data,ret_col,prob_type){
  
  # data <- x
  # ret_col <- analysis_col
  # prob_type <- "normal"
  
  data_s5_1_sum_cols <- c("sum_pos","sum_zero","sum_neg")
  data_s5_1_prob_ind_cols <- c("prob_ind_pos","prob_ind_zero","prob_ind_neg")
  data_s5_1_prob_cum_cols <- c("prob_cum_pos","prob_cum_zero","prob_cum_neg")
  
  data_s5_1_freq <- data.frame(sum_total=NA,matrix(NA, ncol=length(c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols)), nrow=1, 
                                                   dimnames=list(c(), c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols))),stringsAsFactors=FALSE)
  
  data_s5 <- zero_neg_screen_ret_flags(z=data[,analysis_col],analysis_col=analysis_col,flags=c("flag_pos","flag_zero","flag_neg"))
  
  data_s5_1_freq[,c("sum_total",data_s5_1_sum_cols)] <- zero_neg_screen_totals(y=data_s5,flags=c("flag_pos","flag_zero","flag_neg"))
  
  data_s5_1_type <- data.frame(matrix(NA, ncol=2, nrow=3, dimnames=list(c(), c("type","lower_tail"))), stringsAsFactors=FALSE)
  data_s5_1_type[1,] <- c("sum_pos",FALSE)
  data_s5_1_type[2,] <- c("sum_zero",FALSE)
  data_s5_1_type[3,] <- c("sum_neg",TRUE)
  
  data_s5_1_prob_ind_cum_cols <- c(paste(data_s5_1_type[,1],"ind_prop",sep="_"),paste(data_s5_1_type[,1],"cum_prop",sep="_"))
  
  data_s5_1_prob <- data.frame(prob_type=NA,total=NA,  matrix(NA, ncol=2*nrow(data_s5_1_type), nrow=2, dimnames=list(c(), data_s5_1_prob_ind_cum_cols)), 
                               stringsAsFactors=FALSE)
  data_s5_1_prob[,"prob_type"] <- c("population","normal")
  
  data_s5_1_prob[data_s5_1_prob[,"prob_type"]=="population",] <- zero_neg_screen_pop_prob(data_ret=data_s5,data_freq=data_s5_1_freq,type=data_s5_1_type,analysis_col=analysis_col)
  data_s5_1_prob[data_s5_1_prob[,"prob_type"]=="normal",] <- zero_neg_screen_norm_prob(data_ret=data_s5,data_freq=data_s5_1_freq,type=data_s5_1_type,analysis_col=analysis_col)
  
  data_s5_1_freq[,c(data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols)] <- data_s5_1_prob[data_s5_1_prob[,"prob_type"]==prob_type, data_s5_1_prob_ind_cum_cols]
  rm(data_s5_1_prob,data_s5_1_type,data_s5)
  rm(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols,data_s5_1_prob_ind_cum_cols)
  
  return(data_s5_1_freq)
  
}


###############################################################################
cat("SECTION: ZERO AND NEGATIVE TEST", "\n")
###############################################################################


data_s5_1_freq <- ddply(.data=x_df, .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- x_df[x_df[,identifier]==0,]
  # analysis_col <- analysis_col
  # id_col <- identifier

  zero_and_neg_screen <- zero_neg_screen_execute(data=x,ret_col=analysis_col, prob_type="normal")
  
  return(zero_and_neg_screen)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)


### Create Flags

data_s5_1_full <- data.frame(data_s5_1_freq,
                             per_positive_percent_99=NA,per_positive_percent_95=NA,per_positive_percent_90=NA,
                             num_zero_percent_99=NA,num_zero_percent_95=NA,num_zero_percent_90=NA,
                             per_negative_percent_99=NA,per_negative_percent_95=NA,per_negative_percent_90=NA,
                              stringsAsFactors=FALSE)
rm(data_s5_1_freq)

data_s5_1_full[,"per_positive_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.01,1,0)
data_s5_1_full[,"per_positive_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.05,1,0)
data_s5_1_full[,"per_positive_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_pos"]<=0.10,1,0)

data_s5_1_full[,"num_zero_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.01,1,0)
data_s5_1_full[,"num_zero_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.05,1,0)
data_s5_1_full[,"num_zero_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_zero"]<=0.10,1,0)

data_s5_1_full[,"per_negative_percent_99"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.01,1,0)
data_s5_1_full[,"per_negative_percent_95"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.05,1,0)
data_s5_1_full[,"per_negative_percent_90"] <- ifelse(data_s5_1_full[,"prob_cum_neg"]<=0.10,1,0)

data_s5_1_final <- data_s5_1_full[,c(identifier,
                                     "per_positive_percent_99","per_positive_percent_95","per_positive_percent_90",
                                     "num_zero_percent_99","num_zero_percent_95","num_zero_percent_90",
                                     "per_negative_percent_99","per_negative_percent_95","per_negative_percent_90")]

rm2(data_s5_1_full)

