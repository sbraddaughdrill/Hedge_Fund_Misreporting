# TODO: Add comment
# 
# Author:  Brad
# File:    HF_Misreporting_Quant_Screens.R
# Version: 1.0
# Date:    08.26.2014
# Purpose: This is the Performance Screens from Bollen_Pool (2012)
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
external_packages <- c("data.table","gdata","ggplot2","MASS","plyr","quantmod",
                       "reshape2","RSQLite","stringr")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

#crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="\\")
#mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="\\")
#msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="\\")
#similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="\\")
#descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="\\")
#data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="\\")


###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "fund_id"

#analysis_col <- "mktadjret"
analysis_col <- "monthly_ret"

beg_year <- 1994
end_year <- 2011

#descriptive_stats_tables <- ListTables(descriptive_stats_db)
#descriptive_stats_fields <- ListFields(descriptive_stats_db)

data_prescreen <- read.csv(file=paste(output_directory,"data_prescreen.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)


###############################################################################
cat("SECTION: DATA CLEANING", "\n")
###############################################################################

### Preallocate Data
data_trim_lag_count <- 4

data_trim_unlagged_cols <- c("monthly_ret","mktadjret")

#data_trim_lagged_cols <- c(paste(data_trim_unlagged_cols[1],"_lag",seq(1,data_trim_lag_count),sep=""),
#                           paste(data_trim_unlagged_cols[2],"_lag",seq(1,data_trim_lag_count),sep=""))
data_trim_lagged_cols <- unlist(lapply(data_trim_unlagged_cols,function(x,lag_length){ paste(x,"_lag",seq(1,lag_length),sep="") },
                                       lag_length=data_trim_lag_count))

data_trim_lagged_full_cols <- c(data_trim_unlagged_cols,data_trim_lagged_cols)
data_trim_lagged_full_cols <- sort(data_trim_lagged_full_cols)

data_trim_lagged_trim_cols <- data_trim_lagged_full_cols[grepl(analysis_col, data_trim_lagged_full_cols)]

data_trim_id_cols <- c(identifier,"yr","month","yr_month")

data_trim_id_full_cols <- c("return_id",data_trim_id_cols)
data_trim_id_full_cols <- c(identifier,data_trim_id_full_cols[!data_trim_id_full_cols %in% c(identifier)])

data_trim0 <- data.frame(data_prescreen[,c(data_trim_id_cols,data_trim_unlagged_cols)],
                         return_id=NA,
                         matrix(NA, ncol=length(data_trim_unlagged_cols)*data_trim_lag_count, nrow=nrow(data_prescreen), 
                                dimnames=list(c(), data_trim_lagged_cols)),
                         stringsAsFactors=FALSE)

#data_trim0 <- data_trim0[,c(data_trim_id_full_cols, colnames(data_trim0[,!(colnames(data_trim0) %in% c(data_trim_id_full_cols))]))]
data_trim0 <- data_trim0[,c(data_trim_id_full_cols,data_trim_lagged_full_cols)]


### Make sure funds have atleast 12 months of returns
data_trim0_firm_counts <- count(data_trim0, c(identifier))
data_trim0_firm_keep <- data_trim0_firm_counts[data_trim0_firm_counts[,"freq"]>=24,]
data_trim0_firm_keep <- data_trim0_firm_keep[!is.na(data_trim0_firm_keep[,c(identifier)]),]
row.names(data_trim0_firm_keep) <- seq(nrow(data_trim0_firm_keep))

data_trim <- data_trim0[(data_trim0[,c(identifier)] %in% data_trim0_firm_keep[,c(identifier)]),]
row.names(data_trim) <- seq(nrow(data_trim))

rm(data_trim0,data_trim0_firm_counts,data_trim0_firm_keep)


### Create Return ID
data_trim <- data_trim[order(data_trim[,identifier],
                             data_trim[,"yr_month"]),]
row.names(data_trim) <- seq(nrow(data_trim))

data_trim <- ddply(.data=data_trim, .variables=c(identifier), .fun = function(x){ 
  
  x[,"return_id"] <- seq(1,nrow(x))
  
  return(x)
  
},.drop = FALSE)

data_trim <- data_trim[order(data_trim[,identifier],
                             data_trim[,"return_id"]),]
row.names(data_trim) <- seq(nrow(data_trim))

### Populate Lags
for (i in 1:length(data_trim_unlagged_cols))
{
  #i <- 1
  #i <- 2
  #i <- 3
  
  data_trim[,paste(data_trim_unlagged_cols[i],"_lag",seq(1,data_trim_lag_count),sep="")] <- 
    create_lags2(data_trim,data_trim_unlagged_cols[i],identifier,data_trim_lag_count)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(data_trim_unlagged_cols), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 
rm(i)

rm(data_trim_lag_count,data_trim_unlagged_cols,data_trim_lagged_cols,data_trim_lagged_full_cols)

### Create Overall Data

data_trim_overall <- data_trim

data_trim_overall[,identifier] <- 0
data_trim_overall[,"return_id"] <- seq(nrow(data_trim_overall))

### Combine Overall and Group Data

data_trim_full <- rbind(data_trim_overall,data_trim)

rm(data_trim_overall,data_trim)
rm(data_trim_id_cols)


###############################################################################
##
cat("SECTION: SCREEN 1 - DISCONTINUITY AT ZERO", "\n")
##
###############################################################################

data_s1 <- data_trim_full[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)]

###############################################################################
cat("S1 - (1) KINK", "\n")
###############################################################################

data_s1_ret_min <- min(data_s1[data_s1[,identifier]==0,analysis_col])
data_s1_ret_max <- max(data_s1[data_s1[,identifier]==0,analysis_col])

#data_s1_interval <- 0.25
data_s1_interval <- 0.0025
#data_s1_interval <- 0.00025
data_s1_extremes <- round_any(max(abs(data_s1[data_s1[,identifier]==0,analysis_col])),0.005,ceiling)
data_s1_breaks <- seq(-data_s1_extremes,data_s1_extremes,data_s1_interval)

data_s1_bins <- ddply(.data=data_s1, .variables=identifier, .fun = function(x,id_col,analysis_col,breaks,flag_local_breaks,flag_plot){
  
  id_temp <- unique(x[,id_col])
  
  if(flag_local_breaks){ 
    
    ret_min_local <- min(x[,analysis_col])
    ret_max_local <- max(x[,analysis_col])
    
    interval_local <- 0.0025
    #interval_local <- 0.00025
    
    extremes_local <- round_any(max(abs(x[,analysis_col])),0.005,ceiling)
    breaks_local <- seq(-extremes_local,extremes_local,interval_local)
    
    breaks <- breaks_local
    
    rm(ret_min_local,ret_max_local,interval_local,extremes_local,breaks_local)
    
  }
  
  break_keep3 <- which(breaks==0.0)
  break_keep2 <- break_keep3-1
  break_keep1 <- break_keep3-2
  
  bins_temp <- hist(x[,analysis_col], breaks=breaks, plot=flag_plot)
  
  bins_out <- data.frame(matrix(NA, ncol=6, nrow=length(breaks), dimnames=list(c(), c(id_col,"breaks","counts","density","mids","good_bin"))), 
                         stringsAsFactors=FALSE)
  
  bins_out[,id_col] <- id_temp
  bins_out[,"breaks"] <- bins_temp$breaks
  bins_out[,"counts"] <- c(bins_temp$counts,NA)
  bins_out[,"density"] <- c(bins_temp$density,NA)
  bins_out[,"mids"] <- c(bins_temp$mids,NA)
  bins_out[,"good_bin"] <- ifelse(bins_out[,"breaks"] %in% breaks[c(break_keep1,break_keep2,break_keep3)],1,0)
  
  rm(break_keep1,break_keep2,break_keep3,bins_temp)
  
  return(bins_out)
  
}, id_col=identifier, analysis_col=analysis_col,breaks=data_s1_breaks,flag_local_breaks=TRUE,flag_plot=FALSE)

rm(data_s1_ret_min,data_s1_ret_max)
rm(data_s1_interval,data_s1_extremes,data_s1_breaks)

data_s1_bins_keep0 <- data_s1_bins[data_s1_bins[,"good_bin"]==1,]
data_s1_bins_keep <- ddply(.data=data_s1_bins_keep0, .variables=identifier, .fun = function(x){
  
  x_out <- x[order(x[,identifier], x[,"breaks"]),]
  x_out[,"breaks"] <- seq(1,nrow(x))
  
  return(x_out)
  
}, .progress = "none",.inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)

rm(data_s1_bins_keep0)

data_s1_bins_keep[,"breaks"] <- paste("bin_count",data_s1_bins_keep[,"breaks"],sep="")

data_s1_bins_discontinuity0 <- dcast(data_s1_bins_keep[,c(identifier,"breaks","counts")], fund_id ~ breaks, value.var = c("counts"))

rm(data_s1_bins_keep)

data_s1_bins_discontinuity <- data.frame(data_s1_bins_discontinuity0, 
                                         outside_bin_avg=NA, kink_ratio=NA, 
                                         kink_percent_99=NA, kink_percent_95=NA, kink_percent_90=NA, 
                                         kink_percent_75=NA,kink_percent_66=NA,kink_percent_50=NA,
                                         stringsAsFactors=FALSE)
rm(data_s1_bins_discontinuity0)

data_s1_bins_discontinuity[,"outside_bin_avg"] <- rowMeans(data_s1_bins_discontinuity[,c("bin_count1","bin_count3")], na.rm = TRUE)
data_s1_bins_discontinuity[,"kink_ratio"] <- data_s1_bins_discontinuity[,"bin_count2"]/data_s1_bins_discontinuity[,"outside_bin_avg"]
data_s1_bins_discontinuity[,"kink_ratio"] <- ifelse(data_s1_bins_discontinuity[,"bin_count2"]==data_s1_bins_discontinuity[,"outside_bin_avg"],1,data_s1_bins_discontinuity[,"kink_ratio"])
data_s1_bins_discontinuity[,"kink_ratio"] <- ifelse(is.infinite(data_s1_bins_discontinuity[,"kink_ratio"]),NA,data_s1_bins_discontinuity[,"kink_ratio"])

data_s1_bins_discontinuity[,"kink_percent_99"] <- ifelse(data_s1_bins_discontinuity[,"kink_ratio"]<=0.99,1,0)
data_s1_bins_discontinuity[,"kink_percent_95"] <- ifelse(data_s1_bins_discontinuity[,"kink_ratio"]<=0.95,1,0)
data_s1_bins_discontinuity[,"kink_percent_90"] <- ifelse(data_s1_bins_discontinuity[,"kink_ratio"]<=0.90,1,0)
data_s1_bins_discontinuity[,"kink_percent_75"] <- ifelse(data_s1_bins_discontinuity[,"kink_ratio"]<=0.75,1,0)
data_s1_bins_discontinuity[,"kink_percent_66"] <- ifelse(data_s1_bins_discontinuity[,"kink_ratio"]<=0.66,1,0)
data_s1_bins_discontinuity[,"kink_percent_50"] <- ifelse(data_s1_bins_discontinuity[,"kink_ratio"]<=0.50,1,0)

#data_s1_bins_discontinuity_old <- data_s1_bins_discontinuity
#aa <- describe2(data_s1_bins_discontinuity_old[,!(colnames(data_s1_bins_discontinuity_old) %in% c(identifier))])
#bb <- describe2(data_s1_bins_discontinuity[,!(colnames(data_s1_bins_discontinuity) %in% c(identifier))])

data_s1_final <- data_s1_bins_discontinuity[,c(identifier,"kink_percent_99","kink_percent_95","kink_percent_90","kink_percent_75","kink_percent_66","kink_percent_50")]

rm(data_s1_bins_discontinuity,data_s1_bins,data_s1)
gc()


###############################################################################
##
cat("SECTION: SCREEN 2 - LOW CORRELATION WITH OTHER ASSETS", "\n")
##
###############################################################################

###############################################################################
cat("S2 - (1) MAXRSQ ", "\n")
###############################################################################


###############################################################################
cat("S2 - (2) SWITCHRSQ", "\n")
###############################################################################


###############################################################################
cat("S2 - (3) INDEXRSQ", "\n")
###############################################################################







###############################################################################
##
cat("SECTION: SCREEN 3 - UNCONDITIONAL SERIAL CORRELATION", "\n")
##
###############################################################################

data_s3 <- data.frame(data_trim_full[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)],
                      y=NA,x1=NA,
                      stringsAsFactors=FALSE)

data_s3[,"y"] <- data_s3[,analysis_col]
data_s3[,"x1"] <- data_s3[,paste(analysis_col,"lag1",sep="_")]

#colnames(data_s3_1_freq)[match("temp_id",names(data_s3_1_freq))] <- identifier

###############################################################################
cat("S3 - (1) AR(1)", "\n")
###############################################################################

data_s3_1_ids <- unique(data_s3[,c(identifier)])

data_s3_1_reg_cols <- c("Estimate","Std_Error","t_value","Pr_t")

data_s3_1_prob <- data.frame(temp_id=c(data_s3_1_ids),
                             matrix(NA,ncol=length(c(data_s3_1_reg_cols)),nrow=length(data_s3_1_ids),dimnames=list(c(),c(data_s3_1_reg_cols))),
                             ar_1_percent_99=NA,ar_1_percent_95=NA,ar_1_percent_90=NA,
                             stringsAsFactors=FALSE)
colnames(data_s3_1_prob)[match("temp_id",names(data_s3_1_prob))] <- identifier

### Find Serial Correlation

data_s3_1_prob[,data_s3_1_reg_cols] <- ldply(.data=data_s3_1_ids, .fun = function(x,data_ret,reg_cols,id_col,analysis_col){
  
  # x <- 0
  # x <- 5002
  # x <- 5021
  # data_ret <- data_s3
  # reg_cols <- data_s3_1_reg_cols
  # id_col <- identifier
  # analysis_col <- analysis_col
  
  #cat(x, "\n")
  
  data_s3_1_group_coef <- data.frame(var=NA,coefficients(summary(lm(y ~ x1, data=data_ret[data_ret[,id_col]==x,]))), stringsAsFactors=FALSE)
  colnames(data_s3_1_group_coef) <- c("var","Estimate","Std_Error","t_value","Pr_t")
  
  data_s3_1_group_coef[,"var"] <- row.names(data_s3_1_group_coef)
  row.names(data_s3_1_group_coef) <- seq(nrow(data_s3_1_group_coef))
  
  data_s3_1_prob_out <- data.frame(matrix(NA, ncol=length(c(reg_cols)), nrow=1, dimnames=list(c(), c(reg_cols))),stringsAsFactors=FALSE)
  
  for (i in 1:length(reg_cols))
  {
    #i <- 1
    #i <- 2
    
    data_s3_1_prob_out[1,reg_cols[i]]  <- data_s3_1_group_coef[data_s3_1_group_coef[,"var"]=="x1",reg_cols[i]]
    
  }
  
  return(data_s3_1_prob_out)
  
}, data_ret=data_s3,reg_cols=data_s3_1_reg_cols,id_col=identifier,analysis_col=analysis_col,.progress = "text")

data_s3_1_prob[,"ar_1_percent_99"] <- ifelse((data_s3_1_prob[,"Estimate"]>=0 & data_s3_1_prob[,"Pr_t"]<=0.01),1,0)
data_s3_1_prob[,"ar_1_percent_95"] <- ifelse((data_s3_1_prob[,"Estimate"]>=0 & data_s3_1_prob[,"Pr_t"]<=0.05),1,0)
data_s3_1_prob[,"ar_1_percent_90"] <- ifelse((data_s3_1_prob[,"Estimate"]>=0 & data_s3_1_prob[,"Pr_t"]<=0.10),1,0)

data_s3_final <- data_s3_1_prob[,c(identifier,"ar_1_percent_99","ar_1_percent_95","ar_1_percent_90")]

rm(data_s3_1_prob,data_s3_1_ids,data_s3_1_reg_cols,data_s3)
gc()


###############################################################################
##
cat("SECTION: SCREEN 4 - CONDITIONAL SERIAL CORRELATION", "\n")
##
###############################################################################

###############################################################################
cat("S4 - (1) CAR(1)", "\n")
###############################################################################








###############################################################################
##
cat("SECTION: SCREEN 5 - DATA QUALITY", "\n")
##
###############################################################################

data_s5 <- data.frame(data_trim_full[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)],stringsAsFactors=FALSE)


# data_s5_flag_cols <- c("flag_pos","flag_zero","flag_neg")
# 
# data_s5 <- data.frame(data_trim_full[,c(data_trim_id_full_cols,data_trim_lagged_trim_cols)],
#                       matrix(NA, ncol=length(data_s5_flag_cols), nrow=nrow(data_trim_full), dimnames=list(c(), paste(data_s5_flag_cols,"org",sep="_"))),
#                       #Ret_Round=NA,
#                       #matrix(NA, ncol=length(data_s5_flag_cols), nrow=nrow(data_trim_full), dimnames=list(c(), paste(data_s5_flag_cols,"round",sep="_"))),
#                       stringsAsFactors=FALSE)
# 
# data_s5[,"flag_pos_org"] <- ifelse(data_s5[,analysis_col] > 0.00005,1,0)
# data_s5[,"flag_zero_org"] <- ifelse(data_s5[,analysis_col] <= 0.00005 & data_s5[,analysis_col] >= -0.00005,1,0)
# data_s5[,"flag_neg_org"] <- ifelse(data_s5[,analysis_col] < -0.00005,1,0)

#data_s5[,"Ret_Round"] <- round(data_s5[,analysis_col],digits = 4)

#data_s5[,"flag_pos_round"] <- ifelse(data_s5[,"Ret_Round"] >0 ,1,0)
#data_s5[,"flag_zero_round"] <- ifelse(data_s5[,"Ret_Round"] ==0 ,1,0)
#data_s5[,"flag_neg_round"] <- ifelse(data_s5[,"Ret_Round"] <0 ,1,0)

#test <- data_s5[,"flag_pos_org"] - data_s5[,"flag_pos_round"]
#test <- data_s5[,"flag_zero_org"] - data_s5[,"flag_zero_round"]
#test <- data_s5[,"flag_neg_org"] - data_s5[,"flag_neg_round"]
#test_u <- unique(test)

###############################################################################
cat("S5 - (1) NUM_ZERO and (6) PER_NEGATIVE", "\n")
###############################################################################

# data_s5_1_ids <- unique(data_s5[,c(identifier)])
# 
# data_s5_1_sum_cols <- c("sum_pos","sum_zero","sum_neg")
# data_s5_1_prob_ind_cols <- c("prob_ind_pos","prob_ind_zero","prob_ind_neg")
# data_s5_1_prob_cum_cols <- c("prob_cum_pos","prob_cum_zero","prob_cum_neg")
# data_s5_1_num_cols <- c("per_positive_percent_99","per_positive_percent_95","per_positive_percent_90",
#                         "num_zero_percent_99","num_zero_percent_95","num_zero_percent_90",
#                         "per_negative_percent_99","per_negative_percent_95","per_negative_percent_90")
# 
# #data_s5_1_freq <- data.frame(temp_id=c(0,data_s5_1_ids),sum_total=NA,
# #                             matrix(NA, ncol=length(c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols,data_s5_1_num_cols)), nrow=(1+length(data_s5_1_ids)), 
# #                                    dimnames=list(c(), c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols,data_s5_1_num_cols))),stringsAsFactors=FALSE)
# data_s5_1_freq <- data.frame(temp_id=data_s5_1_ids,sum_total=NA,
#                              matrix(NA, ncol=length(c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols,data_s5_1_num_cols)), nrow=length(data_s5_1_ids), 
#                                     dimnames=list(c(), c(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols,data_s5_1_num_cols))),stringsAsFactors=FALSE)
# colnames(data_s5_1_freq)[match("temp_id",names(data_s5_1_freq))] <- identifier
# 
# ### Find Overall Totals
# 
# #data_s5_1_freq[1,data_s5_1_sum_cols] <- colSums(data_s5[,paste(data_s5_flag_cols,"org",sep="_")])
# #data_s5_1_freq[1,c("sum_total")]  <- nrow(data_s5)
# 
# ### Find Group Totals
# 
# data_s5_1_group_temp1 <- ddply(.data=data_s5, .variables=c(identifier), .fun = function(x) colSums(x[,paste(data_s5_flag_cols,"org",sep="_")]))
# 
# data_s5_1_freq[,data_s5_1_sum_cols] <- data_s5_1_group_temp1[,!(colnames(data_s5_1_group_temp1) %in% c(identifier))]
# 
# data_s5_1_group_temp2 <- ddply(.data=data_s5, .variables=c(identifier), .fun = function(x) nrow(x))
# 
# data_s5_1_freq[,c("sum_total")] <- data_s5_1_group_temp2[,!(colnames(data_s5_1_group_temp2) %in% c(identifier))]
# 
# rm(data_s5_1_group_temp1,data_s5_1_group_temp2)
# 
# 
# data_s5_1_type <- data.frame(matrix(NA, ncol=2, nrow=3, dimnames=list(c(), c("type","lower_tail"))), stringsAsFactors=FALSE)
# data_s5_1_type[1,] <- c("sum_pos",FALSE)
# data_s5_1_type[2,] <- c("sum_zero",FALSE)
# data_s5_1_type[3,] <- c("sum_neg",TRUE)
# 
# ### Find Overall Probabilities
# # 
# # 
# # data_ret_overall_trim <- data_s5
# # data_freq_overall_trim <- data_s5_1_freq[data_s5_1_freq[,identifier]==0,]
# # 
# # total_overall <- data.frame(prob_type=NA,total=NA,
# #                             matrix(NA, ncol=2*nrow(data_s5_1_type), nrow=2, 
# #                                    dimnames=list(c(), c(paste(data_s5_1_type[,1],"ind_prop",sep="_"),
# #                                                         paste(data_s5_1_type[,1],"cum_prop",sep="_")))), stringsAsFactors=FALSE)
# # #total_overall <- total_overall[,c("prob_type","total",colnames(total_overall)[c(grep("pos",colnames(total_overall)),
# # #                                                                                grep("zero",colnames(total_overall)),
# # #                                                                                grep("neg",colnames(total_overall)))])]
# # 
# # total_overall[,"prob_type"] <- c("population","normal")
# # total_overall[,"total"] <- data_freq_overall_trim[,"sum_total"]
# # 
# # ### Individual Population Probabilities
# # 
# # total_overall[total_overall[,"prob_type"]=="population","sum_pos_ind_prop"] <- data_freq_overall_trim[,"sum_pos"]/data_freq_overall_trim[,"sum_total"]
# # total_overall[total_overall[,"prob_type"]=="population","sum_zero_ind_prop"] <- data_freq_overall_trim[,"sum_zero"]/data_freq_overall_trim[,"sum_total"]
# # total_overall[total_overall[,"prob_type"]=="population","sum_neg_ind_prop"] <- data_freq_overall_trim[,"sum_neg"]/data_freq_overall_trim[,"sum_total"]
# # 
# # ### Individual Normal Probabilities
# # 
# # mean_overall <- mean(data_ret_overall_trim[,analysis_col], na.rm = FALSE)
# # sd_overall <- sd(data_ret_overall_trim[,analysis_col], na.rm = FALSE)
# # 
# # total_overall[total_overall[,"prob_type"]=="normal","sum_pos_ind_prop"] <- pnorm(0.00005,mean=mean_overall,sd=sd_overall,lower.tail=FALSE)
# # total_overall[total_overall[,"prob_type"]=="normal","sum_zero_ind_prop"] <- pnorm(0.00005,mean=mean_overall,sd=sd_overall,lower.tail=TRUE)-pnorm(-0.00005,mean=mean_overall,sd=sd_overall,lower.tail=TRUE)
# # #total_overall[total_overall[,"prob_type"]=="normal","sum_zero_ind_prop"] <- pnorm(-0.00005,mean=mean_overall,sd=sd_overall,lower.tail=FALSE)-pnorm(0.00005,mean=mean_overall,sd=sd_overall,lower.tail=FALSE)
# # total_overall[total_overall[,"prob_type"]=="normal","sum_neg_ind_prop"] <- pnorm(-0.00005,mean=mean_overall,sd=sd_overall,lower.tail=TRUE)
# # 
# # rm(mean_overall,sd_overall)
# # 
# # ### Binomial Cumulative Probability using Individual Population and Normal Probabilities
# # for (i in 1:nrow(data_s5_1_type))
# # {
# #   # i <- 1
# #   # i <- 2
# #   # i <- 3
# #   
# #   type_temp <- "population"
# #   
# #   total_overall[total_overall[,"prob_type"]==type_temp,paste(data_s5_1_type[i,1],"cum_prop",sep="_")] <- pbinom(q=data_freq_overall_trim[,data_s5_1_type[i,1]],
# #                                                                                                                 size=total_overall[total_overall[,"prob_type"]==type_temp,"total"],
# #                                                                                                                 prob=total_overall[total_overall[,"prob_type"]==type_temp, paste(data_s5_1_type[i,1],"ind_prop",sep="_")],
# #                                                                                                                 lower.tail=as.logical(data_s5_1_type[i,2]), log.p = FALSE)
# #   
# #   type_temp <- "normal"
# #   
# #   total_overall[total_overall[,"prob_type"]==type_temp,paste(data_s5_1_type[i,1],"cum_prop",sep="_")] <- pbinom(q=data_freq_overall_trim[,data_s5_1_type[i,1]],
# #                                                                                                                 size=total_overall[total_overall[,"prob_type"]==type_temp,"total"],
# #                                                                                                                 prob=total_overall[total_overall[,"prob_type"]==type_temp, paste(data_s5_1_type[i,1],"ind_prop",sep="_")],
# #                                                                                                                 lower.tail=as.logical(data_s5_1_type[i,2]), log.p = FALSE)
# #   
# #   rm(type_temp)
# #   
# #   
# # }
# # rm(i)
# # 
# # #model_type_overall <- "population"
# # model_type_overall <- "normal"
# # 
# # data_s5_1_freq[1,c(data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols)] <- total_overall[total_overall[,"prob_type"]==model_type_overall,
# #                                                                                       c(paste(data_s5_1_type[,1],"ind_prop",sep="_"),
# #                                                                                         paste(data_s5_1_type[,1],"cum_prop",sep="_"))]
# # 
# # rm(total_overall)
# # rm(data_ret_overall_trim)
# 
# ### Find Group Probabilities
# 
# data_s5_1_freq[,c(data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols)] <- ldply(.data=data_s5_1_ids, .fun = function(x,data_ret,data_freq,type,id_col,analysis_col,prob_type){
#   
#   # x <- 0
#   # x <- 5002
#   # x <- 5021
#   # data_ret <- data_s5
#   # data_freq <- data_s5_1_freq
#   # type <- data_s5_1_type
#   # id_col <- identifier
#   # analysis_col <- analysis_col
#   # prob_type <- "normal"
#   
#   #fund_id <- x
#   #cat(fund_id, "\n")
#   
#   data_freq_group_trim <- data_freq[data_freq[,id_col]==x,]
#   
#   total_group <- data.frame(prob_type=NA,total=NA,
#                             matrix(NA, ncol=2*nrow(type), nrow=2, 
#                                    dimnames=list(c(), c(paste(type[,1],"ind_prop",sep="_"),paste(type[,1],"cum_prop",sep="_")))), stringsAsFactors=FALSE)
#   
#   total_group[,"prob_type"] <- c("population","normal")
#   total_group[,"total"] <- data_freq_group_trim[,"sum_total"]
#   
#   ### Individual Population Probabilities
#   
#   total_group[total_group[,"prob_type"]=="population","sum_pos_ind_prop"] <- data_freq_group_trim[,"sum_pos"]/data_freq_group_trim[,"sum_total"]
#   total_group[total_group[,"prob_type"]=="population","sum_zero_ind_prop"] <- data_freq_group_trim[,"sum_zero"]/data_freq_group_trim[,"sum_total"]
#   total_group[total_group[,"prob_type"]=="population","sum_neg_ind_prop"] <- data_freq_group_trim[,"sum_neg"]/data_freq_group_trim[,"sum_total"]
#   
#   ### Individual Normal Probabilities
#   
#   mean_group <- mean(data_ret[data_ret[,id_col]==x,analysis_col], na.rm = FALSE)
#   sd_group <- sd(data_ret[data_ret[,id_col]==x,analysis_col], na.rm = FALSE)
#   
#   total_group[total_group[,"prob_type"]=="normal","sum_pos_ind_prop"] <- pnorm(0.00005,mean=mean_group,sd=sd_group,lower.tail=FALSE)
#   total_group[total_group[,"prob_type"]=="normal","sum_zero_ind_prop"] <- pnorm(0.00005,mean=mean_group,sd=sd_group,lower.tail=TRUE)-pnorm(-0.00005,mean=mean_group,sd=sd_group,lower.tail=TRUE)
#   #total_group[total_group[,"prob_type"]=="normal","sum_zero_ind_prop"] <- pnorm(-0.00005,mean=mean_group,sd=sd_group,lower.tail=FALSE)-pnorm(0.00005,mean=mean_group,sd=sd_group,lower.tail=FALSE)
#   total_group[total_group[,"prob_type"]=="normal","sum_neg_ind_prop"] <- pnorm(-0.00005,mean=mean_group,sd=sd_group,lower.tail=TRUE)
#   
#   rm(mean_group,sd_group)
#   
#   ### Binomial Cumulative Probability using Individual Population and Normal Probabilities
#   for (i in 1:nrow(type))
#   {
#     # i <- 1
#     # i <- 2
#     # i <- 3
#     
#     type_temp <- "population"
#     
#     total_group[total_group[,"prob_type"]==type_temp,paste(type[i,1],"cum_prop",sep="_")] <- pbinom(q=data_freq_group_trim[,type[i,1]],
#                                                                                                     size=total_group[total_group[,"prob_type"]==type_temp,"total"],
#                                                                                                     prob=total_group[total_group[,"prob_type"]==type_temp, paste(data_s5_1_type[i,1],"ind_prop",sep="_")],
#                                                                                                     lower.tail=as.logical(type[i,2]), log.p = FALSE)
#     
#     type_temp <- "normal"
#     
#     total_group[total_group[,"prob_type"]==type_temp,paste(type[i,1],"cum_prop",sep="_")] <- pbinom(q=data_freq_group_trim[,type[i,1]],
#                                                                                                     size=total_group[total_group[,"prob_type"]==type_temp,"total"],
#                                                                                                     prob=total_group[total_group[,"prob_type"]==type_temp, paste(data_s5_1_type[i,1],"ind_prop",sep="_")],
#                                                                                                     lower.tail=as.logical(type[i,2]), log.p = FALSE)
#     
#     rm(type_temp)
#     
#     
#   }
#   rm(i)
#   
#   
#   return(total_group[total_group[,"prob_type"]==prob_type,c(paste(type[,1],"ind_prop",sep="_"),paste(type[,1],"cum_prop",sep="_"))])
#   
# }, data_ret=data_s5,data_freq=data_s5_1_freq,type=data_s5_1_type,id_col=identifier,analysis_col=analysis_col,prob_type="normal",.progress = "text")
# 
# data_s5_1_freq[,"per_positive_percent_99"] <- ifelse(data_s5_1_freq[,"prob_cum_pos"]<=0.01,1,0)
# data_s5_1_freq[,"per_positive_percent_95"] <- ifelse(data_s5_1_freq[,"prob_cum_pos"]<=0.05,1,0)
# data_s5_1_freq[,"per_positive_percent_90"] <- ifelse(data_s5_1_freq[,"prob_cum_pos"]<=0.10,1,0)
# 
# data_s5_1_freq[,"num_zero_percent_99"] <- ifelse(data_s5_1_freq[,"prob_cum_zero"]<=0.01,1,0)
# data_s5_1_freq[,"num_zero_percent_95"] <- ifelse(data_s5_1_freq[,"prob_cum_zero"]<=0.05,1,0)
# data_s5_1_freq[,"num_zero_percent_90"] <- ifelse(data_s5_1_freq[,"prob_cum_zero"]<=0.10,1,0)
# 
# data_s5_1_freq[,"per_negative_percent_99"] <- ifelse(data_s5_1_freq[,"prob_cum_neg"]<=0.01,1,0)
# data_s5_1_freq[,"per_negative_percent_95"] <- ifelse(data_s5_1_freq[,"prob_cum_neg"]<=0.05,1,0)
# data_s5_1_freq[,"per_negative_percent_90"] <- ifelse(data_s5_1_freq[,"prob_cum_neg"]<=0.10,1,0)
# 
# data_s5_1_final <- data_s5_1_freq[,c(identifier,
#                                      "per_positive_percent_99","per_positive_percent_95","per_positive_percent_90",
#                                      "num_zero_percent_99","num_zero_percent_95","num_zero_percent_90",
#                                      "per_negative_percent_99","per_negative_percent_95","per_negative_percent_90")]
# 
# rm(data_s5_1_freq,data_s5_1_ids,data_s5_1_type)
# rm(data_s5_1_sum_cols,data_s5_1_prob_ind_cols,data_s5_1_prob_cum_cols,data_s5_1_num_cols)
# gc()

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

data_s5_1_freq <- ddply(.data=data_s5[c(data_trim_id_full_cols,analysis_col)], .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s5[data_s5[,identifier]==0, c(data_trim_id_full_cols,analysis_col)]
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


###############################################################################
cat("S5 - (2) PER_REPEATS", "\n")
###############################################################################










###############################################################################
cat("S5 - (3) STRING", "\n")
###############################################################################









###############################################################################
cat("S5 - (4) NUM_PAIRS", "\n")
###############################################################################









###############################################################################
cat("S5 - (5) UNIFORM", "\n")
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


data_s5_5_gof <- ddply(.data=data_s5[c(data_trim_id_full_cols,analysis_col)], .variables=identifier, .fun = function(x,analysis_col,id_col){
  
  # x <- data_s5[data_s5[,identifier]==0,c(data_trim_id_full_cols,analysis_col)]
  # x <- uniform_data[uniform_data[,identifier]==0,]
  # analysis_col <- analysis_col
  # id_col <- identifier
  
  uniform_out <- uniform_screen_execute(data=x,ret_col=analysis_col,graph=FALSE)
  
  return(uniform_out)
  
}, analysis_col=analysis_col,id_col=identifier, .progress = "text", .inform = FALSE, .drop = TRUE)


### Create Uniform Flags

data_s5_5_gof_full <- data.frame(data_s5_5_gof,
                              uniform_percent_99=NA, uniform_percent_95=NA, uniform_percent_90=NA, 
                              stringsAsFactors=FALSE)
rm(data_s5_5_gof)

data_s5_5_gof_full[,"uniform_percent_99"] <- ifelse(data_s5_5_gof_full[,"pval_uniform"]<=0.01,1,0)
data_s5_5_gof_full[,"uniform_percent_95"] <- ifelse(data_s5_5_gof_full[,"pval_uniform"]<=0.05,1,0)
data_s5_5_gof_full[,"uniform_percent_90"] <- ifelse(data_s5_5_gof_full[,"pval_uniform"]<=0.10,1,0)

data_s5_5_final <- data_s5_5_gof_full[,c(identifier, "uniform_percent_99", "uniform_percent_95", "uniform_percent_90")]

rm2(data_s5_5_gof_full)


###############################################################################
cat("S5 - MERGE FLAGS", "\n")
###############################################################################

data_s5_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_s5_final0)[match("temp_id",names(data_s5_final0))] <- identifier

data_s5_final1 <- merge(data_s5_final0, data_s5_1_final, 
               by.x=c(identifier), by.y=c(identifier), 
               all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_s5_final0,data_s5_1_final)

data_s5_final2 <- merge(data_s5_final1, data_s5_5_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_s5_final1,data_s5_5_final)

data_s5_final <- data_s5_final2

#rm(data_s5,data_s5_final2)
gc()



###############################################################################
cat("MERGE ALL FLAGS", "\n")
###############################################################################

data_screen_final0 <- data.frame(temp_id=unique(data_trim_full[,identifier]),stringsAsFactors=FALSE)
colnames(data_screen_final0)[match("temp_id",names(data_screen_final0))] <- identifier

data_screen_final1 <- merge(data_screen_final0, data_s1_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_screen_final0,data_s1_final)

data_screen_final2 <- merge(data_screen_final1, data_s3_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_screen_final1,data_s3_final)

data_screen_final3 <- merge(data_screen_final2, data_s5_final, 
                        by.x=c(identifier), by.y=c(identifier), 
                        all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

#rm(data_screen_final2,data_s5_final)

data_screen_final <- data_screen_final3

#rm(data_prescreen,data_trim_full,data_screen_final3)
gc()


###############################################################################
cat("OUTPUT FLAGS", "\n")
###############################################################################

data_screens <- data_screen_final 

write.csv(data_screen_final,file=paste(output_directory,"data_screens.csv",sep="\\"),na="",quote=TRUE,row.names=FALSE)

rm(data_screen_final)
rm(data_trim_id_full_cols,data_trim_lagged_trim_cols)