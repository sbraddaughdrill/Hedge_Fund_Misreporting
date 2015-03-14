# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Multivariate_Probit.R
# Version: 1.0
# Date: 09.29.2014
# Purpose: Run regressions
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
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
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
# c("compare","cwhmisc","data.table","descr","fastmatch","formatR",
#   "gtools","Hmisc","installr","knitr","leaps","lmtest","markdown","memisc","mitools",
#   "pander","pbapply","PerformanceAnalytics","plm","psych","quantreg","R.oo","R2wd",
#   "reporttools","reshape2","rms","sandwich","sqldf","stargazer","stringr",
#   "texreg","taRifx","UsingR","xtable","zoo")
external_packages <- c("aod","plyr","gdata","RSQLite")
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
#descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")
#data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="")


###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "Fund_ID"

beg_year <- 1994
#beg_year <- 2007
end_year <- 2013

#strat_col <- "main_investment_strategy"
strat_col <- "Primary_Investment_Strategy_combcol"

#descriptive_stats_tables <- ListTables(descriptive_stats_db)
#descriptive_stats_fields <- ListFields(descriptive_stats_db)

data_all0 <- read.csv(file=paste(output_directory,"data_all_tone",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

data_all0 <- data_all0[order(data_all0[,identifier],
                             data_all0[,"yr"],
                             data_all0[,"month"]),]
row.names(data_all0) <- seq(nrow(data_all0))


###############################################################################
cat("TRIM DATA", "\n")
###############################################################################

#data_all0 <- data_all0[(data_all0[,"yr"]>=beg_year & data_all0[,"yr"]<=end_year),]
data_all0 <- data_all0[,!(colnames(data_all0) %in% c("Fund_Name","Secondary_Investment_Strategy","Strategy","Strat_ID"))]


###############################################################################
cat("CLEAN DATA", "\n")
###############################################################################

data_all0[,"date"] <- as.Date(data_all0[,"date"],format="%Y-%m-%d")
data_all0[,"Date_Added"] <- as.Date(data_all0[,"Date_Added"],format="%Y-%m-%d")
data_all0[,"chgdt"] <- as.Date(data_all0[,"chgdt"],format="%Y-%m-%d")
data_all0[,"Inception_Date"] <- as.Date(data_all0[,"Inception_Date"],format="%Y-%m-%d")

data_all0[,strat_col] <- ifelse(data_all0[,strat_col]=="",NA,data_all0[,strat_col])

for(k in which(sapply(data_all0,class)!="Date"))
{
  #k <- 1
  
  data_all0[[k]] <- unknownToNA(data_all0[[k]], unknown=unknowns_strings,force=TRUE)
  data_all0[[k]] <- ifelse(is.na(data_all0[[k]]),NA,data_all0[[k]])
}
rm2(k)


###############################################################################
cat("MAKE SURE ONLY 1 FIRM-YR-MONTH COMBO", "\n")
###############################################################################

data_all_trim <- data_all0[(data_all0[,"yr"]>=beg_year & data_all0[,"yr"]<=end_year),]

rm2(data_all0)

id_counts <- count(data_all_trim,c(identifier,"yr_month"))

id_counts_single <- id_counts[id_counts[,"freq"]==1,]
id_counts_mult <- id_counts[id_counts[,"freq"]>1,]

id_counts_good <- data_all_trim[!(data_all_trim[,identifier] %in% unique(id_counts_mult[,identifier])),]
id_counts_bad <- data_all_trim[(data_all_trim[,identifier] %in% unique(id_counts_mult[,identifier])),]

id_counts_bad_trim <- ddply(.data=id_counts_bad, .variables=c(identifier,"yr_month"), .fun = function(x){
  
  # x <- id_counts_bad[(id_counts_bad[,identifier]==5021 & id_counts_bad[,"yr_month"]=="2012_01"),]
  #return(tail(x,1))
  return(x[1,])
}, .progress = "text")

data_all <- rbind(id_counts_good,id_counts_bad_trim)

rm2(id_counts,id_counts_single,id_counts_mult,id_counts_good,id_counts_bad,id_counts_bad_trim,data_all_trim)


###############################################################################
cat("COMPUTE ADDITIONAL VARIABLES", "\n")
###############################################################################

#max(data_all[,"quality_score_trim2_90"],na.rm=TRUE)
#min(data_all[,"quality_score_trim2_90"],na.rm=TRUE)
#mean(data_all[,"quality_score_trim2_90"],na.rm=TRUE)
#median(data_all[,"quality_score_trim2_90"],na.rm=TRUE)
#t(quantile(data_all[,"quality_score_trim2_90"],c(0.01,0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95,0.99),na.rm=TRUE))
#t(quantile(data_all[,"quality_score_trim2_90"],seq(.01,1,.01),na.rm=TRUE))


### Create Quality Score Quantile DVs

descrip_stats_data_quantile_cols <- c("qscore_90_Median1","qscore_90_Median2",
                                      "qscore_90_Quartile1","qscore_90_Quartile23","qscore_90_Quartile4")
## "qscore_90_Tercile1","qscore_90_Tercile2","qscore_90_Tercile3"
## "qscore_90_Quintile1","qscore_90_Quintile234","qscore_90_Quintile5"

descrip_stats_data_quantile <-  data.frame(data_all, 
                                           matrix(NA, ncol=length(descrip_stats_data_quantile_cols), nrow=nrow(data_all), dimnames=list(c(), descrip_stats_data_quantile_cols)), stringsAsFactors=FALSE)

### Median DVs

descrip_stats_data_quantile[,"qscore_90_Median1"] <- ifelse((descrip_stats_data_quantile[,"quality_score_trim2_90"]>=0 & 
                                                               descrip_stats_data_quantile[,"quality_score_trim2_90"]<=2),1,0)
descrip_stats_data_quantile[,"qscore_90_Median2"] <- ifelse((descrip_stats_data_quantile[,"quality_score_trim2_90"]>=3 & 
                                                               descrip_stats_data_quantile[,"quality_score_trim2_90"]<=7),1,0)

### Quartile DVs

descrip_stats_data_quantile[,"qscore_90_Quartile1"] <- ifelse((descrip_stats_data_quantile[,"quality_score_trim2_90"]>=0 & 
                                                                 descrip_stats_data_quantile[,"quality_score_trim2_90"]<=1),1,0)
descrip_stats_data_quantile[,"qscore_90_Quartile23"] <- ifelse((descrip_stats_data_quantile[,"quality_score_trim2_90"]>=2 & 
                                                                  descrip_stats_data_quantile[,"quality_score_trim2_90"]<=3),1,0)
descrip_stats_data_quantile[,"qscore_90_Quartile4"] <- ifelse((descrip_stats_data_quantile[,"quality_score_trim2_90"]>=4 & 
                                                                 descrip_stats_data_quantile[,"quality_score_trim2_90"]<=7),1,0)


#utils::View(descrip_stats_data_quantile[,c(identifier,"yr","quality_score_trim2_90",descrip_stats_data_quantile_cols)])

rm2(data_all,descrip_stats_data_quantile_cols)


### Create Monthly Level Data

descrip_stats_data_monthly0 <- descrip_stats_data_quantile

descrip_stats_fund_vars_remove <- c("month",
                                    "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
                                    "nflow_lag1","nflow_lag2","nflow_lag3","nflow_lag4",
                                    "pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                                    "age_m","chgdt",
                                    "mktadjret_sq_lag1","mktadjret_lag2_sq","mktadjret_lag3_sq","mktadjret_lag4_sq",
                                    "sdnet_flow_lag1","sdpct_flow_lag1")

descrip_stats_data_monthly0 <- descrip_stats_data_monthly0[,!(colnames(descrip_stats_data_monthly0) %in% descrip_stats_fund_vars_remove)]
row.names(descrip_stats_data_monthly0) <- seq(nrow(descrip_stats_data_monthly0))

descrip_stats_ios_vars_remove <- c("month",
                                   "punct_ios","conjunctions_ios","prepositions_ios","normalized_space_ios", 
                                   "pronouns_ios","ttr_ios")

descrip_stats_data_monthly0 <- descrip_stats_data_monthly0[,!(colnames(descrip_stats_data_monthly0) %in% descrip_stats_ios_vars_remove)]
row.names(descrip_stats_data_monthly0) <- seq(nrow(descrip_stats_data_monthly0))

descrip_stats_data_monthly <- data.frame(descrip_stats_data_monthly0,
                                         year_group_id=NA,
                                         stringsAsFactors=FALSE)

descrip_stats_data_monthly[,"year_group_id"] <- ifelse((descrip_stats_data_monthly[,"yr"]>=1994 & descrip_stats_data_monthly[,"yr"]<=1999), 1, 
                                                       ifelse((descrip_stats_data_monthly[,"yr"]>=2000 & descrip_stats_data_monthly[,"yr"]<=2006), 2, 
                                                              ifelse((descrip_stats_data_monthly[,"yr"]>=2007 & descrip_stats_data_monthly[,"yr"]<=2013), 3, 
                                                                     descrip_stats_data_monthly[,"year_group_id"])))   

rm2(descrip_stats_data_monthly0)


### Create Aggregate Averages From Monthly Variables

descrip_stats_data_monthly_averages <- ddply(.data=descrip_stats_data_monthly, .variables=identifier, .fun = function(z){ 
  
  # z <- descrip_stats_data_monthly[descrip_stats_data_monthly[,identifier]==5002,]
  
  z_out <- data.frame(z,
                      avg_return=mean(z[,"Monthly_Ret"],na.rm=TRUE),
                      volatilty=sd(z[,"Monthly_Ret"],na.rm=TRUE),
                      size=suppressWarnings(log(max(z[,"AUM"],na.rm=TRUE))),
                      max_age_y=max(z[,"age_y"],na.rm=TRUE),
                      stringsAsFactors=FALSE)
  
  return(z_out)
  
}, .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

rm2(descrip_stats_data_monthly)


### Create Yearly Level Data

monthly_level_cols <- c("date","yr_month","age_y","nflow","pflow","mktadjret","mktadjret_sq","fund_ret_mkt_neg",
                        "Monthly_Ret","Monthly_Ret_lag1","Monthly_Ret_lag2","Monthly_Ret_lag3","Monthly_Ret_lag4",
                        "AUM","AUM_lag1","AUM_lag2","AUM_lag3","AUM_lag4",
                        "AUM_log","AUM_log_lag1","AUM_log_lag2","AUM_log_lag3","AUM_log_lag4",
                        "vwretd","vwretd_annualized","vwretx","vwretx_annualized")

descrip_stats_data_yearly0 <-  descrip_stats_data_monthly_averages[,!(colnames(descrip_stats_data_monthly_averages) %in% monthly_level_cols)]
descrip_stats_data_yearly <- unique(descrip_stats_data_yearly0)
row.names(descrip_stats_data_yearly) <- seq(nrow(descrip_stats_data_yearly))

rm2(descrip_stats_data_yearly0)


### Create Aggregate Averages From Yearly Variables

yearly_avg_cols <- c("all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios",
                     "all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
                     "Primary_Investment_Strategy_combcol_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
                     "Primary_Investment_Strategy_combcol_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios",
                     "all_similarity_050pct_ios_below_quartile1","all_similarity_050pct_ios_above_quartile3",
                     "Primary_Investment_Strategy_combcol_similarity_050pct_ios_below_quartile1","Primary_Investment_Strategy_combcol_similarity_050pct_ios_above_quartile3",
                     "all_similarity_100pct_ios_below_quartile1","all_similarity_100pct_ios_above_quartile3",
                     "Primary_Investment_Strategy_combcol_similarity_100pct_ios_below_quartile1","Primary_Investment_Strategy_combcol_similarity_100pct_ios_above_quartile3",
                     "all_similarity_250pct_ios_below_quartile1","all_similarity_250pct_ios_above_quartile3",
                     "Primary_Investment_Strategy_combcol_similarity_250pct_ios_below_quartile1","Primary_Investment_Strategy_combcol_similarity_250pct_ios_above_quartile3",
                     "all_similarity_500pct_ios_below_quartile1","all_similarity_500pct_ios_above_quartile3",
                     "Primary_Investment_Strategy_combcol_similarity_500pct_ios_below_quartile1","Primary_Investment_Strategy_combcol_similarity_500pct_ios_above_quartile3",
                     "all_similarity_750pct_ios_below_quartile1","all_similarity_750pct_ios_above_quartile3",
                     "Primary_Investment_Strategy_combcol_similarity_750pct_ios_below_quartile1","Primary_Investment_Strategy_combcol_similarity_750pct_ios_above_quartile3",
                     "all_similarity_900pct_ios_below_quartile1","all_similarity_900pct_ios_above_quartile3")

descrip_stats_data_yearly_averages0 <- data.frame(descrip_stats_data_yearly,
                                                  matrix(NA, ncol=length(yearly_avg_cols), nrow=nrow(descrip_stats_data_yearly), dimnames=list(c(),paste("avg",yearly_avg_cols,sep="_"))),stringsAsFactors=FALSE)

descrip_stats_data_yearly_averages <- ddply(.data=descrip_stats_data_yearly_averages0, .variables=identifier, .fun = function(z,yearly_cols){ 
  
  # z <- descrip_stats_data_yearly_averages0[descrip_stats_data_yearly_averages0[,identifier]==5002,]
  # yearly_cols <- yearly_avg_cols
  
  z_out <- z
  
  for (j in 1:length(yearly_cols))
  {
    # j <- 1
    z_out[,paste("avg",yearly_cols[j],sep="_")] <- mean(z_out[,yearly_cols[j]],na.rm=TRUE)
  }
  rm(j)  
  
  return(z_out)
  
}, yearly_cols=yearly_avg_cols,.progress = "none")

descrip_stats_data_yearly_averages <- descrip_stats_data_yearly_averages[,colnames(descrip_stats_data_yearly_averages)[!(colnames(descrip_stats_data_yearly_averages) %in% c(yearly_avg_cols))]]

rm2(descrip_stats_data_yearly,descrip_stats_data_yearly_averages0,yearly_avg_cols)


### Create Aggregate Level Data

descrip_stats_strategy_cols <- unique(descrip_stats_data_yearly_averages[,strat_col])
descrip_stats_strategy_cols <- sort(descrip_stats_strategy_cols)
descrip_stats_strategy_cols <- c(descrip_stats_strategy_cols[!(descrip_stats_strategy_cols %in% c("Others"))],"Others")

descrip_stats_ios_read_cols <- c("sentences_ios","words_ios","chars_no_space_ios","num_syll_ios","sntc_per_word_ios",
                                 "avg_sentc_length_ios","avg_word_length_ios","avg_syll_word_ios","sntc_per100_ios",
                                 "syll_per100_ios","lett_per100_ios","fog_hard_words_ios",
                                 "ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                                 "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios")

descrip_stats_ios_sim_cols <- names(descrip_stats_data_yearly_averages)[grep("pct_ios", names(descrip_stats_data_yearly_averages))] 

descrip_stats_ios_quartile_cols <- c(names(descrip_stats_data_yearly_averages)[grep("below_quartile1", names(descrip_stats_data_yearly_averages))],
                                     names(descrip_stats_data_yearly_averages)[grep("above_quartile3", names(descrip_stats_data_yearly_averages))])

descrip_stats_pattern_cols_99 <- c("per_positive_percent_99","num_zero_percent_99","per_repeats_percent_99","uniform_percent_99",
                                   "string_percent_99","num_pairs_percent_99","per_negative_percent_99","ar_1_percent_99","indexrsq_percent_99",
                                   "kink_percent_99","quality_score_trim0_99","quality_score_trim1_99","quality_score_trim2_99")
descrip_stats_pattern_cols_95 <- c("per_positive_percent_95","num_zero_percent_95","per_repeats_percent_95","uniform_percent_95",
                                   "string_percent_95","num_pairs_percent_95","per_negative_percent_95","ar_1_percent_95","indexrsq_percent_95",
                                   "kink_percent_95","quality_score_trim0_95","quality_score_trim1_95","quality_score_trim2_95")
#descrip_stats_pattern_cols_90 <- c("per_positive_percent_90","num_zero_percent_90","per_repeats_percent_90","uniform_percent_90",
#                                   "string_percent_90","num_pairs_percent_90","per_negative_percent_90","ar_1_percent_90","indexrsq_percent_90",
#                                   "kink_percent_90","quality_score_trim0_90","quality_score_trim1_90","quality_score_trim2_90")
descrip_stats_pattern_cols_90 <- c("per_positive_percent_90","num_zero_percent_90","per_repeats_percent_90","uniform_percent_90",
                                   "string_percent_90","num_pairs_percent_90","per_negative_percent_90","ar_1_percent_90","indexrsq_percent_90",
                                   "kink_percent_90","quality_score_trim0_90","quality_score_trim1_90","quality_score_trim2_90",
                                   "qscore_90_Median1","qscore_90_Median2",
                                   "qscore_90_Quartile1","qscore_90_Quartile23","qscore_90_Quartile4")

descrip_stats_pattern_cols <- c(descrip_stats_pattern_cols_99,descrip_stats_pattern_cols_95,descrip_stats_pattern_cols_90)
#descrip_stats_pattern_cols_trim <- descrip_stats_pattern_cols[!(descrip_stats_pattern_cols %in% c(descrip_stats_pattern_cols[grep("per_positive", descrip_stats_pattern_cols)],descrip_stats_pattern_cols[grep("per_repeats", descrip_stats_pattern_cols)]))]
descrip_stats_pattern_cols_trim <- descrip_stats_pattern_cols[!(descrip_stats_pattern_cols %in% c(descrip_stats_pattern_cols[grep("per_positive", descrip_stats_pattern_cols)]))]

rm2(descrip_stats_pattern_cols_99,descrip_stats_pattern_cols_95,descrip_stats_pattern_cols_90)

#yearly_level_cols <- c("yr","year_group_id","sdnet_flow","sdpct_flow",descrip_stats_ios_read_cols,
#                       descrip_stats_ios_sim_cols,descrip_stats_ios_quartile_cols)
yearly_level_cols <- c("yr","year_group_id","sdnet_flow","sdpct_flow",descrip_stats_ios_quartile_cols)

descrip_stats_data_aggregate0 <-  descrip_stats_data_yearly_averages[,!(colnames(descrip_stats_data_yearly_averages) %in% yearly_level_cols)]
#descrip_stats_data_aggregate <- unique(descrip_stats_data_aggregate0)
descrip_stats_data_aggregate <- data.frame(yr=NA,year_group_id=NA,unique(descrip_stats_data_aggregate0),stringsAsFactors=FALSE)
descrip_stats_data_aggregate[,"yr"] <- 9999
descrip_stats_data_aggregate[,"year_group_id"] <- 9999
row.names(descrip_stats_data_aggregate) <- seq(nrow(descrip_stats_data_aggregate))

rm2(descrip_stats_data_aggregate0)

###############################################################################
cat("CORRELATION - READBILITY", "\n")
###############################################################################

#"kink_percent_90","indexrsq_percent_90","ar_1_percent_90","num_zero_percent_90","uniform_percent_90",
#"string_percent_90","num_pairs_percent_90","per_negative_percent_90",

cor_read <- corstarsl(descrip_stats_data_aggregate[,c("qscore_90_Median1","qscore_90_Median2",
                                                      "qscore_90_Quartile1","qscore_90_Quartile23","qscore_90_Quartile4",
                                                      "avg_grade_level_ios","ARI_ios", "Coleman_Liau_ios", "Flesch_Kincaid_ios", "FOG_ios", "SMOG_ios", 
                                                      "avg_return","volatilty","size","max_age_y",
                                                      "Listed_on_Exchange_bin","Domicile_onshore_bin","Flagship_bin","Dead_bin")],round=3)
# utils::View(cor_read)

cor_sim <- corstarsl(descrip_stats_data_aggregate[,c( "qscore_90_Median1","qscore_90_Median2",
                                                      "qscore_90_Quartile1","qscore_90_Quartile23","qscore_90_Quartile4",
                                                      "avg_all_similarity_900pct_ios","avg_Primary_Investment_Strategy_combcol_similarity_900pct_ios",
                                                      "avg_return","volatilty","size","max_age_y",
                                                      "Listed_on_Exchange_bin","Domicile_onshore_bin","Flagship_bin","Dead_bin")],round=3)
# utils::View(cor_sim)

cor_tone <- corstarsl(descrip_stats_data_aggregate[,c("qscore_90_Median1","qscore_90_Median2",
                                                      "qscore_90_Quartile1","qscore_90_Quartile23","qscore_90_Quartile4",
                                                      "per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty",
                                                      "avg_return","volatilty","size","max_age_y",
                                                      "Listed_on_Exchange_bin","Domicile_onshore_bin","Flagship_bin","Dead_bin")],round=3)
# utils::View(cor_tone)


# ###############################################################################
# cat("PANEL REGRESSION - VARIABLES", "\n")
# ###############################################################################
# 
# pattern_str <- "kink_percent_90  + indexrsq_percent_90 + ar_1_percent_90 + num_zero_percent_90 + uniform_percent_90 + string_percent_90 + num_pairs_percent_90 + per_negative_percent_90"
# quality_str <- "num_zero_percent_90 + uniform_percent_90 + string_percent_90 + num_pairs_percent_90 + per_negative_percent_90"
# nonquality_str <- "kink_percent_90  + indexrsq_percent_90 + ar_1_percent_90"
# pb_str <- "kink_percent_90  + indexrsq_percent_90 + ar_1_percent_90 + num_zero_percent_90 + uniform_percent_90 + string_percent_90"

###############################################################################
cat("PANEL REGRESSION - READBILITY", "\n")
###############################################################################

reg_read_1_logit <- glm(as.formula(paste("qscore_90_Quartile1","avg_grade_level_ios",sep="~")), 
                        family = binomial(link = "logit"), 
                        data=descrip_stats_data_aggregate)

summary(reg_read_1_logit)

## CIs using profiled log-likelihood
confint(reg_read_1_logit)

## CIs using standard errors
confint.default(reg_read_1_logit)

wald.test(b = coef(reg_read_1_logit), Sigma = vcov(reg_read_1_logit), Terms = 2:2)
wald.test(b = coef(reg_read_1_logit), Sigma = vcov(reg_read_1_logit), L = cbind(0,1))

## odds ratios and 95% CI
exp(cbind(OR = coef(reg_read_1_logit), confint(reg_read_1_logit)))

reg_read_1_probit <- glm(as.formula(paste("qscore_90_Quartile1","avg_grade_level_ios",sep="~")), 
                         family = binomial(link = "probit"), 
                         data=descrip_stats_data_aggregate)

summary(reg_read_1_probit)

## CIs using profiled log-likelihood
confint(reg_read_1_probit)

## CIs using standard errors
confint.default(reg_read_1_probit)

wald.test(b = coef(reg_read_1_probit), Sigma = vcov(reg_read_1_probit), Terms = 2:2)
wald.test(b = coef(reg_read_1_probit), Sigma = vcov(reg_read_1_probit), L = cbind(0,1))


































#Regression equations
regression_equations1_1 <- list(#time_frame=list(c(beg_year,2000,2000,2006),
  #                c(end_year,2011,2005,2011)),
  time_frame=list(c(9999),c(9999)),
  dep_var= c("kink_percent_90","indexrsq_percent_90","ar_1_percent_90","num_zero_percent_90","uniform_percent_90","string_percent_90","num_pairs_percent_90",
             "per_negative_percent_90","qscore_90_Median1","qscore_90_Median2","qscore_90_Quartile1","qscore_90_Quartile4"),
  models=c("avg_grade_level_XXX ",
           "ari_XXX             ",
           "coleman_liau_XXX    ",
           "flesch_kincaid_XXX  ",
           "fog_XXX             ",
           "smog_XXX            ",
           "avg_grade_level_XXX + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "ari_XXX             + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "coleman_liau_XXX    + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "flesch_kincaid_XXX  + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "fog_XXX             + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "smog_XXX            + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin "),
  model_type=c("pooling"),
  note=c("readability"))

regression_equations1 <- list(regression_equations1_1)
rm(regression_equations1_1)

regression_equations_expand1 <- regression_expand(regression_equations1)

regression_equations_expand_adj_read1 <- regression_read_adjustment(equations_df=regression_equations_expand1,equations_col="indep_var",
                                                                    name_col="outname_short", read_prefix="XXX", read_list="ios",
                                                                    index_list=c("date_index","list_index","dep_index","model_index"))

regression_equations_expand_adj_sim1 <- regression_sim_adjustment(equations_df=regression_equations_expand_adj_read1,equations_col="indep_var",
                                                                  name_col="outname_short", sim_prefix="YYYpct", sim_list=c('NOSIM'),
                                                                  index_list=c("date_index","list_index","dep_index","model_index","read_index"))

regression_equations_final1 <- regression_equations_expand_adj_sim1
regression_equations_final1 <- regression_equations_final1[order(regression_equations_final1[,"date_index"],regression_equations_final1[,"list_index"],
                                                                 regression_equations_final1[,"dep_index"],regression_equations_final1[,"read_index"],
                                                                 regression_equations_final1[,"sim_index"],regression_equations_final1[,"model_index"]),]
row.names(regression_equations_final1) <- seq(nrow(regression_equations_final1))

rm2(regression_equations_expand1,regression_equations_expand_adj_read1,regression_equations_expand_adj_sim1)

# regression_correlation1 <- regression_correlation(x=regression_equations_final1,data=data_all,decimals=corr_decimals,
#                                                   read_abrev <- c('ios'), sim_abrev <- c('NOSIM'),
#                                                   additional_vars=c("pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#                                                                     "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#                                                                     "exret_lag1","exret_lag2","exret_lag3","exret_lag4"))
# 
# 
# rm2(regression_correlation1)


#regression_execute_logit_probit(equations_df=regression_equations_final1,data_all=data_all,date_index_var="date_index",
#                          id=identifier,output_dir=output_directory_reg_readability)

output_directory_reg_readability_logit <- paste(output_directory,"reg_readability_logit","\\",sep="")
create_directory(output_directory_reg_readability_logit,remove=1)

regression_execute_logit_probit(equations_df=regression_equations_final1,data_all=descrip_stats_data_aggregate,date_index_var="date_index",
                                id=identifier,output_dir=output_directory_reg_readability_logit,family="logit")

output_directory_reg_readability_probit <- paste(output_directory,"reg_readability_probit","\\",sep="")
create_directory(output_directory_reg_readability_probit,remove=1)

regression_execute_logit_probit(equations_df=regression_equations_final1,data_all=descrip_stats_data_aggregate,date_index_var="date_index",
                                id=identifier,output_dir=output_directory_reg_readability_probit,family="probit")

rm2(output_directory_reg_readability_logit,output_directory_reg_readability_probit,regression_equations_final1)


###############################################################################
cat("PANEL REGRESSION - SIMILARITY", "\n")
###############################################################################

#sim_type2 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type2 <- c("050pct","750pct","900pct")

#Regression equations
regression_equations2_1 <- list(time_frame=list(c(9999),c(9999)),
                                dep_var= c("kink_percent_90","indexrsq_percent_90","ar_1_percent_90","num_zero_percent_90","uniform_percent_90","string_percent_90","num_pairs_percent_90",
                                           "per_negative_percent_90","qscore_90_Median1","qscore_90_Median2","qscore_90_Quartile1","qscore_90_Quartile4"),
                                models=c("avg_all_similarity_YYYpct_XXX ",
                                         "avg_Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
                                         "avg_all_similarity_YYYpct_XXX                         + avg_return + volatilty + size + max_age_y ",
                                         "avg_Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX    + avg_return + volatilty + size + max_age_y ",
                                         "avg_all_similarity_YYYpct_XXX                         + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
                                         "avg_Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX    + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin "),
                                model_type=c("pooling"),
                                note=c("similarity"))

#regression_equations2 <- list(regression_equations2_1,regression_equations2_2)
#rm(regression_equations2_1,regression_equations2_2)

regression_equations2 <- list(regression_equations2_1)
rm(regression_equations2_1)

regression_equations_expand2 <- regression_expand(regression_equations2)

regression_equations_expand_adj_read2 <- regression_read_adjustment(equations_df=regression_equations_expand2,equations_col="indep_var",
                                                                    name_col="outname_short", read_prefix="XXX", read_list="ios",
                                                                    index_list=c("date_index","list_index","dep_index","model_index"))

regression_equations_expand_adj_sim2 <- regression_sim_adjustment(equations_df=regression_equations_expand_adj_read2,equations_col="indep_var",
                                                                  name_col="outname_short", sim_prefix="YYYpct", sim_list=sim_type2,
                                                                  #name_col="outname_short", sim_prefix="YYYpct", sim_list="NOSIM",
                                                                  index_list=c("date_index","list_index","dep_index","model_index","read_index"))

regression_equations_final2 <- regression_equations_expand_adj_sim2
regression_equations_final2 <- regression_equations_final2[order(regression_equations_final2[,"date_index"],regression_equations_final2[,"list_index"],
                                                                 regression_equations_final2[,"dep_index"],regression_equations_final2[,"read_index"],
                                                                 regression_equations_final2[,"sim_index"],regression_equations_final2[,"model_index"]),]
row.names(regression_equations_final2) <- seq(nrow(regression_equations_final2))

rm2(regression_equations_expand2,regression_equations_expand_adj_read2,regression_equations_expand_adj_sim2)

# 
# regression_correlation2 <- regression_correlation(x=regression_equations_final2,data=data_all,decimals=corr_decimals,
#                                                   read_abrev <- c('ios'), sim_abrev <- sim_type2,
#                                                   additional_vars=c("pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#                                                                     "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#                                                                     "exret_lag1","exret_lag2","exret_lag3","exret_lag4"))
# 
# rm2(regression_correlation2,sim_type2)

output_directory_reg_similarity_logit <- paste(output_directory,"reg_similarity_logit","\\",sep="")
create_directory(output_directory_reg_similarity_logit,remove=1)

regression_execute_logit_probit(equations_df=regression_equations_final2,data_all=descrip_stats_data_aggregate,date_index_var="date_index",
                                id=identifier,output_dir=output_directory_reg_similarity_logit,family="logit")

output_directory_reg_similarity_probit <- paste(output_directory,"reg_similarity_probit","\\",sep="")
create_directory(output_directory_reg_similarity_probit,remove=1)

regression_execute_logit_probit(equations_df=regression_equations_final2,data_all=descrip_stats_data_aggregate,date_index_var="date_index",
                                id=identifier,output_dir=output_directory_reg_similarity_probit,family="probit")

rm2(output_directory_reg_similarity_logit,output_directory_reg_similarity_probit,regression_equations_final2)


###############################################################################
cat("PANEL REGRESSION - TONE", "\n")
###############################################################################

#Regression equations
regression_equations3_1 <- list(#time_frame=list(c(beg_year,2000,2000,2006),
  #                c(end_year,2011,2005,2011)),
  time_frame=list(c(9999),c(9999)),
  dep_var= c("kink_percent_90","indexrsq_percent_90","ar_1_percent_90","num_zero_percent_90","uniform_percent_90","string_percent_90","num_pairs_percent_90",
             "per_negative_percent_90","qscore_90_Median1","qscore_90_Median2","qscore_90_Quartile1","qscore_90_Quartile4"),
  models=c("per_litigious    ",
           "per_modalstrong  ",
           "per_modalweak    ",
           "per_negative     ",
           "per_positive     ",
           "per_uncertainty  ",
           "per_litigious    + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "per_modalstrong  + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "per_modalweak    + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "per_negative     + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "per_positive     + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin ",
           "per_uncertainty  + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin "),
  model_type=c("pooling"),
  note=c("tone"))

# regression_equations3_2 <- list(time_frame=list(c(end_year),c(end_year)),
#                                 #time_frame=list(c(beg_year),c(end_year)),
#                                 dep_var=c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios"),
#                                 models=c("quality_score_trim2_90",
#                                          pattern_str,
#                                          quality_str,
#                                          nonquality_str,
#                                          "quality_score_trim2_90 + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
#                                          paste(pattern_str, "    + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",sep=""),
#                                          paste(pattern_str, "    + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",sep=""),
#                                          paste(nonquality_str, " + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",sep="")),
#                                 model_type=c("pooling"),
#                                 note=c("tone"))

#regression_equations3 <- list(regression_equations3_1,regression_equations3_2)
#rm(regression_equations3_1,regression_equations3_2)

regression_equations3 <- list(regression_equations3_1)
rm(regression_equations3_1)

regression_equations_expand3 <- regression_expand(regression_equations3)

regression_equations_expand_adj_read3 <- regression_read_adjustment(equations_df=regression_equations_expand3,equations_col="indep_var",
                                                                    name_col="outname_short", read_prefix="XXX", read_list="ios",
                                                                    index_list=c("date_index","list_index","dep_index","model_index"))

regression_equations_expand_adj_sim3 <- regression_sim_adjustment(equations_df=regression_equations_expand_adj_read3,equations_col="indep_var",
                                                                  name_col="outname_short", sim_prefix="YYYpct", sim_list=c('NOSIM'),
                                                                  index_list=c("date_index","list_index","dep_index","model_index","read_index"))

regression_equations_final3 <- regression_equations_expand_adj_sim3
regression_equations_final3 <- regression_equations_final3[order(regression_equations_final3[,"date_index"],regression_equations_final3[,"list_index"],
                                                                 regression_equations_final3[,"dep_index"],regression_equations_final3[,"read_index"],
                                                                 regression_equations_final3[,"sim_index"],regression_equations_final3[,"model_index"]),]
row.names(regression_equations_final3) <- seq(nrow(regression_equations_final3))

rm2(regression_equations_expand3,regression_equations_expand_adj_read3,regression_equations_expand_adj_sim3)

# regression_correlation3 <- regression_correlation(x=regression_equations_final3,data=data_all,decimals=corr_decimals,
#                                                   read_abrev <- c('ios'), sim_abrev <- c('NOSIM'),
#                                                   additional_vars=c("pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#                                                                     "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#                                                                     "exret_lag1","exret_lag2","exret_lag3","exret_lag4"))
# 
# 
# rm2(regression_correlation3)


#regression_execute_logit_probit(equations_df=regression_equations_final3,data_all=data_all,date_index_var="date_index",
#                          id=identifier,output_dir=output_directory_reg_readability)

output_directory_reg_tone_logit <- paste(output_directory,"reg_tone_logit","\\",sep="")
create_directory(output_directory_reg_tone_logit,remove=1)

regression_execute_logit_probit(equations_df=regression_equations_final3,data_all=descrip_stats_data_aggregate,date_index_var="date_index",
                                id=identifier,output_dir=output_directory_reg_tone_logit,family="logit")

output_directory_reg_tone_probit <- paste(output_directory,"reg_tone_probit","\\",sep="")
create_directory(output_directory_reg_tone_probit,remove=1)

regression_execute_logit_probit(equations_df=regression_equations_final3,data_all=descrip_stats_data_aggregate,date_index_var="date_index",
                                id=identifier,output_dir=output_directory_reg_tone_probit,family="probit")

rm2(output_directory_reg_tone_logit,output_directory_reg_tone_probit,regression_equations_final3)


###############################################################################
cat("PANEL REGRESSION - READABILITY & SIMILARITY", "\n")
###############################################################################

#sim_type4 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type4 <- c("050pct","750pct","900pct")

#Regression equations
regression_equations4_1 <- list(time_frame=list(c(9999),c(9999)),
                                dep_var= c("kink_percent_90","indexrsq_percent_90","ar_1_percent_90","num_zero_percent_90","uniform_percent_90","string_percent_90","num_pairs_percent_90",
                                           "per_negative_percent_90","qscore_90_Median1","qscore_90_Median2","qscore_90_Quartile1","qscore_90_Quartile4"),
                                models=c("coleman_liau_XXX + avg_all_similarity_YYYpct_XXX",
                                         "fog_XXX          + avg_all_similarity_YYYpct_XXX",
                                         "coleman_liau_XXX + avg_Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
                                         "fog_XXX          + avg_Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
                                         "coleman_liau_XXX + avg_all_similarity_YYYpct_XXX                      + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
                                         "fog_XXX          + avg_all_similarity_YYYpct_XXX                      + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
                                         "coleman_liau_XXX + avg_Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
                                         "fog_XXX          + avg_Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX + avg_return + volatilty + size + max_age_y + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin"),
                                model_type=c("pooling"),
                                note=c("readbility_similarity"))


#regression_equations4 <- list(regression_equations4_1,regression_equations4_2)
#rm(regression_equations4_1,regression_equations4_2)

regression_equations4 <- list(regression_equations4_1)
rm(regression_equations4_1)

regression_equations_expand4 <- regression_expand(regression_equations4)

regression_equations_expand_adj_read4 <- regression_read_adjustment(equations_df=regression_equations_expand4,equations_col="indep_var",
                                                                    name_col="outname_short", read_prefix="XXX", read_list="ios",
                                                                    index_list=c("date_index","list_index","dep_index","model_index"))

regression_equations_expand_adj_sim4 <- regression_sim_adjustment(equations_df=regression_equations_expand_adj_read4,equations_col="indep_var",
                                                                  name_col="outname_short", sim_prefix="YYYpct", sim_list=sim_type3,
                                                                  index_list=c("date_index","list_index","dep_index","model_index","read_index"))

regression_equations_final4 <- regression_equations_expand_adj_sim4
regression_equations_final4 <- regression_equations_final4[order(regression_equations_final4[,"date_index"],regression_equations_final4[,"list_index"],
                                                                 regression_equations_final4[,"dep_index"],regression_equations_final4[,"read_index"],
                                                                 regression_equations_final4[,"sim_index"],regression_equations_final4[,"model_index"]),]
row.names(regression_equations_final4) <- seq(nrow(regression_equations_final4))

rm2(regression_equations_expand4,regression_equations_expand_adj_read4,regression_equations_expand_adj_sim4)


# regression_correlation4 <- regression_correlation(x=regression_equations_final4,data=data_all,decimals=corr_decimals,
#                                                   read_abrev <- c('ios'), sim_abrev <- sim_type4,
#                                                   additional_vars=c("pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#                                                                     "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#                                                                     "exret_lag1","exret_lag2","exret_lag3","exret_lag4"))
# 
# rm2(regression_correlation4,sim_type4)

output_directory_reg_readability_similarity_logit <- paste(output_directory,"reg_readability_similarity_logit","\\",sep="")
create_directory(output_directory_reg_readability_similarity_logit,remove=1)

regression_execute_logit_probit(equations_df=regression_equations_final4,data_all=descrip_stats_data_aggregate,date_index_var="date_index",
                                id=identifier,output_dir=output_directory_reg_readability_similarity_logit,family="logit")

output_directory_reg_readability_similarity_probit <- paste(output_directory,"reg_readability_similarity_probit","\\",sep="")
create_directory(output_directory_reg_readability_similarity_probit,remove=1)

regression_execute_logit_probit(equations_df=regression_equations_final4,data_all=descrip_stats_data_aggregate,date_index_var="date_index",
                                id=identifier,output_dir=output_directory_reg_readability_similarity_probit,family="probit")

rm2(output_directory_reg_readability_similarity_logit,output_directory_reg_readability_similarity_probit,regression_equations_final4)


# ###############################################################################
# cat("PANEL REGRESSION - READABILITY & SIMILARITY QUARTILES", "\n")
# ###############################################################################
# 
# output_directory_reg_readability_similarity_quartiles <- paste(output_directory,"reg_readability_similarity_quartiles","\\",sep="")
# create_directory(output_directory_reg_readability_similarity_quartiles,remove=1)
# 
# data_year_groups4 <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
#                                 stringsAsFactors=FALSE)
# 
# data_year_groups4[1,] <- c(beg_year,end_year)
# #data_year_groups4[2,] <- c(2000,2011)
# #data_year_groups4[3,] <- c(2006,2011)
# #data_year_groups4[4,] <- c(1994,1999)
# #data_year_groups4[5,] <- c(2000,2005)
# 
# 
# #dep_var4 <- c("pflow","nflow")
# #dep_var4 <- c("pflow")
# #dep_var4 <- c("pflow","mktadjret")
# dep_var4 <- c("pflow","mktadjret","exret",
#               "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
#               "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24")
# 
# model_type4 <- "pooling"
# 
# note4 <- "readbility_similarity_year"
# 
# #sim_type4 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
# sim_type4 <- c("050pct","900pct")
# 
# 
# #Regression equations
# regression_equations4 <- data.frame(grade=NA,
#                                     similarity=NA,
#                                     controls=NA,
#                                     quantile=NA,
#                                     fixed_effects=NA,
#                                     full_independent_vars=NA,
#                                     stringsAsFactors=FALSE)
# regression_equations4[1,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
#                                NA,NA,NA,NA)
# regression_equations4[2,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                NA,NA,NA,NA)
# regression_equations4[3,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin",
#                                NA,NA,NA)
# regression_equations4[4,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin",
#                                NA,NA,NA)
# regression_equations4[5,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
#                                NA,NA,NA)
# regression_equations4[6,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
#                                NA,NA,NA)
# regression_equations4[7,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
#                                NA,
#                                NA,#"factor(yr)",
#                                NA)
# regression_equations4[8,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
#                                NA,
#                                NA,#"factor(yr)",
#                                NA)
# regression_equations4[9,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + AUM_log_lag1 + Management_Fee_bin + Other_Fee_bin + Listed_on_Exchange_bin + Domicile_onshore_bin + Flagship_bin + Dead_bin",
#                                NA,
#                                NA,#"factor(yr)",
#                                NA)
# 
# regression_equations4 <- unknown_to_NA(regression_equations4,unknowns_strings)
# 
# #Create Independent Variable Equation
# for (i in 1:nrow(regression_equations4))
# {
#   
#   temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)], use.names=FALSE))))
#   regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
#   
# }
# 
# for (k in 1:nrow(data_year_groups4))
# {
#   #k <- 1
#   
#   cat("START YEAR:", data_year_groups4[k,1], "END YEAR:", data_year_groups4[k,2],"\n")
#   
#   data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
#   data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
#   
#   for (i in 1:length(dep_var4))  
#     #for (i in 4:length(dep_var4))
#   {
#     #i <- 1
#     
#     for (j in 1:length(sim_type4))
#     {
#       #j <- 1
#       #j <- 2
#       
#       out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sim_type4[j],sep="_")
#       
#       #models <- rep( list(list()), nrow(regression_equations4) )
#       se <- rep( list(list()), nrow(regression_equations4) )
#       pval <- rep( list(list()), nrow(regression_equations4) )
#       
#       for (l in 1:nrow(regression_equations4))
#       {
#         #l <- 1
#         
#         ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
#         ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
#         ind_vars_reg0 <- gsub("YYYpct",sim_type4[j],ind_vars_reg0,ignore.case = TRUE)
#         reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
#         #reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")), data_temp)
#         #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
#         #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
#         #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
#         reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
#         #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
#         #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#         #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#         
#         #models[[l]] <- reg0
#         se[[l]] <- reg0_rse[,4]
#         pval[[l]] <- reg0_rse[,4]
#         
#         assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
#         #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
#         
#         rm2(ind_vars_reg0,reg0,reg0_rse)
#         
#       }
#       
#       htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))),
#               model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),
#               override.se=se,
#               override.pval=pval,
#               stars=c(0.01, 0.05, 0.1), digits=3, 
#               caption="Effect of Readability & Similarity on Hedge Fund Flows – Multivariate",
#               file=paste(output_directory_reg_readability_similarity_quartiles,out_file_name,".doc",sep=""))
#       
#       #custom.names
#       
#       progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var4), 
#                         inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type4))
#       
#       rm2(se,pval,out_file_name,l)
#       eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
#       
#     }
#     rm2(j)
#     
#   } 
#   
#   rm2(data_temp,data_temp.pd,i)
#   
# }
# 
# rm2(data_year_groups4,dep_var4,model_type4,note4,sim_type4,temp_char_vec,regression_equations4,k)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###############################################################################
# cat("YOUNG FUNDS", "\n")
# ###############################################################################
# 
# data_year_groups4 <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
#                                 stringsAsFactors=FALSE)
# 
# data_year_groups4[1,] <- c(beg_year,end_year)
# data_year_groups4[2,] <- c(2000,2011)
# data_year_groups4[3,] <- c(1994,1999)
# data_year_groups4[4,] <- c(2000,2005)
# data_year_groups4[5,] <- c(2006,2011)
# 
# #dep_var4 <- c("pflow","nflow")
# dep_var4 <- c("pflow")
# 
# model_type4 <- "pooling"
# 
# note4 <- "young_readbility_similarity"
# 
# sim_type4 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
# #sim_type4 <- c("050pct","900pct")
# 
# 
# #Regression equations
# regression_equations4 <- data.frame(grade=NA,
#                                     similarity=NA,
#                                     controls=NA,
#                                     quantile=NA,
#                                     fixed_effects=NA,
#                                     full_independent_vars=NA,
#                                     stringsAsFactors=FALSE)
# regression_equations4[1,] <- c("avg_grade_level_XXX",
#                                "all_similarity_YYYpct_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
#                                NA,NA,NA,NA)
# regression_equations4[2,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                "all_similarity_YYYpct_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
#                                NA,NA,NA,NA)
# regression_equations4[3,] <- c("avg_grade_level_XXX",
#                                "all_similarity_YYYpct_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations4[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                "all_similarity_YYYpct_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations4[5,] <- c(NA,NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + total_fee",
#                                "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                NA,NA)
# regression_equations4[6,] <- c(NA,NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + total_fee",
#                                "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                NA,NA)
# regression_equations4[7,] <- c(NA,NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + total_fee",
#                                "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3 + all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                NA,NA)
# regression_equations4[8,] <- c(NA,NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + total_fee",
#                                "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3 + all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                "factor(yr)",
#                                NA)
# regression_equations4 <- unknown_to_NA(regression_equations4,unknowns_strings)
# 
# #Create Independent Variable Equation
# for (i in 1:nrow(regression_equations4))
# {
#   
#   temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)], use.names=FALSE))))
#   regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
#   
# }
# 
# for (k in 1:nrow(data_year_groups4))
# {
#   #k <- 1
#   
#   cat("START YEAR:", data_year_groups4[k,1], "END YEAR:", data_year_groups4[k,2],"\n")
#   
#   data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
#   data_temp <- data_temp[data_temp[,"age_m"]<=36,]
#   data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
#   
#   for (i in 1:length(dep_var4))
#   {
#     #i <- 1
#     
#     for (j in 1:length(sim_type4))
#     {
#       #j <- 1
#       #j <- 2
#       
#       out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sim_type4[j],sep="_")
#       
#       #models <- rep( list(list()), nrow(regression_equations4) )
#       se <- rep( list(list()), nrow(regression_equations4) )
#       pval <- rep( list(list()), nrow(regression_equations4) )
#       
#       for (l in 1:nrow(regression_equations4))
#       {
#         #l <- 1
#         
#         ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
#         ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
#         ind_vars_reg0 <- gsub("YYYpct",sim_type4[j],ind_vars_reg0,ignore.case = TRUE)
#         reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
#         #reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")), data_temp)
#         #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
#         #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
#         #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
#         reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
#         #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
#         #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#         #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#         
#         #models[[l]] <- reg0
#         se[[l]] <- reg0_rse[,4]
#         pval[[l]] <- reg0_rse[,4]
#         
#         assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
#         #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
#         
#         rm2(ind_vars_reg0,reg0,reg0_rse)
#         
#       }
#       
#       htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))),
#               model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),
#               override.se=se,
#               override.pval=pval,
#               stars=c(0.01, 0.05, 0.1), digits=3, 
#               caption="Effect of Readability & Similarity on Young Hedge Fund Flows – Multivariate",
#               file=paste(output_directory,out_file_name,".doc",sep=""))
#       
#       #custom.names
#       
#       progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var4), 
#                         inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type4))
#       
#       rm2(se,pval,out_file_name,l)
#       eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
#       
#     }
#     rm2(j)
#     
#   } 
#   
#   rm2(data_temp,data_temp.pd,i)
#   
# }
# 
# rm2(data_year_groups4,dep_var4,model_type4,note4,sim_type4,temp_char_vec,regression_equations4,k)
# 
# 
# ###############################################################################
# cat("FLOW VOLATILITY DATA", "\n")
# ###############################################################################
# 
# data_vol_other0 <- data_all[,c(identifier,"yr","sdpct_flow",
#                                "age_y","total_fee","performance_fee","Management_Fee_bin","Other_Fee_bin",
#                                "ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
#                                "avg_grade_level_ios","avg_grade_level_acf_ios","avg_grade_level_ac_ios",
#                                "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
#                                "Primary_Investment_Strategy_combcol_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
#                                "Primary_Investment_Strategy_combcol_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios",
#                                "ARI_ios_below_quartile1","ARI_ios_above_quartile3",
#                                "Coleman_Liau_ios_below_quartile1","Coleman_Liau_ios_above_quartile3",
#                                "Flesch_Kincaid_ios_below_quartile1","Flesch_Kincaid_ios_above_quartile3",
#                                "FOG_ios_below_quartile1","FOG_ios_above_quartile3",
#                                "SMOG_ios_below_quartile1","SMOG_ios_above_quartile3",
#                                "avg_grade_level_ios_below_quartile1","avg_grade_level_ios_above_quartile3",
#                                "avg_grade_level_acf_ios_below_quartile1","avg_grade_level_acf_ios_above_quartile3",
#                                "avg_grade_level_ac_ios_below_quartile1","avg_grade_level_ac_ios_above_quartile3",
#                                "all_similarity_050pct_ios_below_quartile1","all_similarity_100pct_ios_below_quartile1","all_similarity_250pct_ios_below_quartile1",
#                                "all_similarity_500pct_ios_below_quartile1","all_similarity_750pct_ios_below_quartile1","all_similarity_900pct_ios_below_quartile1",
#                                "all_similarity_050pct_ios_above_quartile3","all_similarity_100pct_ios_above_quartile3","all_similarity_250pct_ios_above_quartile3",
#                                "all_similarity_500pct_ios_above_quartile3","all_similarity_750pct_ios_above_quartile3","all_similarity_900pct_ios_above_quartile3",
#                                "Primary_Investment_Strategy_combcol_similarity_050pct_ios_below_quartile1","Primary_Investment_Strategy_combcol_similarity_100pct_ios_below_quartile1",
#                                "Primary_Investment_Strategy_combcol_similarity_250pct_ios_below_quartile1","Primary_Investment_Strategy_combcol_similarity_500pct_ios_below_quartile1",
#                                "Primary_Investment_Strategy_combcol_similarity_750pct_ios_below_quartile1","Primary_Investment_Strategy_combcol_similarity_900pct_ios_below_quartile1",
#                                "Primary_Investment_Strategy_combcol_similarity_050pct_ios_above_quartile3","Primary_Investment_Strategy_combcol_similarity_100pct_ios_above_quartile3",
#                                "Primary_Investment_Strategy_combcol_similarity_250pct_ios_above_quartile3","Primary_Investment_Strategy_combcol_similarity_500pct_ios_above_quartile3",
#                                "Primary_Investment_Strategy_combcol_similarity_750pct_ios_above_quartile3","Primary_Investment_Strategy_combcol_similarity_900pct_ios_above_quartile3")]
# 
# data_vol_other_dt0 <- data.table(data_vol_other0, key = "fund_id,yr")
# #data_vol_other_dt <- data_vol_other_dt0[, tail(.SD, 1), by = key(data_vol_other_dt0)]
# data_vol_other_dt <- data_vol_other_dt0[unique(data_vol_other_dt0[,key(data_vol_other_dt0), with = FALSE]), mult = 'last']
# data_vol_other <- data.frame(data_vol_other_dt,stringsAsFactors=FALSE)
# data_vol_other <- unique(data_vol_other)
# 
# rm2(data_vol_other0,data_vol_other_dt0,data_vol_other_dt)
# 
# data_vol_dt <- data.table(data_all[c(identifier,"yr","aum","monthly_ret")])
# setkeyv(data_vol_dt,c(identifier,"yr"))
# 
# ret_annualized1 <- data_vol_dt[, list(aret1=monthly_ret+1),by="fund_id,yr"]
# ret_annualized2 <- ret_annualized1[, list(aret2=prod(aret1, na.rm=FALSE)),by="fund_id,yr"]
# ret_annualized3 <- ret_annualized2[, list(aret=aret2-1),by="fund_id,yr"]
# ret_annualized3 <- as.data.frame(ret_annualized3,stringsAsFactors=FALSE)
# ret_annualized <- data.frame(ret_annualized3,
#                              aret_lag1=NA,
#                              stringsAsFactors=FALSE)
# ret_annualized[,"aret_lag1"] <- create_lags2(ret_annualized,"aret",identifier,1)
# 
# rm2(ret_annualized1,ret_annualized2,ret_annualized3)
# 
# data_averaged0 <- data_vol_dt[, list(a_aum=mean(aum,na.rm=TRUE)),by="fund_id,yr"]
# data_averaged0 <- as.data.frame(data_averaged0,stringsAsFactors=FALSE)
# data_averaged <- data.frame(data_averaged0,
#                             a_aum_log=suppressWarnings(log(data_averaged0[,"a_aum"])),
#                             stringsAsFactors=FALSE)
# 
# rm2(data_averaged0)
# 
# data_vol0 <- merge(data_averaged, ret_annualized, 
#                    by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
#                    all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
# 
# rm2(data_averaged,ret_annualized)
# 
# data_vol <- merge(data_vol0, data_vol_other, 
#                   by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
#                   all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
# 
# data_vol <- unknown_to_NA(data_vol,unknowns_strings)
# 
# rm2(data_vol0,data_vol_other,data_vol_dt)
# 
# 
# ###############################################################################
# cat("FLOW VOLATILITY REGRESSION", "\n")
# ###############################################################################
# 
# 
# data_year_groups5 <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
#                                 stringsAsFactors=FALSE)
# 
# data_year_groups5[1,] <- c(beg_year,end_year)
# data_year_groups5[2,] <- c(2000,2011)
# data_year_groups5[3,] <- c(1994,1999)
# data_year_groups5[4,] <- c(2000,2005)
# data_year_groups5[5,] <- c(2006,2011)
# 
# 
# dep_var5 <- c("sdpct_flow")
# 
# model_type5 <- "pooling"
# 
# note5 <- "vol_readability_similarity"
# 
# sim_type5 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
# 
# #Regression equations
# regression_equations5 <- data.frame(grade=NA,
#                                     similarity=NA,
#                                     controls=NA,
#                                     quantile=NA,
#                                     fixed_effects=NA,
#                                     full_independent_vars=NA,
#                                     stringsAsFactors=FALSE)
# regression_equations5[1,] <- c("avg_grade_level_XXX",
#                                "all_similarity_YYYpct_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
#                                NA,NA,NA,NA)
# regression_equations5[2,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                "all_similarity_YYYpct_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
#                                NA,NA,NA,NA)
# regression_equations5[3,] <- c("avg_grade_level_XXX",
#                                "all_similarity_YYYpct_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
#                                "aret_lag1 + a_aum_log + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations5[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                "all_similarity_YYYpct_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",
#                                "aret_lag1 + a_aum_log + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations5[5,] <- c(NA,NA,
#                                "aret_lag1 + a_aum_log + age_y + total_fee",
#                                "ARI_ios_below_quartile1 + ARI_ios_above_quartile3 + Coleman_Liau_ios_below_quartile1 + Coleman_Liau_ios_above_quartile3 + Flesch_Kincaid_ios_below_quartile1 + Flesch_Kincaid_ios_above_quartile3 + FOG_ios_below_quartile1 + FOG_ios_above_quartile3 + SMOG_ios_below_quartile1 + SMOG_ios_above_quartile3",
#                                NA,NA)
# regression_equations5[6,] <- c(NA,NA,
#                                "aret_lag1 + a_aum_log + age_y + total_fee",
#                                "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
#                                NA,NA)
# regression_equations5[7,] <- c(NA,NA,
#                                "aret_lag1 + a_aum_log + age_y + total_fee",
#                                "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                NA,NA)
# regression_equations5[7,] <- c(NA,NA,
#                                "aret_lag1 + a_aum_log + age_y + total_fee",
#                                "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_below_quartile1 + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX_above_quartile3",
#                                "factor(yr)",
#                                NA)
# regression_equations5 <- unknown_to_NA(regression_equations5,unknowns_strings)
# 
# #Create Independent Variable Equation
# for (i in 1:nrow(regression_equations5))
# {
#   
#   temp_char_vec <- c(na.omit(as.character(unlist(regression_equations5[i,1:(ncol(regression_equations5)-1)], use.names=FALSE))))
#   regression_equations5[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
#   
# }
# 
# 
# for (k in 1:nrow(data_year_groups5))
# {
#   #k <- 1
#   
#   cat("START YEAR:", data_year_groups5[k,1], "END YEAR:", data_year_groups5[k,2],"\n")
#   
#   data_temp <- data_vol[(data_vol[,"yr"]>=data_year_groups5[k,1] & data_vol[,"yr"]<=data_year_groups5[k,2]),]
#   data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr"), drop.index=TRUE, row.names=TRUE)
#   
#   for (i in 1:length(dep_var5))
#   {
#     #i <- 1
#     
#     for (j in 1:length(sim_type5))
#     {
#       #j <- 1
#       #j <- 2
#       
#       out_file_name <- paste("reg_compare_plm",dep_var5[i],data_year_groups5[k,1],data_year_groups5[k,2],note5,sim_type5[j],sep="_")
#       
#       #models <- rep( list(list()), nrow(regression_equations5) )
#       se <- rep( list(list()), nrow(regression_equations5) )
#       pval <- rep( list(list()), nrow(regression_equations5) )
#       
#       for (l in 1:nrow(regression_equations5))
#       {
#         #l <- 1
#         
#         ind_vars_reg0 <- regression_equations5[l,"full_independent_vars"]
#         ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
#         ind_vars_reg0 <- gsub("YYYpct",sim_type5[j],ind_vars_reg0,ignore.case = TRUE)
#         reg0 <- plm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type5)
#         #reg0 <- lm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")), data_temp)
#         #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
#         #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
#         #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
#         reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"yr"])
#         #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"yr"])
#         #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#         #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#         
#         #models[[l]] <- reg0
#         se[[l]] <- reg0_rse[,4]
#         pval[[l]] <- reg0_rse[,4]
#         
#         assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
#         #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
#         
#         rm2(ind_vars_reg0,reg0,reg0_rse)
#         
#       }
#       
#       htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations5)),sep="",collapse=","),")",sep=""))), 
#               model.names=paste("(",seq(1,nrow(regression_equations5)),")",sep=""),
#               override.se=se,
#               override.pval=pval,
#               stars=c(0.01, 0.05, 0.1), digits=3, 
#               caption="Effect of Readability Similarity on Hedge Fund Flow Volatility – Multivariate",
#               file=paste(output_directory,out_file_name,".doc",sep=""))
#       
#       progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var5), 
#                         inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type5))
#       
#       rm2(se,pval,out_file_name,l)
#       
#     }
#     rm2(j)
#     
#   } 
#   
#   rm2(data_temp,data_temp.pd,i)
#   
# }
# 
# rm2(data_year_groups5,dep_var5,model_type5,note5,sim_type5,temp_char_vec,regression_equations5,k)
# 
# 
# 
# 
# 
# 
# 
# ###############################################################################
# cat("PRINCIPAL COMPONENT - READABILITY", "\n")
# ###############################################################################
# 
# pc_scores_loadings <- function(data_in,variables,suffix,note,tol){
#   
#   #data_in <- data_all
#   #variables <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios")
#   #suffix <- "ios"
#   #note <- "Readability"
#   #tol=NULL
#   #tol=0.3
#   
#   title_full <- ifelse(is.null(tol), paste(note," (Tol = Null)",sep=""), paste(note," (Tol = ",tol,")",sep=""))
#   
#   pc_temp <- prcomp(~ ., data=data_in[,variables], na.action=na.omit, scale=TRUE)
#   
#   #Plotting the variances
#   plot(pc_temp, main = title_full)
#   barplot(pc_temp$sdev/pc_temp$sdev[1], main = title_full)
#   
#   pc_sum <- summary(pc_temp) 
#   
#   #Importance (NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign)
#   pc_importance <- pc_sum$importance
#   pc_importance <- as.matrix(pc_importance)
#   pc_importance <- data.frame(type="Importance",
#                               var=row.names(pc_importance),
#                               pc_importance,
#                               PC_avg=rowMeans(pc_importance),
#                               stringsAsFactors=FALSE)
#   row.names(pc_importance)  <- seq(nrow(pc_importance))
#   
#   #Loadings (NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign)
#   pc_loadings <- pc_sum$rotation
#   pc_loadings <- as.matrix(pc_loadings)
#   pc_loadings <- data.frame(type="Loadings",
#                             var=row.names(pc_loadings),
#                             pc_loadings,
#                             PC_avg=rowMeans(pc_loadings),
#                             stringsAsFactors=FALSE)
#   row.names(pc_loadings)  <- seq(nrow(pc_loadings))
#   
#   #Importance and Loadings
#   pc_importance_loadings <- rbind(pc_importance,pc_loadings)
#   row.names(pc_importance_loadings)  <- seq(nrow(pc_importance_loadings))
#   rm(pc_importance,pc_loadings)
#   
#   #Center
#   pc_center <- pc_sum$center
#   pc_center <- as.matrix(pc_center)
#   pc_center <- data.frame(var=row.names(pc_center),
#                           pc_center,
#                           stringsAsFactors=FALSE)
#   row.names(pc_center)  <- seq(nrow(pc_center))
#   
#   #Scale
#   pc_scale <- pc_sum$scale
#   pc_scale <- as.matrix(pc_scale)
#   pc_scale <- data.frame(var=row.names(pc_scale),
#                          pc_scale,
#                          stringsAsFactors=FALSE)
#   row.names(pc_scale)  <- seq(nrow(pc_scale))
#   
#   #Center and Scale
#   pc_center_scale <- merge(pc_center, pc_scale, 
#                            by.x="var", by.y="var", 
#                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)
#   row.names(pc_center_scale) <- seq(nrow(pc_center_scale))
#   rm(pc_center,pc_scale)
#   
#   #Scores
#   pc_scores <- pc_sum$x
#   pc_scores <- as.matrix(pc_scores)
#   colnames(pc_scores) <- paste(colnames(pc_scores),suffix,sep="_")
#   pc_scores <- data.frame(pc_scores,
#                           stringsAsFactors=FALSE)
#   row.names(pc_scores)  <- seq(nrow(pc_scores))
#   
#   #Combine data
#   comb_list <- list("Importance_and_Loadings" = pc_importance_loadings, "Center_and_Scale" = pc_center_scale, "Scores" = pc_scores)
#   
#   rm(pc_importance_loadings,pc_center_scale,pc_scores)
#   
#   return(comb_list)
#   
# }
# 
# pca_text_read_vars_ios <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios")
# 
# #pc_read_ios <- pc_scores_loadings(data_all,pca_text_read_vars_ios,"ios","Readability", NULL)
# pc_read_ios <- pc_scores_loadings(data_all,pca_text_read_vars_ios,"ios","Readability", 0.2)
# 
# pc_read_ios_loadings <- pc_read_ios$Importance_and_Loadings
# pc_read_ios_center <- pc_read_ios$Center_and_Scale
# pc_read_ios_scores <- pc_read_ios$Scores
# 
# rm2(pca_text_read_vars_ios,pc_read_ios,pc_read_ios_loadings,pc_read_ios_center)
# 
# 
# ###############################################################################
# cat("PRINCIPAL COMPONENT - SIMILARITY", "\n")
# ###############################################################################
# 
# #pca_text_sim_vars_ios <- c("all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
# #                           "Primary_Investment_Strategy_combcol_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
# #                           "Primary_Investment_Strategy_combcol_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")
# pca_text_sim_vars_ios <- c("all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios")
# 
# #pc_sim_ios <- pc_scores_loadings(data_all,pca_text_sim_vars_ios,"ios","Similarity", NULL)
# pc_sim_ios <- pc_scores_loadings(data_all,pca_text_sim_vars_ios,"ios","Similarity", 0.4 )
# 
# pc_sim_ios_loadings <- pc_sim_ios$Importance_and_Loadings
# pc_sim_ios_center <- pc_sim_ios$Center_and_Scale
# pc_sim_ios_scores <- pc_sim_ios$Scores
# 
# rm2(pca_text_sim_vars_ios,pc_sim_ios,pc_sim_ios_loadings,pc_sim_ios_center)
# 
# 
# ###############################################################################
# cat("PRINCIPAL COMPONENT - READABILITY & SIMILARITY", "\n")
# ###############################################################################
# 
# # pca_text_both_vars_ios <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
# #                             "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
# #                             "Primary_Investment_Strategy_combcol_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
# #                             "Primary_Investment_Strategy_combcol_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")
# pca_text_both_vars_ios <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
#                             "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios")
# 
# #pc_both_ios <- pc_scores_loadings(data_all,pca_text_both_vars_ios,"ios","Readability and Similarity", NULL)
# pc_both_ios <- pc_scores_loadings(data_all,pca_text_both_vars_ios,"ios","Readability and Similarity", 0.2)
# 
# pc_both_ios_loadings <- pc_both_ios$Importance_and_Loadings
# pc_both_ios_center <- pc_both_ios$Center_and_Scale
# pc_both_ios_scores <- pc_both_ios$Scores
# 
# rm2(pca_text_both_vars_ios,pc_both_ios,pc_both_ios_loadings,pc_both_ios_center)
# 
# 
# ###############################################################################
# cat("PRINCIPAL COMPONENT DATA - MERGE", "\n")
# ###############################################################################
# 
# data_all_pc_rn <- data.frame(rn=as.integer(row.names(data_all)),
#                              data_all,
#                              stringsAsFactors=FALSE)
# 
# ios_scores_rn <- data.frame(rn=as.integer(row.names(pc_both_ios_scores)),
#                             pc_both_ios_scores,
#                             stringsAsFactors=FALSE)
# 
# data_all_pc <- merge(data_all_pc_rn, ios_scores_rn, 
#                      by.x="rn", by.y="rn", 
#                      all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)
# 
# data_all_pc <- data_all_pc[order(data_all_pc[,"rn"]),]
# data_all_pc <- data_all_pc[,!(colnames(data_all_pc) %in% "rn")]
# data_all_pc <- data_all_pc[order(data_all_pc[,identifier],
#                                  data_all_pc[,"yr"],
#                                  data_all_pc[,"month"]),]
# row.names(data_all_pc) <- seq(nrow(data_all_pc))
# 
# rm2(data_all_pc_rn,ios_scores_rn)
# rm2(pc_read_ios_scores,pc_sim_ios_scores,pc_both_ios_scores)
# 
# 
# ###############################################################################
# cat("PRINCIPAL COMPONENT REGRESSION", "\n")
# ###############################################################################
# 

# data_pc.pd <- pdata.frame(data_all_pc, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
# dep_var_pc <- c("pflow")
# 
# index_vars_pc <- c(identifier, "yr_month")
# controls_pc <- "mktadjret_agglag1 + mktadjret_agglag2 + mktadjret_agglag3 + sddret_agglag1 + age_y + log_mtna_agg"
# #fe_pc <- "factor(yr)"
# fe_pc <- "factor(branding_name) + factor(yr)"
# 
# model_type_pc <- "pooling"
# note <- "pca_sep_sim_good"
# 
# 
# for (i in 1:length(dep_var_pc))
# {
#   #i <- 1
#   
#   out_file_name <- paste("reg_compare_plm",dep_var_pc[i],deparse(substitute(data_all_pc)),note,sep="_")
#   
#   if (dep_var_pc[i]=="pflow")
#   {
#     #vars_ext <- "pflow_lag1 + pflow_lag2 + pflow_lag3"
#     vars_ext <- ""
#     
#   } else if (dep_var_pc[i]=="nflow")
#   {
#     #vars_ext <- "nflow_lag1 + nflow_lag2 + nflow_lag3"
#     vars_ext <- ""
#     
#   } else
#   {
#     cat("ERROR", "\n")
#   }
#   
#   
#   ind_vars_reg0_pc <- "PC1_ios + PC2_ios + PC3_ios + PC4_ios"
#   reg0_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg0_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   #reg0_rse_pc <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
#   reg0_rse_pc <- mcl.plm(data_all_pc,reg0_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg0_pc),digits=3,model.names=c("(1)"),override.se=list(reg0_rse_pc[,2]),override.pval=list(reg0_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   ind_vars_reg2_pc <- "PC1_pr + PC2_pr + PC3_pr + PC4_pr"
#   reg2_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg2_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   #reg2_rse_pc <- coeftest(reg2, vcov=function(x) vcovDC(x, type="HC1"))
#   reg2_rse_pc <- mcl.plm(data_all_pc,reg2_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg2_pc),digits=3,model.names=c("(1)"),override.se=list(reg2_rse_pc[,2]),override.pval=list(reg2_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   ind_vars_reg3_pc <- "PC1_ios + PC2_ios + PC3_ios + PC4_ios + PC1_pr + PC2_pr + PC3_pr + PC4_pr"
#   reg3_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg3_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   #reg3_rse_pc <- coeftest(reg3, vcov=function(x) vcovDC(x, type="HC1"))
#   reg3_rse_pc <- mcl.plm(data_all_pc,reg3_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg3_pc),digits=3,model.names=c("(1)"),override.se=list(reg3_rse_pc[,2]),override.pval=list(reg3_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   ind_vars_reg4_pc <- paste(ind_vars_reg3_pc,controls_pc,sep="+")
#   reg4_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg4_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   reg4_rse_pc <- mcl.plm(data_all_pc,reg4_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg4_pc),digits=3,model.names=c("(1)"),override.se=list(reg4_rse_pc[,2]),override.pval=list(reg4_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   ind_vars_reg5_pc <- paste(ind_vars_reg3_pc,controls_pc,fe_pc,sep="+")
#   #ind_vars_reg5_pc <- paste(ind_vars_reg3_pc,controls_pc,sep="+")
#   #reg5_pc <- plm(as.formula(paste(dep_var_pc[i],ind_vars_reg5_pc,sep="~")), data_pc.pd,model=model_type_pc)
#   #reg5_rse_pc <- mcl.plm(data_all_pc,reg5_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   reg5_pc <- lm(as.formula(paste(dep_var_pc[i],ind_vars_reg5_pc,sep="~")), data_all_pc)
#   reg5_rse_pc <- mcl(data_all_pc,reg5_pc, data_all_pc[,identifier], data_all_pc[,"month"])
#   #screenreg(list(reg5_pc),digits=3,model.names=c("(1)"),override.se=list(reg5_rse_pc[,2]),override.pval=list(reg5_rse_pc[,4]),stars=c(0.01,0.05,0.1))
#   
#   htmlreg(list(reg0_pc,reg2_pc,reg3_pc,reg4_pc,reg5_pc), 
#           caption="The Importance of Clustering Standard Errors", digits=3, 
#           model.names=c("(1)","(2)","(3)","(4)","(5)"),
#           override.se=list(reg0_rse_pc[,2],reg2_rse_pc[,2],reg3_rse_pc[,2],reg4_rse_pc[,2],reg5_rse_pc[,2]),
#           override.pval=list(reg0_rse_pc[,4],reg2_rse_pc[,4],reg3_rse_pc[,4],reg4_rse_pc[,4],reg5_rse_pc[,4]),
#           stars=c(0.01, 0.05, 0.1),
#           file=paste(output_directory,out_file_name,".doc",sep=""))
#   
#   rm2(ind_vars_reg0_pc,ind_vars_reg2_pc,ind_vars_reg3_pc,ind_vars_reg4_pc,ind_vars_reg5_pc,out_file_name)
#   rm2(reg0_pc,reg2_pc,reg3_pc,reg4_pc,reg5_pc,reg0_rse_pc,reg2_rse_pc,reg3_rse_pc,reg4_rse_pc,reg5_rse_pc)
#   
# } 




# 
# data_year_groups4 <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
#                                 stringsAsFactors=FALSE)
# data_year_groups4[1,] <- c(1992,2012)
# data_year_groups4[2,] <- c(2000,2011)
# data_year_groups4[3,] <- c(1992,1998)
# data_year_groups4[4,] <- c(1999,2005)
# data_year_groups4[5,] <- c(2006,2012)
# 
# #dep_var4 <- c("pflow","nflow")
# dep_var4 <- c("pflow")
# 
# model_type4 <- "pooling"
# 
# note4 <- "pc"
# 
# #Regression equations
# regression_equations4 <- data.frame(grade=NA,
#                                     similarity=NA,
#                                     controls=NA,
#                                     quantile=NA,
#                                     fixed_effects=NA,
#                                     full_independent_vars=NA,
#                                     stringsAsFactors=FALSE)
# regression_equations4[1,] <- c("avg_grade_level_XXX",
#                                NA,NA,NA,NA,NA)
# regression_equations4[2,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,NA,NA,NA,NA)
# regression_equations4[3,] <- c("avg_grade_level_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations4[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations4[5,] <- c("avg_grade_level_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + Management_Fee_bin + performance_fee + Other_Fee_bin",
#                                NA,NA,NA)
# regression_equations4[6,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + Management_Fee_bin + performance_fee + Other_Fee_bin",
#                                NA,NA,NA)
# regression_equations4[7,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_sq_lag1 + age_y + Management_Fee_bin + performance_fee + Other_Fee_bin + Flagship_bin + closed_bin + Dead_bin",
#                                NA,NA,NA)
# 
# regression_equations4 <- unknown_to_NA(regression_equations4,unknowns_strings)
# #Create Independent Variable Equation
# for (i in 1:nrow(regression_equations4))
# {
#   
#   temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)], use.names=FALSE))))
#   regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
#   
# }
# 
# for (k in 1:nrow(data_year_groups4))
# {
#   #k <- 1
#   #k <- 2
#   
#   cat("START YEAR:", data_year_groups4[k,1], "END YEAR:", data_year_groups4[k,2],"\n")
#   
#   data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
#   data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
#   
#   for (i in 1:length(dep_var4))
#   {
#     #i <- 1
#     
#     #out_file_name <- paste("reg_compare_plm",dep_var4[i],deparse(substitute(data_all)),note4,sep="_")
#     out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sep="_")
#     
#     #models <- rep( list(list()), nrow(regression_equations4) )
#     se <- rep( list(list()), nrow(regression_equations4) )
#     pval <- rep( list(list()), nrow(regression_equations4) )
#     
#     for (l in 1:nrow(regression_equations4))
#     {
#       #l <- 1
#       
#       ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
#       ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
#       reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
#       #reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")), data_temp)
#       #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
#       #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
#       #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
#       reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
#       #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
#       #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#       #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
#       
#       #models[[l]] <- reg0
#       se[[l]] <- reg0_rse[,4]
#       pval[[l]] <- reg0_rse[,4]
#       
#       assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
#       #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
#       
#       rm2(ind_vars_reg0,reg0,reg0_rse)
#       
#     }
#     
#     htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))), 
#             model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),
#             override.se=se,
#             override.pval=pval,
#             stars=c(0.01, 0.05, 0.1), digits=3, 
#             caption="Effect of Readability on Hedge Fund Flows – Multivariate",
#             file=paste(output_directory,out_file_name,".doc",sep=""))
#     
#     #custom.names
#     
#     progress_function(outer_loop_count=k, outer_loop_start_val=1, outer_loop_end_val=nrow(data_year_groups4), 
#                       inner_loop_count=i, inner_loop_start_val=1, inner_loop_end_val=length(dep_var4))
#     
#     rm2(se,pval,out_file_name,l)
#     eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
#     
#   } 
#   
#   rm2(data_temp,data_temp.pd,i)
#   
# }
# 
# rm2(data_year_groups4,dep_var4,model_type4,note4,temp_char_vec,regression_equations4,k)
# 
# 
