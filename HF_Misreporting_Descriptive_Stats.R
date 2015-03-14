# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Descriptive_Stats.R
# Version: 1.0
# Date: 09.8.2014
# Purpose: Compute descriptive statistics
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP","\n")
###############################################################################

# Clear workspace
rm(list=ls(all=T))
rm(list=ls(all.names=T))

# Limit History to not exceed 500 lines
Sys.setenv(R_HISTSIZE=500)

repo <- c("http://cran.us.r-project.org")
options(repos=structure(repo))
options(install.packages.check.source=F)

# String as factors is F -- used for read.csv
options(StringsAsFactors=F)

# Default maxprint option
options(max.print=500)
# options(max.print=99999)

# Memory limit
#memory.limit(size=8183)

#Remove scientific notation if digits less than 100
options("scipen"=100)

#Uknown Strings
unknowns_strings <- c(" ","\n","",".","n/a","na","NA",NA,"<NA>","null","NULL",NULL,"nan","NaN",NaN,Inf,
                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                      NA_character_,"NA_character_",NA_real_,"NA_real_")

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK,5=CORALSEA FROM LAPTOP) 
Location <- 1


if (Location==1) {
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("F:/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location==2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T) 
  
} else if (Location==3) {
  
  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location==4) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else if (Location==5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/",winslash="\\",mustWork=T)
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES","\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS","\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=F)


###############################################################################
cat("SECTION: LIBRARIES","\n")
###############################################################################

#Load External Packages
# c("compare","cwhmisc","descr","fastmatch","formatR",
#   "gtools","Hmisc","installr","knitr","leaps","lmtest","markdown","memisc","mitools",
#   "pander","pbapply","PerformanceAnalytics","plm","psych","quantreg","R.oo","R2wd",
#   "reporttools","reshape2","rms","sandwich","sqldf","stargazer","stringr",
#   "texreg","taRifx","UsingR","xtable","zoo")
external_packages <- c("data.table","gdata","plyr")
invisible(unlist(sapply(external_packages,load_external_packages,repo_str=repo,simplify=F,USE.NAMES=F)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("IMPORT DATA","\n")
###############################################################################

identifier <- "Fund_ID"

#start_year <- 1994
start_year <- 2007
end_year <- 2013

#strat_col <- "main_investment_strategy"
strat_col <- "Primary_Investment_Strategy_combcol"

data_all0 <- read.csv(file=paste(output_directory,"data_all",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)


# ###############################################################################
# cat("MERGE DATA","\n")
# ###############################################################################
# 
# data_tone <- read.csv(file=paste(output_directory,"data_tone",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)
# 
# data_all1 <- merge(data_all0,data_tone,
#                    by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
#                    all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
# 
# rm2(data_all0,data_tone)
# 
# data_all1 <- data_all1[order(data_all1[,identifier],
#                              data_all1[,"yr"],
#                              data_all1[,"month"]),]
# row.names(data_all1) <- seq(nrow(data_all1))

data_all1 <- data_all0
rm2(data_all0)

###############################################################################
cat("TRIM DATA","\n")
###############################################################################

#data_all1 <- data_all1[(data_all1[,"yr"]>=start_year & data_all1[,"yr"]<=end_year),]
data_all1 <- data_all1[,!(colnames(data_all1) %in% c("Fund_Name","Secondary_Investment_Strategy","Strategy","Strat_ID"))]

###############################################################################
cat("CLEAN DATA","\n")
###############################################################################

data_all1[,"date"] <- as.Date(data_all1[,"date"],format="%Y-%m-%d")
data_all1[,"Date_Added"] <- as.Date(data_all1[,"Date_Added"],format="%Y-%m-%d")
data_all1[,"chgdt"] <- as.Date(data_all1[,"chgdt"],format="%Y-%m-%d")
data_all1[,"Inception_Date"] <- as.Date(data_all1[,"Inception_Date"],format="%Y-%m-%d")

data_all1[,strat_col] <- ifelse(data_all1[,strat_col]=="",NA,data_all1[,strat_col])

for(k in which(sapply(data_all1,class)!="Date"))
{
  #k <- 1
  
  data_all1[[k]] <- unknownToNA(data_all1[[k]],unknown=unknowns_strings,force=T)
  data_all1[[k]] <- ifelse(is.na(data_all1[[k]]),NA,data_all1[[k]])
}
rm2(k)


###############################################################################
cat("WINSORIZE","\n")
###############################################################################

winsorize_vars <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                    "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                    "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                    "all_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios",
                    "all_similarity_250pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
                    "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",
                    "all_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios",
                    "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")


data_all <- data_all1
for (i in 1:length(winsorize_vars))
{
  #i <- 1
  #i <- 2
  #data_all[,winsorize_vars[i]] <- winsorize_both(data_all[,winsorize_vars[i]],q=0.025)
  data_all[,winsorize_vars[i]] <- winsorize_both(data_all[,winsorize_vars[i]],q=0.005)
  
}
rm(i)

rm2(data_all1,winsorize_vars)


###############################################################################
cat("DESCRIPTIVE STATISTICS - VARIABLES","\n")
###############################################################################

### Create Monthly Level Data

descrip_stats_data_monthly0 <- data_all

descrip_stats_fund_vars_remove <- c("month","age_m","chgdt","Monthly_Ret_org",
                                    unlist(lapply("nflow",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("pflow",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("mktadjret",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("mktadjret_sq",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("Monthly_Ret_sq",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("Monthly_Ret2",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("Monthly_Ret2_sq",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("Yearly_Ret2",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("Yearly_Ret2_sq",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                                    unlist(lapply("sdnet_flow",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=1)),
                                    unlist(lapply("sdpct_flow",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=1)))

descrip_stats_data_monthly0 <- descrip_stats_data_monthly0[,!(colnames(descrip_stats_data_monthly0) %in% descrip_stats_fund_vars_remove)]
row.names(descrip_stats_data_monthly0) <- seq(nrow(descrip_stats_data_monthly0))

descrip_stats_ios_vars_remove <- c("month",
                                   "punct_ios","conjunctions_ios","prepositions_ios","normalized_space_ios",
                                   "pronouns_ios","ttr_ios")

descrip_stats_data_monthly0 <- descrip_stats_data_monthly0[,!(colnames(descrip_stats_data_monthly0) %in% descrip_stats_ios_vars_remove)]
row.names(descrip_stats_data_monthly0) <- seq(nrow(descrip_stats_data_monthly0))

descrip_stats_data_monthly <- data.frame(descrip_stats_data_monthly0,year_group_id=NA,stringsAsFactors=F)

descrip_stats_data_monthly[,"year_group_id"] <- ifelse((descrip_stats_data_monthly[,"yr"]>=1994 & descrip_stats_data_monthly[,"yr"]<=1999),1,
                                                       ifelse((descrip_stats_data_monthly[,"yr"]>=2000 & descrip_stats_data_monthly[,"yr"]<=2006),2,
                                                              ifelse((descrip_stats_data_monthly[,"yr"]>=2007 & descrip_stats_data_monthly[,"yr"]<=2013),3,
                                                                     descrip_stats_data_monthly[,"year_group_id"])))   

rm2(descrip_stats_data_monthly0)


### Create Yearly Level Data

monthly_level_cols <- c("date","yr_month",
                        "Dead_Date","Dead_Reason","Dead_bin","Date_Added","Flagship_bin","Closed_bin","Limited_bin",
                        "age_y","nflow","pflow",
                        "mktadjret","fund_ret_mkt_neg","vwretd","vwretd_annualized","vwretx","vwretx_annualized",
                        "mktadjret_sq","Monthly_Ret_sq","Monthly_Ret2_sq","Yearly_Ret2_sq",
                        "Monthly_Ret",unlist(lapply("Monthly_Ret",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                        "AUM",unlist(lapply("AUM",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                        "AUM_log",unlist(lapply("AUM_log",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)))

descrip_stats_data_yearly0 <-  descrip_stats_data_monthly[,!(colnames(descrip_stats_data_monthly) %in% monthly_level_cols)]
descrip_stats_data_yearly1 <- unique(descrip_stats_data_yearly0)
row.names(descrip_stats_data_yearly1) <- seq(nrow(descrip_stats_data_yearly1))

descrip_stats_data_yearly2 <- descrip_stats_data_yearly1
descrip_stats_data_yearly2 <- as.data.table(descrip_stats_data_yearly2)
descrip_stats_data_yearly2[,na_count :=rowSums(is.na(.SD)),.SDcols=colnames(descrip_stats_data_yearly2)]
setorderv(descrip_stats_data_yearly2,c(identifier,"yr","na_count"),c(1,1,1))
setkeyv(descrip_stats_data_yearly2,c(identifier,"yr"))
#descrip_stats_data_yearly3 <- data_all2[,.SD[c(1)],by=c(identifier,"yr")]
descrip_stats_data_yearly3 <- descrip_stats_data_yearly2[,.SD[c(1)],by=c(identifier,"yr")]
descrip_stats_data_yearly3 <- as.data.frame(descrip_stats_data_yearly3,stringsAsFactors=F)

rm2(descrip_stats_data_yearly0,descrip_stats_data_yearly1,descrip_stats_data_yearly2)

### Create Aggregate Level Data
###
### NOT RELEVANT SINCE USING YEARLY HF DATA SNAPSHOTS!!! ####

descrip_stats_strategy_cols <- unique(descrip_stats_data_yearly3[,strat_col])
descrip_stats_strategy_cols <- sort(descrip_stats_strategy_cols)
descrip_stats_strategy_cols <- c(descrip_stats_strategy_cols[!(descrip_stats_strategy_cols %in% c("OTHERS"))],"OTHERS")

descrip_stats_ios_read_cols <- c("sentences_ios","words_ios","chars_no_space_ios","num_syll_ios","sntc_per_word_ios",
                                 "avg_sentc_length_ios","avg_word_length_ios","avg_syll_word_ios","sntc_per100_ios",
                                 "syll_per100_ios","lett_per100_ios","fog_hard_words_ios",
                                 "ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                                 "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios")

descrip_stats_ios_sim_cols <- names(descrip_stats_data_yearly3)[grep("pct_ios",names(descrip_stats_data_yearly3))] 

#descrip_stats_ios_tone_cols <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")
descrip_stats_ios_tone_cols <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty",
                                 "litigious_dv","modalstrong_dv","modalweak_dv","negative_dv","positive_dv","uncertainty_dv")

descrip_stats_ios_quartile_cols <- c(names(descrip_stats_data_yearly3)[grep("below_quartile1",names(descrip_stats_data_yearly3))],
                                  names(descrip_stats_data_yearly3)[grep("above_quartile3",names(descrip_stats_data_yearly3))])

# descrip_stats_ios_quartile_cols <- c("ari_ios_below_quartile1","ari_ios_above_quartile3","coleman_liau_ios_below_quartile1","coleman_liau_ios_above_quartile3",
#                                      "flesch_kincaid_ios_below_quartile1","flesch_kincaid_ios_above_quartile3","fog_ios_below_quartile1","fog_ios_above_quartile3",
#                                      "smog_ios_below_quartile1","smog_ios_above_quartile3","avg_grade_level_ios_below_quartile1","avg_grade_level_ios_above_quartile3",
#                                      "avg_grade_level_ac_ios_below_quartile1","avg_grade_level_ac_ios_above_quartile3","avg_grade_level_acf_ios_below_quartile1",
#                                      "avg_grade_level_acf_ios_above_quartile3","all_similarity_050pct_ios_below_quartile1","all_similarity_050pct_ios_above_quartile3",
#                                      "main_investment_strategy_similarity_050pct_ios_below_quartile1","main_investment_strategy_similarity_050pct_ios_above_quartile3",
#                                      "all_similarity_100pct_ios_below_quartile1","all_similarity_100pct_ios_above_quartile3",
#                                      "main_investment_strategy_similarity_100pct_ios_below_quartile1","main_investment_strategy_similarity_100pct_ios_above_quartile3",
#                                      "all_similarity_250pct_ios_below_quartile1","all_similarity_250pct_ios_above_quartile3",
#                                      "main_investment_strategy_similarity_250pct_ios_below_quartile1","main_investment_strategy_similarity_250pct_ios_above_quartile3",
#                                      "all_similarity_500pct_ios_below_quartile1","all_similarity_500pct_ios_above_quartile3",
#                                      "main_investment_strategy_similarity_500pct_ios_below_quartile1","main_investment_strategy_similarity_500pct_ios_above_quartile3",
#                                      "all_similarity_750pct_ios_below_quartile1","all_similarity_750pct_ios_above_quartile3",
#                                      "main_investment_strategy_similarity_750pct_ios_below_quartile1","main_investment_strategy_similarity_750pct_ios_above_quartile3",
#                                      "all_similarity_900pct_ios_below_quartile1","all_similarity_900pct_ios_above_quartile3")          


pattern_cols_sub <- c("per_positive_percent","num_zero_percent","per_repeats_percent","uniform_percent",
                      "string_percent","num_pairs_percent","per_negative_percent","ar_1_percent","indexrsq_percent",
                      "kink_percent","quality_score_trim0","quality_score_trim1","quality_score_trim2")

pattern_cols_all <- unique(unlist(lapply(pattern_cols_sub,function(x,cols){cols[grep(x,cols)]},cols=colnames(data_all))))


### All pattern cols

pattern_cols_99 <- pattern_cols_all[grep("_99_",pattern_cols_all)]
pattern_cols_99_any <- pattern_cols_99[grep("_any_",pattern_cols_99)]
pattern_cols_99_avg <- pattern_cols_99[grep("_avg_",pattern_cols_99)]

pattern_cols_95 <- pattern_cols_all[grep("_95_",pattern_cols_all)]
pattern_cols_95_any <- pattern_cols_95[grep("_any_",pattern_cols_95)]
pattern_cols_95_avg <- pattern_cols_95[grep("_avg_",pattern_cols_95)]

pattern_cols_90 <- pattern_cols_all[grep("_90_",pattern_cols_all)]
pattern_cols_90_any <- pattern_cols_90[grep("_any_",pattern_cols_90)]
pattern_cols_90_avg <- pattern_cols_90[grep("_avg_",pattern_cols_90)]

descrip_stats_pattern_cols <- c(pattern_cols_99_any,pattern_cols_99_avg,pattern_cols_95_any,pattern_cols_95_avg,pattern_cols_90_any,pattern_cols_90_avg)

#descrip_stats_pattern_cols_trim <- descrip_stats_pattern_cols[!(descrip_stats_pattern_cols %in% c(descrip_stats_pattern_cols[grep("per_positive",descrip_stats_pattern_cols)],descrip_stats_pattern_cols[grep("per_repeats",descrip_stats_pattern_cols)]))]
descrip_stats_pattern_cols_trim <- descrip_stats_pattern_cols[!(descrip_stats_pattern_cols %in% c(descrip_stats_pattern_cols[grep("per_positive",descrip_stats_pattern_cols)]))]

rm2(pattern_cols_99,pattern_cols_99_any,pattern_cols_99_avg)
rm2(pattern_cols_95,pattern_cols_95_any,pattern_cols_95_avg)
rm2(pattern_cols_90,pattern_cols_90_any,pattern_cols_90_avg)
rm2(pattern_cols_sub,pattern_cols_all)


### Revision cols
revision_cols <- c("Revision_DV","Revision_1BP_DV","Revision_10BP_DV","Revision_50BP_DV","Revision_100BP_DV")

#yearly_level_cols <- c("yr","year_group_id","sdnet_flow","sdpct_flow",descrip_stats_ios_read_cols,descrip_stats_ios_sim_cols,descrip_stats_ios_quartile_cols)
yearly_level_cols <- ""

data_all_lookup0 <- data_all[!is.na(data_all[,strat_col]),]

data_all_lookup1 <- unique(data_all_lookup0[,c(identifier,"yr")])
data_all_lookup1 <- data_all_lookup1[order(data_all_lookup1[,identifier],data_all_lookup1[,"yr"]),]
row.names(data_all_lookup1) <- seq(nrow(data_all_lookup1))

data_all_lookup <- data_all_lookup1
data_all_lookup <- as.data.table(data_all_lookup)
setkeyv(data_all_lookup,c(identifier,"yr"))
setorderv(data_all_lookup,c(identifier,"yr"),c(1,1))
data_all_lookup <- data_all_lookup[,.SD[c(1)],by=c(identifier)]
data_all_lookup <- as.data.frame(data_all_lookup,stringsAsFactors=F)

rm2(data_all_lookup0,data_all_lookup1)

descrip_stats_data_aggregate0 <- merge(data_all_lookup,descrip_stats_data_yearly3,
                                     by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                                     all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))
descrip_stats_data_aggregate1 <- descrip_stats_data_aggregate0[,!(colnames(descrip_stats_data_aggregate0) %in% yearly_level_cols)]

descrip_stats_data_aggregate2 <- descrip_stats_data_aggregate1[,colnames(descrip_stats_data_aggregate1)[!(colnames(descrip_stats_data_aggregate1) %in% c("Inception_Date"))]]

descrip_stats_data_aggregate <- data.frame(yr=NA,year_group_id=NA,unique(descrip_stats_data_aggregate2),stringsAsFactors=F)
descrip_stats_data_aggregate[,"yr"] <- 9999
descrip_stats_data_aggregate[,"year_group_id"] <- 9999
row.names(descrip_stats_data_aggregate) <- seq(nrow(descrip_stats_data_aggregate))

descrip_stats_data_aggregate_no_strat <- descrip_stats_data_aggregate[,colnames(descrip_stats_data_aggregate)[!(colnames(descrip_stats_data_aggregate) %in% c(strat_col))]]

rm2(data_all_lookup,descrip_stats_data_aggregate0,descrip_stats_data_aggregate1,descrip_stats_data_aggregate2)


### Model Parameters
descriptive_overall_vars_model1_vars <- c("pflow","sdpct_flow","mktadjret","mktadjret_sq","fund_ret_mkt_neg",
                                          "AUM","age_y",
                                          "total_fee","Management_Fee_bin","Performance_Fee_bin","Other_Fee_bin",
                                          "Sharpe_Ratio","Sortino_Ratio",
                                          "Listed_on_Exchange_bin","Hurdle_Rate_bin",
                                          "Domicile_onshore_bin","Leverage_bin","Lockup_bin","Minimum_Investment_Size_bin",
                                          "Flagship_bin","Closed_bin","Dead_bin")

#"high_water_mark_bin"


descriptive_overall_vars_model1 <- list(note="PA",data="descrip_stats_data_monthly",vars=descriptive_overall_vars_model1_vars)

descriptive_overall_vars_model2_vars <- c(descrip_stats_ios_read_cols,descrip_stats_ios_sim_cols,descrip_stats_ios_tone_cols)
descriptive_overall_vars_model2 <- list(note="PB",data="descrip_stats_data_yearly",vars=descriptive_overall_vars_model2_vars)

#descriptive_overall_vars_model3_vars <- c(descrip_stats_pattern_cols)
descriptive_overall_vars_model3_vars <- c(descriptive_overall_vars_model1_vars,descriptive_overall_vars_model2_vars,descrip_stats_pattern_cols,revision_cols)
descriptive_overall_vars_model3 <- list(note="PC",data="descrip_stats_data_aggregate",vars=descriptive_overall_vars_model3_vars)

descriptive_overall_vars_model <- list(descriptive_overall_vars_model1,descriptive_overall_vars_model2,descriptive_overall_vars_model3)

#descriptive_overall_vars_model_note <- sapply(descriptive_overall_vars_model,"[[","note")
#descriptive_overall_vars_model_data <- sapply(descriptive_overall_vars_model,"[[","data")
descriptive_overall_vars_model_vars <- sapply(descriptive_overall_vars_model,"[[","vars")

descriptive_overall_vars_model_vars_all <- data.frame(id=NA,var=unique(unlist(descriptive_overall_vars_model_vars)),stringsAsFactors=F)
descriptive_overall_vars_model_vars_all[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_all))
#colnames(descriptive_overall_vars_model_vars_all)[2] <- "var"

descriptive_overall_vars_model_vars_all <- descriptive_overall_vars_model_vars_all[order(descriptive_overall_vars_model_vars_all[,"id"],descriptive_overall_vars_model_vars_all[,"var"]),]
row.names(descriptive_overall_vars_model_vars_all) <- seq(nrow(descriptive_overall_vars_model_vars_all))



###############################################################################
cat("DESCRIPTIVE STATISTICS - OVERALL","\n")
###############################################################################

output_directory_descrip_stats_overall <- paste(output_directory,"descriptive_stats_overall","\\",sep="")
create_directory(output_directory_descrip_stats_overall,remove=1)

#descriptive_overall_groups_parameters <- data.frame(matrix(NA,ncol=5,nrow=3,dimnames=list(c(),c("Start_yr","End_yr","note","data","vars"))),stringsAsFactors=F)
#descriptive_overall_groups_parameters[01,] <- c(2007     ,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
#descriptive_overall_groups_parameters[02,] <- c(2007     ,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
#descriptive_overall_groups_parameters[03,] <- c(9999     ,9999    ,"PC","descrip_stats_data_aggregate","descriptive_overall_vars_model3_vars")

descriptive_overall_groups_parameters <- data.frame(matrix(NA,ncol=5,nrow=1,dimnames=list(c(),c("Start_yr","End_yr","note","data","vars"))),stringsAsFactors=F)
descriptive_overall_groups_parameters[1,] <- c(9999      ,9999   ,"PC","descrip_stats_data_aggregate_no_strat","descriptive_overall_vars_model3_vars")

a_ply(.data=descriptive_overall_groups_parameters,.margins=1,.fun=descriptive_stats_execute,
      vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_overall,
      stats_keep=c("n","mean","sd","min","quartile1","median","quartile3","max"),.progress="text")

rm2(output_directory_descrip_stats_overall,descriptive_overall_groups_parameters)


###############################################################################
cat("DESCRIPTIVE STATISTICS - BY YEAR ","\n")
###############################################################################

# output_directory_descrip_stats_by_year <- paste(output_directory,"descriptive_stats_yearly","\\",sep="")
# create_directory(output_directory_descrip_stats_by_year,remove=1)
# 
# descriptive_overall_groups_by_year_parameters <-  data.frame(matrix(NA,ncol=5,nrow=2,dimnames=list(c(),c("Start_yr","End_yr","note","data","vars"))),stringsAsFactors=F)
# descriptive_overall_groups_by_year_parameters[1,] <- c(start_year,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
# descriptive_overall_groups_by_year_parameters[2,] <- c(start_year,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
# 
# a_ply(.data=descriptive_overall_groups_by_year_parameters,.margins=1,.fun=descriptive_stats_by_group_execute,
#       vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_by_year,
#       by_var="yr",stat_types=c("mean","median"),col_order=seq(start_year,end_year,1),.progress="text")
# 
# rm2(output_directory_descrip_stats_by_year,descriptive_overall_groups_by_year_parameters)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY YEAR GROUP","\n")
###############################################################################

# output_directory_descrip_stats_by_year_group <- paste(output_directory,"descriptive_stats_yearly_group","\\",sep="")
# create_directory(output_directory_descrip_stats_by_year_group,remove=1)
# 
# descriptive_overall_groups_by_year_group_parameters <-  data.frame(matrix(NA,ncol=5,nrow=2,dimnames=list(c(),c("Start_yr","End_yr","note","data","vars"))),stringsAsFactors=F)
# descriptive_overall_groups_by_year_group_parameters[1,] <- c(start_year,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
# descriptive_overall_groups_by_year_group_parameters[2,] <- c(start_year,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
# 
# a_ply(.data=descriptive_overall_groups_by_year_group_parameters,.margins=1,.fun=descriptive_stats_by_group_execute,
#       vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_by_year_group,
#       by_var="year_group_id",stat_types=c("mean","median"),col_order=seq(1,3,1),.progress="text")
# 
# 
# rm2(output_directory_descrip_stats_by_year_group,descriptive_overall_groups_by_year_group_parameters)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY STRATEGY","\n")
###############################################################################

output_directory_descrip_stats_by_strategy <- paste(output_directory,"descriptive_stats_strategy","\\",sep="")
create_directory(output_directory_descrip_stats_by_strategy,remove=1)

# descriptive_overall_groups_by_strategy_parameters <-  data.frame(matrix(NA,ncol=5,nrow=3,dimnames=list(c(),c("Start_yr","End_yr","note","data","vars"))),stringsAsFactors=F)
# descriptive_overall_groups_by_strategy_parameters[1,] <- c(start_year,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
# descriptive_overall_groups_by_strategy_parameters[2,] <- c(start_year,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
# descriptive_overall_groups_by_strategy_parameters[3,] <- c(9999,9999,"PC","descrip_stats_data_aggregate","descriptive_overall_vars_model3_vars")

descriptive_overall_groups_by_strategy_parameters <-  data.frame(matrix(NA,ncol=5,nrow=1,dimnames=list(c(),c("Start_yr","End_yr","note","data","vars"))),stringsAsFactors=F)
descriptive_overall_groups_by_strategy_parameters[1,] <- c(9999,9999,"PC","descrip_stats_data_aggregate","descriptive_overall_vars_model3_vars")

a_ply(.data=descriptive_overall_groups_by_strategy_parameters,.margins=1,.fun=descriptive_stats_by_group_execute,
      vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_by_strategy,
      by_var=strat_col,stat_types=c("mean","median"),col_order=descrip_stats_strategy_cols,.progress="text")

rm2(output_directory_descrip_stats_by_strategy,descriptive_overall_groups_by_strategy_parameters)

rm2(descriptive_overall_vars_model1_vars,descriptive_overall_vars_model2_vars,descriptive_overall_vars_model3_vars)
rm2(monthly_level_cols,yearly_level_cols,descrip_stats_fund_vars_remove,descrip_stats_ios_vars_remove)
rm2(descriptive_overall_vars_model,descriptive_overall_vars_model1,descriptive_overall_vars_model2,descriptive_overall_vars_model3)
rm2(descriptive_overall_vars_model_vars,descriptive_overall_vars_model_vars_all)


###############################################################################
cat("CORRELATION MATRIX - IOS (PA),FLAGS (PB),& COMBINED (PC) ","\n")
###############################################################################

corr_decimals <- 2

output_directory_correlation <- paste(output_directory,"correlation","\\",sep="")
create_directory(output_directory_correlation,remove=1)

### Panel A

corr_text_vars_ios_sim <- descrip_stats_ios_sim_cols[grep("_900pct_ios",descrip_stats_ios_sim_cols)] 
corr_text_vars_ios <- c(corr_text_vars_ios_sim,"ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios")

#correlation_stars_PA0 <- corstarsl(data_all[,corr_text_vars_ios],round=corr_decimals)
correlation_stars_PA0 <- corstarsl(descrip_stats_data_aggregate[,corr_text_vars_ios],round=corr_decimals)

correlation_stars_PA <- matrix("",ncol=nrow(correlation_stars_PA0),nrow=nrow(correlation_stars_PA0),
                               dimnames=list(rownames(correlation_stars_PA0),rownames(correlation_stars_PA0)))

correlation_stars_PA0 <- data.frame(lapply(correlation_stars_PA0,as.character),stringsAsFactors=F)

for (i in 1:ncol(correlation_stars_PA0))
{
  temp_col_name <- colnames(correlation_stars_PA0)[i]
  correlation_stars_PA[,temp_col_name] <- correlation_stars_PA0[,temp_col_name]
  
  rm(temp_col_name)
}
rm(i)

diag(correlation_stars_PA) <- paste(format(1.0,digits=corr_decimals,nsmall=corr_decimals),"***",sep="")

correlation_stars_PA <- data.frame(var=row.names(correlation_stars_PA),correlation_stars_PA,stringsAsFactors=F)
row.names(correlation_stars_PA) <- seq(nrow(correlation_stars_PA))

write.csv(correlation_stars_PA,file=paste(output_directory_correlation,"correlation_stars_PA.csv",sep=""),na="",quote=T,row.names=F)

rm2(correlation_stars_PA0,correlation_stars_PA)


### Panel B

corr_text_vars_pattern0 <- descrip_stats_pattern_cols_trim[grep("_90",descrip_stats_pattern_cols_trim)] 

#corr_text_vars_pattern0_any <- corr_text_vars_pattern0[grep("_any_",corr_text_vars_pattern0)] 
#corr_text_vars_pattern0_avg <- corr_text_vars_pattern0[grep("_avg_",corr_text_vars_pattern0)] 

#corr_text_vars_pattern1 <- corr_text_vars_pattern0[!(corr_text_vars_pattern0 %in% c(corr_text_vars_pattern0[grep("trim0",corr_text_vars_pattern0)],corr_text_vars_pattern0[grep("trim1",corr_text_vars_pattern0)]))]
corr_text_vars_pattern1 <- corr_text_vars_pattern0[!(corr_text_vars_pattern0 %in% c(corr_text_vars_pattern0[grep("trim0",corr_text_vars_pattern0)],corr_text_vars_pattern0[grep("trim2",corr_text_vars_pattern0)]))]

corr_text_vars_pattern <- corr_text_vars_pattern1

#correlation_stars_PB <- corstar(data_all[,corr_text_vars_pattern],round=corr_decimals)
correlation_stars_PB0 <- corstarsl(data_all[,corr_text_vars_pattern],round=corr_decimals)

correlation_stars_PB <- matrix("",ncol=nrow(correlation_stars_PB0),nrow=nrow(correlation_stars_PB0),
                               dimnames=list(rownames(correlation_stars_PB0),rownames(correlation_stars_PB0)))

correlation_stars_PB0 <- data.frame(lapply(correlation_stars_PB0,as.character),stringsAsFactors=F)

for (i in 1:ncol(correlation_stars_PB0))
{
  temp_col_name <- colnames(correlation_stars_PB0)[i]
  correlation_stars_PB[,temp_col_name] <- correlation_stars_PB0[,temp_col_name]
  
  rm(temp_col_name)
}
rm(i)

diag(correlation_stars_PB) <- paste(format(1.0,digits=corr_decimals,nsmall=corr_decimals),"***",sep="")

correlation_stars_PB <- data.frame(var=row.names(correlation_stars_PB),correlation_stars_PB,stringsAsFactors=F)
row.names(correlation_stars_PB) <- seq(nrow(correlation_stars_PB))

write.csv(correlation_stars_PB,file=paste(output_directory_correlation,"correlation_stars_PB.csv",sep=""),na="",quote=T,row.names=F)

rm2(correlation_stars_PB0,correlation_stars_PB)


### Panel C

corr_tone_vars_ios <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty",
                        "litigious_dv","modalstrong_dv","modalweak_dv","negative_dv","positive_dv","uncertainty_dv")

#correlation_stars_PC <- corstar(data_all[,corr_tone_vars_ios],round=corr_decimals)
correlation_stars_PC0 <- corstarsl(data_all[,corr_tone_vars_ios],round=corr_decimals)

correlation_stars_PC <- matrix("",ncol=nrow(correlation_stars_PC0),nrow=nrow(correlation_stars_PC0),
                               dimnames=list(rownames(correlation_stars_PC0),rownames(correlation_stars_PC0)))

correlation_stars_PC0 <- data.frame(lapply(correlation_stars_PC0,as.character),stringsAsFactors=F)

for (i in 1:ncol(correlation_stars_PC0))
  
  temp_col_name <- colnames(correlation_stars_PC0)[i]
  correlation_stars_PC[,temp_col_name] <- correlation_stars_PC0[,temp_col_name]
  
  rm(temp_col_name)
}
rm(i)

diag(correlation_stars_PC) <- paste(format(1.0,digits=corr_decimals,nsmall=corr_decimals),"***",sep="")

correlation_stars_PC <- data.frame(var=row.names(correlation_stars_PC),correlation_stars_PC,stringsAsFactors=F)
row.names(correlation_stars_PC) <- seq(nrow(correlation_stars_PC))

write.csv(correlation_stars_PC,file=paste(output_directory_correlation,"correlation_stars_PC.csv",sep=""),na="",quote=T,row.names=F)

rm2(correlation_stars_PC0,correlation_stars_PC)


### Panel D
corr_revision_vars_ios <- c("Revision_DV","Revision_1BP_DV","Revision_10BP_DV","Revision_50BP_DV","Revision_100BP_DV")

corr_text_vars_comb <- c(corr_text_vars_pattern,corr_text_vars_ios,corr_tone_vars_ios,corr_revision_vars_ios)

#correlation_stars_PD <- corstar(data_all[,corr_text_vars_comb],round=corr_decimals)
correlation_stars_PD0 <- corstarsl(data_all[,corr_text_vars_comb],round=corr_decimals)

correlation_stars_PD <- matrix("",ncol=nrow(correlation_stars_PD0),nrow=nrow(correlation_stars_PD0),
                               dimnames=list(rownames(correlation_stars_PD0),rownames(correlation_stars_PD0)))

correlation_stars_PD0 <- data.frame(lapply(correlation_stars_PD0,as.character),stringsAsFactors=F)

for (i in 1:ncol(correlation_stars_PD0))
{
  
  temp_col_name <- colnames(correlation_stars_PD0)[i]
  correlation_stars_PD[,temp_col_name] <- correlation_stars_PD0[,temp_col_name]
  
  rm(temp_col_name)
}
rm(i)

diag(correlation_stars_PD) <- paste(format(1.0,digits=corr_decimals,nsmall=corr_decimals),"***",sep="")

correlation_stars_PD <- data.frame(var=row.names(correlation_stars_PD),correlation_stars_PD,stringsAsFactors=F)
row.names(correlation_stars_PD) <- seq(nrow(correlation_stars_PD))

write.csv(correlation_stars_PD,file=paste(output_directory_correlation,"correlation_stars_PD.csv",sep=""),na="",quote=T,row.names=F)

rm2(correlation_stars_PD0,correlation_stars_PD)

rm2(corr_text_vars_ios_sim,corr_text_vars_ios,corr_text_vars_pattern,corr_tone_vars_ios)
rm2(descrip_stats_strategy_cols,descrip_stats_ios_read_cols,descrip_stats_ios_sim_cols,descrip_stats_ios_tone_cols)
rm2(corr_text_vars_pattern0,corr_text_vars_pattern1,descrip_stats_pattern_cols,descrip_stats_pattern_cols_trim)  
rm2(output_directory_correlation,corr_decimals)

#rm2(descrip_stats_data_monthly,descrip_stats_data_yearly,descrip_stats_data_aggregate)


