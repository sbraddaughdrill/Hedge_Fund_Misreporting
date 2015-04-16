# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Multivariate.R
# Version: 1.0
# Date: 09.29.2014
# Purpose: Run regressions
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

source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=F)


###############################################################################
cat("SECTION: LIBRARIES","\n")
###############################################################################

#Load External Packages
# c("compare","cwhmisc","descr","fastmatch","formatR","gtools","Hmisc","installr",
#   "knitr","leaps","markdown","memisc","mitools","pander","pbapply",
#   "PerformanceAnalytics","psych","quantreg","R.oo","R2wd","reporttools","reshape2",
#   "rms","sqldf","stargazer","stringr","taRifx","UsingR","xtable","zoo")
external_packages <- c("data.table","gdata","lmtest","MASS","plm","plyr","reshape2","sandwich","texreg")
invisible(unlist(sapply(external_packages,load_external_packages,repo_str=repo,simplify=F,USE.NAMES=F)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("IMPORT DATA","\n")
###############################################################################

identifier <- "Fund_ID"

beg_year <- 1994
#beg_year <- 2007
end_year <- 2013

#strat_col <- "main_investment_strategy"
strat_col <- "Primary_Investment_Strategy_combcol"

data_all_org <- read.csv(file=paste(output_directory,"data_all",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)


###############################################################################
cat("TRIM DATA","\n")
###############################################################################

#data_all0 <- data_all_org[(data_all_org[,"yr"]>=beg_year & data_all_org[,"yr"]<=end_year),]
#data_all0 <- data_all_org[,!(colnames(data_all_org) %in% c("Fund_Name","Secondary_Investment_Strategy","Strategy","Strat_ID"))]
data_all0 <- data_all_org[,!(colnames(data_all_org) %in% c("Secondary_Investment_Strategy","Strat_ID"))]

rm2(data_all_org)


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

rm2(winsorize_vars)


###############################################################################
cat("MULTIVARIATE ANALYSIS - VARIABLES","\n")
###############################################################################

pattern_cols_sub <- c("per_positive_percent","num_zero_percent","per_repeats_percent","uniform_percent",
                      "string_percent","num_pairs_percent","per_negative_percent","ar_1_percent","indexrsq_percent",
                      "kink_percent","quality_score_trim0","quality_score_trim1","quality_score_trim2","quality_score_trim3")

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

pattern_cols <- c(pattern_cols_99_any,pattern_cols_99_avg,pattern_cols_95_any,pattern_cols_95_avg,pattern_cols_90_any,pattern_cols_90_avg)

### Quaity Score cols

quality_score_cols_trim0 <- pattern_cols_all[grep("quality_score_trim0",pattern_cols_all)]
quality_score_cols_trim1 <- pattern_cols_all[grep("quality_score_trim1",pattern_cols_all)]
quality_score_cols_trim2 <- pattern_cols_all[grep("quality_score_trim2",pattern_cols_all)]
quality_score_cols_trim3 <- pattern_cols_all[grep("quality_score_trim3",pattern_cols_all)]

#quality_score_cols <- c(quality_score_cols_trim0,quality_score_cols_trim1,quality_score_cols_trim2,quality_score_cols_trim3)
quality_score_cols <- c(quality_score_cols_trim0,quality_score_cols_trim1)

### Revision cols
revision_cols <- c("Revision_DV","Revision_1BP_DV","Revision_10BP_DV","Revision_50BP_DV","Revision_100BP_DV")


### Dep Vars (Text Vars)

multivariate_vars_dep <- c(pattern_cols,revision_cols)

### Continuous Vars

multivariate_vars_continuous_fund <- c("pflow","sdpct_flow","sdpct_flow_lag1",
                                       "Yearly_Ret2","Yearly_Ret2_lag1","Yearly_Ret2_sq","Yearly_Ret2_sq_lag1",
                                       "mktadjret","mktadjret_lag1",
                                       "mktadjret_sq","mktadjret_sq_lag1",
                                       "AUM","AUM_lag1","AUM_log","AUM_log_lag1",
                                       "Fund_Size_USm","Fund_Capacity_USm","Firms_Total_Asset_USm","Total_Asset_in_Hedge_Funds_USm",
                                       "age_y","age_m","Sharpe_Ratio","Sortino_Ratio",
                                       "total_fee","Management_Fee_bin","Performance_Fee_bin","Other_Fee_bin")

#"pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#"mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#"mktadjret_sq_lag1","mktadjret_sq_lag2","mktadjret_sq_lag3","mktadjret_sq_lag4",
#"AUM_log_lag1","AUM_log_lag2","AUM_log_lag3","AUM_log_lag4",

multivariate_vars_continuous_text <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                                       "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                                       "chars_no_space_ios","words_ios","sentences_ios",
                                       "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                                       "all_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios",
                                       "all_similarity_250pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
                                       "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",
                                       "all_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios",
                                       "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")

multivariate_vars_continuous_tone <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")

multivariate_vars_continuous <- c(multivariate_vars_continuous_fund,multivariate_vars_continuous_text,multivariate_vars_continuous_tone)


### Binary Vars

multivariate_vars_binary_fund <- c("Listed_on_Exchange_bin","Hurdle_Rate_bin","Domicile_onshore_bin","Leverage_bin","Lockup_bin",
                                   "Flagship_bin","Closed_bin","Dead_bin")

multivariate_vars_binary_tone <- c("litigious_dv","modalstrong_dv","modalweak_dv","negative_dv","positive_dv","uncertainty_dv")

multivariate_vars_binary <- c(multivariate_vars_binary_fund,multivariate_vars_binary_tone)

rm2(quality_score_cols_trim0,quality_score_cols_trim1,quality_score_cols_trim2,quality_score_cols_trim3)
rm2(pattern_cols_99,pattern_cols_99_any,pattern_cols_99_avg)
rm2(pattern_cols_95,pattern_cols_95_any,pattern_cols_95_avg)
rm2(pattern_cols_90,pattern_cols_90_any,pattern_cols_90_avg)
rm2(pattern_cols_sub,pattern_cols_all)
#rm2(pattern_cols,revision_cols)


###############################################################################
cat("ADD NEW COLUMNS","\n")
###############################################################################

data_all_multivariate_new_cols <- c("age_y_log","age_m_log","AUM_USm","AUM_USm_log","Fund_Size_USm_log","Fund_Capacity_USm_log","Fund_Utilization_Ratio",
                                    "Firm_Size_combcol","Firm_Size_combcol_log","Fund_to_Firm_Size",paste(strat_col,"2",sep=""))

#data_all_multivariate_full_keep_trim0 <- data.frame(unique(data_all[,c(identifier,"yr",multivariate_vars_dep,multivariate_vars_continuous)]),
#                                                    matrix(NA,ncol=length(data_all_multivariate_new_cols),nrow=1,dimnames=list(c(),data_all_multivariate_new_cols)),stringsAsFactors=F)

data_all_multivariate_full_keep_trim0 <- data.frame(unique(data_all[,colnames(data_all)[colnames(data_all) %in% c(identifier,"Fund_Name","Strategy","yr",strat_col,multivariate_vars_dep,multivariate_vars_continuous,multivariate_vars_binary)]]),
                                                    matrix(NA,ncol=length(data_all_multivariate_new_cols),nrow=1,dimnames=list(c(),data_all_multivariate_new_cols)),
                                                    #matrix(NA,ncol=length(quality_score_cols),nrow=1,dimnames=list(c(),paste(quality_score_cols,"rv",sep="_"))),
                                                    stringsAsFactors=F)

data_all_multivariate_full_keep_trim0[,"age_m"] <- data_all_multivariate_full_keep_trim0[,"age_m"]+1
data_all_multivariate_full_keep_trim0[,"age_m_log"] <- log(data_all_multivariate_full_keep_trim0[,"age_m"])

data_all_multivariate_full_keep_trim0[,"age_y"] <- data_all_multivariate_full_keep_trim0[,"age_m"]/12
data_all_multivariate_full_keep_trim0[,"age_y_log"] <- log(data_all_multivariate_full_keep_trim0[,"age_y"])

data_all_multivariate_full_keep_trim0[,"AUM_USm"] <- data_all_multivariate_full_keep_trim0[,"AUM"]/1000000
data_all_multivariate_full_keep_trim0[,"AUM_USm_log"] <- log(data_all_multivariate_full_keep_trim0[,"AUM_USm"])

data_all_multivariate_full_keep_trim0[,"Fund_Size_USm_log"] <- log(data_all_multivariate_full_keep_trim0[,"Fund_Size_USm"])
data_all_multivariate_full_keep_trim0[,"Fund_Capacity_USm_log"] <- log(data_all_multivariate_full_keep_trim0[,"Fund_Capacity_USm"])

data_all_multivariate_full_keep_trim0[,"Fund_Utilization_Ratio"] <- data_all_multivariate_full_keep_trim0[,"Fund_Size_USm"]/data_all_multivariate_full_keep_trim0[,"Fund_Capacity_USm"]

data_all_multivariate_full_keep_trim0[,"Firm_Size_combcol"] <- ifelse((is.na(data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"]) & is.na(data_all_multivariate_full_keep_trim0[,"Total_Asset_in_Hedge_Funds_USm"])),NA,
                                                                      ifelse((!is.na(data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"]) & is.na(data_all_multivariate_full_keep_trim0[,"Total_Asset_in_Hedge_Funds_USm"])),data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"],
                                                                             ifelse((is.na(data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"]) & !is.na(data_all_multivariate_full_keep_trim0[,"Total_Asset_in_Hedge_Funds_USm"])),data_all_multivariate_full_keep_trim0[,"Total_Asset_in_Hedge_Funds_USm"],data_all_multivariate_full_keep_trim0[,"Firms_Total_Asset_USm"])))

data_all_multivariate_full_keep_trim0[,"Firm_Size_combcol_log"] <- log(data_all_multivariate_full_keep_trim0[,"Firm_Size_combcol"])

data_all_multivariate_full_keep_trim0[,"Fund_to_Firm_Size"] <- data_all_multivariate_full_keep_trim0[,"Fund_Size_USm"]/data_all_multivariate_full_keep_trim0[,"Firm_Size_combcol"]

data_all_multivariate_full_keep_trim0[,paste(strat_col,"2",sep="")] <- ifelse(data_all_multivariate_full_keep_trim0[,strat_col] %in% c("ARBITRAGE","DEBT","DUAL APPROACH","EVENT DRIVEN","TOP DOWN","VALUE","OTHERS"),"OTHER2",data_all_multivariate_full_keep_trim0[,strat_col])

for (i in which(colnames(data_all_multivariate_full_keep_trim0) %in% multivariate_vars_binary))
{
  #i <- 1
  #i <- 2
  data_all_multivariate_full_keep_trim0[[i]] <- ifelse(is.na(data_all_multivariate_full_keep_trim0[[i]]),0,data_all_multivariate_full_keep_trim0[[i]])
}
rm(i)
for (i in which(colnames(data_all_multivariate_full_keep_trim0) %in% revision_cols))
{
  #i <- 1
  #i <- 2
  data_all_multivariate_full_keep_trim0[[i]] <- ifelse(is.na(data_all_multivariate_full_keep_trim0[[i]]),0,data_all_multivariate_full_keep_trim0[[i]])
}
rm(i)

data_all_multivariate_full_keep_trim <- data_all_multivariate_full_keep_trim0

rm2(data_all_multivariate_new_cols,data_all_multivariate_full_keep_trim0)


###############################################################################
cat("FIND FIRST YEAR FOR EACH FUND","\n")
###############################################################################

#data_all_multivariate_lookup0 <- data_all[!is.na(data_all[,strat_col]),]
data_all_multivariate_lookup0 <- data_all[(!is.na(data_all[,strat_col]) & !is.na(data_all[,"AUM"]) & !is.na(data_all[,"age_y"]) & !is.na(data_all[,"total_fee"])),]

data_all_multivariate_lookup1 <- unique(data_all_multivariate_lookup0[,c(identifier,"yr")])
data_all_multivariate_lookup1 <- data_all_multivariate_lookup1[order(data_all_multivariate_lookup1[,identifier],data_all_multivariate_lookup1[,"yr"]),]
row.names(data_all_multivariate_lookup1) <- seq(nrow(data_all_multivariate_lookup1))

data_all_multivariate_lookup <- data_all_multivariate_lookup1

data_all_multivariate_lookup <- as.data.table(data_all_multivariate_lookup)
setkeyv(data_all_multivariate_lookup,c(identifier,"yr"))
setorderv(data_all_multivariate_lookup,c(identifier,"yr"),c(1,1))
data_all_multivariate_lookup <- data_all_multivariate_lookup[,.SD[c(1)],by=c(identifier)]
data_all_multivariate_lookup <- as.data.frame(data_all_multivariate_lookup,stringsAsFactors=F)

rm2(data_all_multivariate_lookup0,data_all_multivariate_lookup1)

data_all_multivariate_full1 <- merge(data_all_multivariate_lookup,data_all_multivariate_full_keep_trim,
                                     by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                                     all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
data_all_multivariate_full1 <- data_all_multivariate_full1[order(data_all_multivariate_full1[,identifier],data_all_multivariate_full1[,"yr"]),]
row.names(data_all_multivariate_full1) <- seq(nrow(data_all_multivariate_full1))

rm2(data_all_multivariate_lookup,data_all_multivariate_full_keep_trim)

data_all_multivariate_full2 <- as.data.table(data_all_multivariate_full1)
setkeyv(data_all_multivariate_full2,c(identifier,"yr"))
setorderv(data_all_multivariate_full2,c(identifier,"yr"),c(1,1))

data_all_multivariate_full2 <- data_all_multivariate_full2[!is.na(Primary_Investment_Strategy_combcol)]
data_all_multivariate_full2 <- data_all_multivariate_full2[!is.na(AUM)]
data_all_multivariate_full2 <- data_all_multivariate_full2[!is.na(age_y)]
data_all_multivariate_full2 <- data_all_multivariate_full2[!is.na(total_fee)]

data_all_multivariate_full2[,na_count:=rowSums(is.na(.SD)),.SDcols=colnames(data_all_multivariate_full2)]
setorderv(data_all_multivariate_full2,c(identifier,"yr","na_count"),c(1,1,1))
data_all_multivariate_full2[,na_count:=NULL,by=NULL]

setkeyv(data_all_multivariate_full2,c(identifier,"yr"))
data_all_multivariate_full2 <- data_all_multivariate_full2[,.SD[c(1)],by=c(identifier,"yr")]
data_all_multivariate_full <- as.data.frame(data_all_multivariate_full2,stringsAsFactors=F)

rm2(data_all_multivariate_full1,data_all_multivariate_full2)


###############################################################################
cat("FIND FUNDS WITH SAME TEXT MEASURES","\n")
###############################################################################

data_all_multivariate_full_trim <- data_all_multivariate_full
data_all_multivariate_full_trim <- data_all_multivariate_full_trim[!(data_all_multivariate_full_trim[,identifier] %in% c(38679,38680)),]

rm2(data_all_multivariate_full)


###############################################################################
cat("FIND TOP-3 FOR EACH TEXT MEASURE - SETUP","\n")
###############################################################################

data_all_multivariate_full_cols_id <- c(identifier,"Fund_Name","Strategy")
data_all_multivariate_full_cols_nonid <- c("quality_score_trim1_90_any_024","quality_score_trim1_90_avg_024",
                                           "ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios","words_ios",
                                           "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",multivariate_vars_continuous_tone)


data_summary_rank_lookup <- data_all_multivariate_full_trim[,c(identifier,"Fund_Name","yr")]

top_bot_out_path <- paste(output_directory,"Top_Bottom_Stats",sep="//",collapse="//")  
create_directory(top_bot_out_path,remove=1)


###############################################################################
cat("FIND TOP-3 FOR EACH TEXT MEASURE - WINSORIZE","\n")
###############################################################################

data_summary_rank_winsorize <- merge(data_summary_rank_lookup,data_all_multivariate_full_trim[,c(data_all_multivariate_full_cols_id,"yr",data_all_multivariate_full_cols_nonid)],
                                      by.x=c(identifier,"yr","Fund_Name"),by.y=c(identifier,"yr","Fund_Name"),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_winsorize <- data_summary_rank_winsorize[order(data_summary_rank_winsorize[,identifier]),]
row.names(data_summary_rank_winsorize) <- seq(nrow(data_summary_rank_winsorize))

data_summary_rank_winsorize <- data.frame(data_summary_rank_winsorize,
                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"rank",sep="_"))),
                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"top3",sep="_"))),
                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"bot3",sep="_"))),
                                           stringsAsFactors=F)

data_summary_rank_winsorize_trim0 <- ldply(.data=data_all_multivariate_full_cols_nonid,.fun=function(x,data,id_cols){
  
  # x <- data_all_multivariate_full_cols_nonid[[1]]
  # data <- data_summary_rank_winsorize
  # id_cols <- data_all_multivariate_full_cols_id
  
  #cat(data_all_multivariate_full_cols_nonid[[i]], "\n")
  
  temp_var <- unlist(x)
  
  temp_vector <- data.frame(temp_col=sort(unique(data[,temp_var])),matrix(NA,ncol=3,nrow=1,dimnames=list(c(),c("temp_rank","temp_top3","temp_bot3"))),stringsAsFactors=F)
  temp_vector[,"temp_rank"] <- seq(1,nrow(temp_vector))
  temp_vector[,"temp_top3"] <-ifelse(temp_vector[,"temp_rank"] %in% tail(temp_vector[,"temp_rank"],3),temp_vector[,"temp_rank"],0)
  temp_vector[,"temp_bot3"] <-ifelse(temp_vector[,"temp_rank"] %in% head(temp_vector[,"temp_rank"],3),temp_vector[,"temp_rank"],0)
  
  temp_vector <- temp_vector[order(-temp_vector[,"temp_top3"]),]
  temp_vector[,"temp_top3"] <- c("T1","T2","T3",rep(NA,nrow(temp_vector)-3))
  
  temp_vector <- temp_vector[order(-temp_vector[,"temp_bot3"]),]
  temp_vector[,"temp_bot3"] <- c("B3","B2","B1",rep(NA,nrow(temp_vector)-3))
  
  temp_vector[,"temp_rank"] <- ifelse((is.na(temp_vector[,"temp_top3"]) & is.na(temp_vector[,"temp_bot3"])),NA,
                                      ifelse((!is.na(temp_vector[,"temp_top3"]) & is.na(temp_vector[,"temp_bot3"])),temp_vector[,"temp_top3"],
                                             ifelse((is.na(temp_vector[,"temp_top3"]) & !is.na(temp_vector[,"temp_bot3"])),temp_vector[,"temp_bot3"],
                                                    paste(temp_vector[,"temp_top3"],temp_vector[,"temp_bot3"],sep=","))))
  
  colnames(temp_vector)[match("temp_col",names(temp_vector))] <- temp_var
  colnames(temp_vector)[match("temp_rank",names(temp_vector))] <- paste(temp_var,"rank",sep="_")
  colnames(temp_vector)[match("temp_top3",names(temp_vector))] <- paste(temp_var,"top3",sep="_")
  colnames(temp_vector)[match("temp_bot3",names(temp_vector))] <- paste(temp_var,"bot3",sep="_")
  
  data <- merge(data[,!(colnames(data) %in% paste(temp_var,c("rank","top3","bot3"),sep="_"))],temp_vector,
                by.x=temp_var,by.y=temp_var,all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
  
  data <- data[order(data[,identifier]),]
  row.names(data) <- seq(nrow(data))
  
  data <- data[,c(id_cols,colnames(data)[!(colnames(data) %in% id_cols)])]
  
  data <- data[,c(colnames(data)[!(colnames(data) %in% c(temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_")))],
                  c(temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_")))]
  
  
  data_trim <- data.frame(var_name=NA,data[,c(id_cols,temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_"))],stringsAsFactors=F)
  
  colnames(data_trim)[match(temp_var,names(data_trim))] <- "var_value"
  colnames(data_trim)[match(paste(temp_var,"rank",sep="_"),names(data_trim))] <- "rank"
  colnames(data_trim)[match(paste(temp_var,"top3",sep="_"),names(data_trim))] <- "top3"
  colnames(data_trim)[match(paste(temp_var,"bot3",sep="_"),names(data_trim))] <- "bot3"
  
  data_trim[,"var_name"] <- temp_var
  
  data_trim <- data_trim[,c(id_cols,colnames(data_trim)[!(colnames(data_trim) %in% id_cols)])]
  
  rm(temp_var,temp_vector)
  
  return(data_trim)
  
},data=data_summary_rank_winsorize,id_cols=data_all_multivariate_full_cols_id,.progress="none",.inform=F)

data_summary_rank_winsorize_trim1 <- data_summary_rank_winsorize_trim0[,colnames(data_summary_rank_winsorize_trim0)[!(colnames(data_summary_rank_winsorize_trim0) %in% c("top3","bot3"))]]
data_summary_rank_winsorize_trim2 <- data_summary_rank_winsorize_trim1[!(data_summary_rank_winsorize_trim1[,"var_name"] %in% c("quality_score_trim1_90_any_024","quality_score_trim1_90_avg_024","per_modalstrong","per_modalweak")),]
data_summary_rank_winsorize_trim3 <- data_summary_rank_winsorize_trim2[!((data_summary_rank_winsorize_trim2[,"var_name"] %in% c("per_litigious","per_negative","per_positive","per_uncertainty")) & (data_summary_rank_winsorize_trim2[,"var_value"]==0)),]
data_summary_rank_winsorize_trim4 <- data_summary_rank_winsorize_trim3[!is.na(data_summary_rank_winsorize_trim3[,"rank"]),]

rm(data_summary_rank_winsorize_trim0,data_summary_rank_winsorize_trim1,data_summary_rank_winsorize_trim2,data_summary_rank_winsorize_trim3)


## Readability

data_summary_rank_winsorize_readability_cast_data <- data_summary_rank_winsorize_trim4[data_summary_rank_winsorize_trim4[,"var_name"] %in% c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios","words_ios"),]

data_summary_rank_winsorize_readability_cast0a <- unique(data_summary_rank_winsorize_readability_cast_data[,data_all_multivariate_full_cols_id])

data_summary_rank_winsorize_readability_cast0b <- dcast(data=data_summary_rank_winsorize_readability_cast_data[,!(colnames(data_summary_rank_winsorize_readability_cast_data) %in% c("rank"))],
                                                         Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
data_summary_rank_winsorize_readability_cast0c <- dcast(data=data_summary_rank_winsorize_readability_cast_data[,!(colnames(data_summary_rank_winsorize_readability_cast_data) %in% c("var_value"))],
                                                         Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
colnames(data_summary_rank_winsorize_readability_cast0c) <- paste(colnames(data_summary_rank_winsorize_readability_cast0c),"rank",sep="_")
colnames(data_summary_rank_winsorize_readability_cast0c)[1] <- identifier

data_summary_rank_winsorize_readability_cast1 <- merge(data_summary_rank_winsorize_readability_cast0a,data_summary_rank_winsorize_readability_cast0b,
                                                        by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_winsorize_readability_cast2 <- merge(data_summary_rank_winsorize_readability_cast1,data_summary_rank_winsorize_readability_cast0c,
                                                        by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_winsorize_readability_cast3 <- data_summary_rank_winsorize_readability_cast2[,sort(colnames(data_summary_rank_winsorize_readability_cast2))]

data_summary_rank_winsorize_readability_out <- data_summary_rank_winsorize_readability_cast3[,c(data_all_multivariate_full_cols_id,
                                                                                                  colnames(data_summary_rank_winsorize_readability_cast3)[!(colnames(data_summary_rank_winsorize_readability_cast3) %in% data_all_multivariate_full_cols_id)])]

data_summary_rank_winsorize_readability_out <- data_summary_rank_winsorize_readability_out[order(data_summary_rank_winsorize_readability_out[,identifier]),]
row.names(data_summary_rank_winsorize_readability_out) <- seq(nrow(data_summary_rank_winsorize_readability_out))

write.csv(data_summary_rank_winsorize_readability_out,file=paste(top_bot_out_path,"//","Rank_Winsorize_Readability",".csv",sep=""),row.names=FALSE)

rm2(data_summary_rank_winsorize_readability_cast_data)
rm2(data_summary_rank_winsorize_readability_cast0a,data_summary_rank_winsorize_readability_cast0b,data_summary_rank_winsorize_readability_cast0c)
rm2(data_summary_rank_winsorize_readability_cast1,data_summary_rank_winsorize_readability_cast2,data_summary_rank_winsorize_readability_cast3)

## Similarity

data_summary_rank_winsorize_similarity_cast_data <- data_summary_rank_winsorize_trim4[data_summary_rank_winsorize_trim4[,"var_name"] %in% c("all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios"),]

data_summary_rank_winsorize_similarity_cast0a <- unique(data_summary_rank_winsorize_similarity_cast_data[,data_all_multivariate_full_cols_id])

data_summary_rank_winsorize_similarity_cast0b <- dcast(data=data_summary_rank_winsorize_similarity_cast_data[,!(colnames(data_summary_rank_winsorize_similarity_cast_data) %in% c("rank"))],
                                                         Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
data_summary_rank_winsorize_similarity_cast0c <- dcast(data=data_summary_rank_winsorize_similarity_cast_data[,!(colnames(data_summary_rank_winsorize_similarity_cast_data) %in% c("var_value"))],
                                                         Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
colnames(data_summary_rank_winsorize_similarity_cast0c) <- paste(colnames(data_summary_rank_winsorize_similarity_cast0c),"rank",sep="_")
colnames(data_summary_rank_winsorize_similarity_cast0c)[1] <- identifier

data_summary_rank_winsorize_similarity_cast1 <- merge(data_summary_rank_winsorize_similarity_cast0a,data_summary_rank_winsorize_similarity_cast0b,
                                                        by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_winsorize_similarity_cast2 <- merge(data_summary_rank_winsorize_similarity_cast1,data_summary_rank_winsorize_similarity_cast0c,
                                                        by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_winsorize_similarity_cast3 <- data_summary_rank_winsorize_similarity_cast2[,sort(colnames(data_summary_rank_winsorize_similarity_cast2))]

data_summary_rank_winsorize_similarity_out <- data_summary_rank_winsorize_similarity_cast3[,c(data_all_multivariate_full_cols_id,
                                                                                                  colnames(data_summary_rank_winsorize_similarity_cast3)[!(colnames(data_summary_rank_winsorize_similarity_cast3) %in% data_all_multivariate_full_cols_id)])]

data_summary_rank_winsorize_similarity_out <- data_summary_rank_winsorize_similarity_out[order(data_summary_rank_winsorize_similarity_out[,identifier]),]
row.names(data_summary_rank_winsorize_similarity_out) <- seq(nrow(data_summary_rank_winsorize_similarity_out))

write.csv(data_summary_rank_winsorize_similarity_out,file=paste(top_bot_out_path,"//","Rank_Winsorize_Similarity",".csv",sep=""),row.names=FALSE)

rm2(data_summary_rank_winsorize_similarity_cast_data)
rm2(data_summary_rank_winsorize_similarity_cast0a,data_summary_rank_winsorize_similarity_cast0b,data_summary_rank_winsorize_similarity_cast0c)
rm2(data_summary_rank_winsorize_similarity_cast1,data_summary_rank_winsorize_similarity_cast2,data_summary_rank_winsorize_similarity_cast3)

## Tone

data_summary_rank_winsorize_tone_cast_data <- data_summary_rank_winsorize_trim4[data_summary_rank_winsorize_trim4[,"var_name"] %in% multivariate_vars_continuous_tone,]

data_summary_rank_winsorize_tone_cast0a <- unique(data_summary_rank_winsorize_tone_cast_data[,data_all_multivariate_full_cols_id])

data_summary_rank_winsorize_tone_cast0b <- dcast(data=data_summary_rank_winsorize_tone_cast_data[,!(colnames(data_summary_rank_winsorize_tone_cast_data) %in% c("rank"))],
                                                        Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
data_summary_rank_winsorize_tone_cast0c <- dcast(data=data_summary_rank_winsorize_tone_cast_data[,!(colnames(data_summary_rank_winsorize_tone_cast_data) %in% c("var_value"))],
                                                        Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
colnames(data_summary_rank_winsorize_tone_cast0c) <- paste(colnames(data_summary_rank_winsorize_tone_cast0c),"rank",sep="_")
colnames(data_summary_rank_winsorize_tone_cast0c)[1] <- identifier

data_summary_rank_winsorize_tone_cast1 <- merge(data_summary_rank_winsorize_tone_cast0a,data_summary_rank_winsorize_tone_cast0b,
                                                       by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_winsorize_tone_cast2 <- merge(data_summary_rank_winsorize_tone_cast1,data_summary_rank_winsorize_tone_cast0c,
                                                       by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_winsorize_tone_cast3 <- data_summary_rank_winsorize_tone_cast2[,sort(colnames(data_summary_rank_winsorize_tone_cast2))]

data_summary_rank_winsorize_tone_out <- data_summary_rank_winsorize_tone_cast3[,c(data_all_multivariate_full_cols_id,
                                                                                                colnames(data_summary_rank_winsorize_tone_cast3)[!(colnames(data_summary_rank_winsorize_tone_cast3) %in% data_all_multivariate_full_cols_id)])]

data_summary_rank_winsorize_tone_out <- data_summary_rank_winsorize_tone_out[order(data_summary_rank_winsorize_tone_out[,identifier]),]
row.names(data_summary_rank_winsorize_tone_out) <- seq(nrow(data_summary_rank_winsorize_tone_out))

write.csv(data_summary_rank_winsorize_tone_out,file=paste(top_bot_out_path,"//","Rank_Winsorize_Tone",".csv",sep=""),row.names=FALSE)

rm2(data_summary_rank_winsorize_tone_cast_data)
rm2(data_summary_rank_winsorize_tone_cast0a,data_summary_rank_winsorize_tone_cast0b,data_summary_rank_winsorize_tone_cast0c)
rm2(data_summary_rank_winsorize_tone_cast1,data_summary_rank_winsorize_tone_cast2,data_summary_rank_winsorize_tone_cast3)

rm2(data_summary_rank_winsorize_trim4,data_summary_rank_winsorize)
rm2(data_summary_rank_winsorize_readability_out,data_summary_rank_winsorize_similarity_out,data_summary_rank_winsorize_tone_out)


###############################################################################
cat("FIND TOP-3 FOR EACH TEXT MEASURE - NON WINSORIZE","\n")
###############################################################################

data_summary_rank_non_winsorize <- merge(data_summary_rank_lookup,unique(data_all1[,c(data_all_multivariate_full_cols_id,"yr",data_all_multivariate_full_cols_nonid)]),
                                          by.x=c(identifier,"yr","Fund_Name"),by.y=c(identifier,"yr","Fund_Name"),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_non_winsorize <- data_summary_rank_non_winsorize[order(data_summary_rank_non_winsorize[,identifier]),]
row.names(data_summary_rank_non_winsorize) <- seq(nrow(data_summary_rank_non_winsorize))

data_summary_rank_non_winsorize <- data.frame(data_summary_rank_non_winsorize,
                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"rank",sep="_"))),
                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"top3",sep="_"))),
                                           matrix(NA,ncol=length(data_all_multivariate_full_cols_nonid),nrow=1,dimnames=list(c(),paste(data_all_multivariate_full_cols_nonid,"bot3",sep="_"))),
                                           stringsAsFactors=F)

data_summary_rank_non_winsorize_trim0 <- ldply(.data=data_all_multivariate_full_cols_nonid,.fun=function(x,data,id_cols){
  
  # x <- data_all_multivariate_full_cols_nonid[[1]]
  # data <- data_summary_rank_non_winsorize
  # id_cols <- data_all_multivariate_full_cols_id
  
  #cat(data_all_multivariate_full_cols_nonid[[i]], "\n")
  
  temp_var <- unlist(x)
  
  temp_vector <- data.frame(temp_col=sort(unique(data[,temp_var])),matrix(NA,ncol=3,nrow=1,dimnames=list(c(),c("temp_rank","temp_top3","temp_bot3"))),stringsAsFactors=F)
  temp_vector[,"temp_rank"] <- seq(1,nrow(temp_vector))
  temp_vector[,"temp_top3"] <-ifelse(temp_vector[,"temp_rank"] %in% tail(temp_vector[,"temp_rank"],3),temp_vector[,"temp_rank"],0)
  temp_vector[,"temp_bot3"] <-ifelse(temp_vector[,"temp_rank"] %in% head(temp_vector[,"temp_rank"],3),temp_vector[,"temp_rank"],0)
  
  temp_vector <- temp_vector[order(-temp_vector[,"temp_top3"]),]
  temp_vector[,"temp_top3"] <- c("T1","T2","T3",rep(NA,nrow(temp_vector)-3))
  
  temp_vector <- temp_vector[order(-temp_vector[,"temp_bot3"]),]
  temp_vector[,"temp_bot3"] <- c("B3","B2","B1",rep(NA,nrow(temp_vector)-3))
  
  temp_vector[,"temp_rank"] <- ifelse((is.na(temp_vector[,"temp_top3"]) & is.na(temp_vector[,"temp_bot3"])),NA,
                                      ifelse((!is.na(temp_vector[,"temp_top3"]) & is.na(temp_vector[,"temp_bot3"])),temp_vector[,"temp_top3"],
                                             ifelse((is.na(temp_vector[,"temp_top3"]) & !is.na(temp_vector[,"temp_bot3"])),temp_vector[,"temp_bot3"],
                                                    paste(temp_vector[,"temp_top3"],temp_vector[,"temp_bot3"],sep=","))))
  
  colnames(temp_vector)[match("temp_col",names(temp_vector))] <- temp_var
  colnames(temp_vector)[match("temp_rank",names(temp_vector))] <- paste(temp_var,"rank",sep="_")
  colnames(temp_vector)[match("temp_top3",names(temp_vector))] <- paste(temp_var,"top3",sep="_")
  colnames(temp_vector)[match("temp_bot3",names(temp_vector))] <- paste(temp_var,"bot3",sep="_")
  
  data <- merge(data[,!(colnames(data) %in% paste(temp_var,c("rank","top3","bot3"),sep="_"))],temp_vector,
                by.x=temp_var,by.y=temp_var,all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
  
  data <- data[order(data[,identifier]),]
  row.names(data) <- seq(nrow(data))
  
  data <- data[,c(id_cols,colnames(data)[!(colnames(data) %in% id_cols)])]
  
  data <- data[,c(colnames(data)[!(colnames(data) %in% c(temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_")))],
                  c(temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_")))]
  
  
  data_trim <- data.frame(var_name=NA,data[,c(id_cols,temp_var,paste(temp_var,c("rank","top3","bot3"),sep="_"))],stringsAsFactors=F)
  
  colnames(data_trim)[match(temp_var,names(data_trim))] <- "var_value"
  colnames(data_trim)[match(paste(temp_var,"rank",sep="_"),names(data_trim))] <- "rank"
  colnames(data_trim)[match(paste(temp_var,"top3",sep="_"),names(data_trim))] <- "top3"
  colnames(data_trim)[match(paste(temp_var,"bot3",sep="_"),names(data_trim))] <- "bot3"
  
  data_trim[,"var_name"] <- temp_var
  
  data_trim <- data_trim[,c(id_cols,colnames(data_trim)[!(colnames(data_trim) %in% id_cols)])]
  
  rm(temp_var,temp_vector)
  
  return(data_trim)
  
},data=data_summary_rank_non_winsorize,id_cols=data_all_multivariate_full_cols_id,.progress="none",.inform=F)

data_summary_rank_non_winsorize_trim1 <- data_summary_rank_non_winsorize_trim0[,colnames(data_summary_rank_non_winsorize_trim0)[!(colnames(data_summary_rank_non_winsorize_trim0) %in% c("top3","bot3"))]]
data_summary_rank_non_winsorize_trim2 <- data_summary_rank_non_winsorize_trim1[!(data_summary_rank_non_winsorize_trim1[,"var_name"] %in% c("quality_score_trim1_90_any_024","quality_score_trim1_90_avg_024","per_modalstrong","per_modalweak")),]
data_summary_rank_non_winsorize_trim3 <- data_summary_rank_non_winsorize_trim2[!((data_summary_rank_non_winsorize_trim2[,"var_name"] %in% c("per_litigious","per_negative","per_positive","per_uncertainty")) & (data_summary_rank_non_winsorize_trim2[,"var_value"]==0)),]
data_summary_rank_non_winsorize_trim4 <- data_summary_rank_non_winsorize_trim3[!is.na(data_summary_rank_non_winsorize_trim3[,"rank"]),]

rm(data_summary_rank_non_winsorize_trim0,data_summary_rank_non_winsorize_trim1,data_summary_rank_non_winsorize_trim2,data_summary_rank_non_winsorize_trim3)


## Readability

data_summary_rank_non_winsorize_readability_cast_data <- data_summary_rank_non_winsorize_trim4[data_summary_rank_non_winsorize_trim4[,"var_name"] %in% c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios","words_ios"),]

data_summary_rank_non_winsorize_readability_cast0a <- unique(data_summary_rank_non_winsorize_readability_cast_data[,data_all_multivariate_full_cols_id])

data_summary_rank_non_winsorize_readability_cast0b <- dcast(data=data_summary_rank_non_winsorize_readability_cast_data[,!(colnames(data_summary_rank_non_winsorize_readability_cast_data) %in% c("rank"))],
                                                         Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
data_summary_rank_non_winsorize_readability_cast0c <- dcast(data=data_summary_rank_non_winsorize_readability_cast_data[,!(colnames(data_summary_rank_non_winsorize_readability_cast_data) %in% c("var_value"))],
                                                         Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
colnames(data_summary_rank_non_winsorize_readability_cast0c) <- paste(colnames(data_summary_rank_non_winsorize_readability_cast0c),"rank",sep="_")
colnames(data_summary_rank_non_winsorize_readability_cast0c)[1] <- identifier

data_summary_rank_non_winsorize_readability_cast1 <- merge(data_summary_rank_non_winsorize_readability_cast0a,data_summary_rank_non_winsorize_readability_cast0b,
                                                        by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_non_winsorize_readability_cast2 <- merge(data_summary_rank_non_winsorize_readability_cast1,data_summary_rank_non_winsorize_readability_cast0c,
                                                        by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_non_winsorize_readability_cast3 <- data_summary_rank_non_winsorize_readability_cast2[,sort(colnames(data_summary_rank_non_winsorize_readability_cast2))]

data_summary_rank_non_winsorize_readability_out <- data_summary_rank_non_winsorize_readability_cast3[,c(data_all_multivariate_full_cols_id,
                                                                                                  colnames(data_summary_rank_non_winsorize_readability_cast3)[!(colnames(data_summary_rank_non_winsorize_readability_cast3) %in% data_all_multivariate_full_cols_id)])]

data_summary_rank_non_winsorize_readability_out <- data_summary_rank_non_winsorize_readability_out[order(data_summary_rank_non_winsorize_readability_out[,identifier]),]
row.names(data_summary_rank_non_winsorize_readability_out) <- seq(nrow(data_summary_rank_non_winsorize_readability_out))

write.csv(data_summary_rank_non_winsorize_readability_out,file=paste(top_bot_out_path,"//","Rank_Non_Winsorize_Readability",".csv",sep=""),row.names=FALSE)

rm2(data_summary_rank_non_winsorize_readability_cast_data)
rm2(data_summary_rank_non_winsorize_readability_cast0a,data_summary_rank_non_winsorize_readability_cast0b,data_summary_rank_non_winsorize_readability_cast0c)
rm2(data_summary_rank_non_winsorize_readability_cast1,data_summary_rank_non_winsorize_readability_cast2,data_summary_rank_non_winsorize_readability_cast3)

## Similarity

data_summary_rank_non_winsorize_similarity_cast_data <- data_summary_rank_non_winsorize_trim4[data_summary_rank_non_winsorize_trim4[,"var_name"] %in% c("all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios"),]

data_summary_rank_non_winsorize_similarity_cast0a <- unique(data_summary_rank_non_winsorize_similarity_cast_data[,data_all_multivariate_full_cols_id])

data_summary_rank_non_winsorize_similarity_cast0b <- dcast(data=data_summary_rank_non_winsorize_similarity_cast_data[,!(colnames(data_summary_rank_non_winsorize_similarity_cast_data) %in% c("rank"))],
                                                        Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
data_summary_rank_non_winsorize_similarity_cast0c <- dcast(data=data_summary_rank_non_winsorize_similarity_cast_data[,!(colnames(data_summary_rank_non_winsorize_similarity_cast_data) %in% c("var_value"))],
                                                        Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
colnames(data_summary_rank_non_winsorize_similarity_cast0c) <- paste(colnames(data_summary_rank_non_winsorize_similarity_cast0c),"rank",sep="_")
colnames(data_summary_rank_non_winsorize_similarity_cast0c)[1] <- identifier

data_summary_rank_non_winsorize_similarity_cast1 <- merge(data_summary_rank_non_winsorize_similarity_cast0a,data_summary_rank_non_winsorize_similarity_cast0b,
                                                       by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_non_winsorize_similarity_cast2 <- merge(data_summary_rank_non_winsorize_similarity_cast1,data_summary_rank_non_winsorize_similarity_cast0c,
                                                       by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_non_winsorize_similarity_cast3 <- data_summary_rank_non_winsorize_similarity_cast2[,sort(colnames(data_summary_rank_non_winsorize_similarity_cast2))]

data_summary_rank_non_winsorize_similarity_out <- data_summary_rank_non_winsorize_similarity_cast3[,c(data_all_multivariate_full_cols_id,
                                                                                                colnames(data_summary_rank_non_winsorize_similarity_cast3)[!(colnames(data_summary_rank_non_winsorize_similarity_cast3) %in% data_all_multivariate_full_cols_id)])]

data_summary_rank_non_winsorize_similarity_out <- data_summary_rank_non_winsorize_similarity_out[order(data_summary_rank_non_winsorize_similarity_out[,identifier]),]
row.names(data_summary_rank_non_winsorize_similarity_out) <- seq(nrow(data_summary_rank_non_winsorize_similarity_out))

write.csv(data_summary_rank_non_winsorize_similarity_out,file=paste(top_bot_out_path,"//","Rank_Non_Winsorize_Similarity",".csv",sep=""),row.names=FALSE)

rm2(data_summary_rank_non_winsorize_similarity_cast_data)
rm2(data_summary_rank_non_winsorize_similarity_cast0a,data_summary_rank_non_winsorize_similarity_cast0b,data_summary_rank_non_winsorize_similarity_cast0c)
rm2(data_summary_rank_non_winsorize_similarity_cast1,data_summary_rank_non_winsorize_similarity_cast2,data_summary_rank_non_winsorize_similarity_cast3)

## Tone

data_summary_rank_non_winsorize_tone_cast_data <- data_summary_rank_non_winsorize_trim4[data_summary_rank_non_winsorize_trim4[,"var_name"] %in% multivariate_vars_continuous_tone,]

data_summary_rank_non_winsorize_tone_cast0a <- unique(data_summary_rank_non_winsorize_tone_cast_data[,data_all_multivariate_full_cols_id])

data_summary_rank_non_winsorize_tone_cast0b <- dcast(data=data_summary_rank_non_winsorize_tone_cast_data[,!(colnames(data_summary_rank_non_winsorize_tone_cast_data) %in% c("rank"))],
                                                  Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="var_value")
data_summary_rank_non_winsorize_tone_cast0c <- dcast(data=data_summary_rank_non_winsorize_tone_cast_data[,!(colnames(data_summary_rank_non_winsorize_tone_cast_data) %in% c("var_value"))],
                                                  Fund_ID ~ var_name,margins=NULL,subset=NULL,fill=NULL,drop=F,value.var="rank")
colnames(data_summary_rank_non_winsorize_tone_cast0c) <- paste(colnames(data_summary_rank_non_winsorize_tone_cast0c),"rank",sep="_")
colnames(data_summary_rank_non_winsorize_tone_cast0c)[1] <- identifier

data_summary_rank_non_winsorize_tone_cast1 <- merge(data_summary_rank_non_winsorize_tone_cast0a,data_summary_rank_non_winsorize_tone_cast0b,
                                                 by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_non_winsorize_tone_cast2 <- merge(data_summary_rank_non_winsorize_tone_cast1,data_summary_rank_non_winsorize_tone_cast0c,
                                                 by.x=c(identifier),by.y=c(identifier),all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))

data_summary_rank_non_winsorize_tone_cast3 <- data_summary_rank_non_winsorize_tone_cast2[,sort(colnames(data_summary_rank_non_winsorize_tone_cast2))]

data_summary_rank_non_winsorize_tone_out <- data_summary_rank_non_winsorize_tone_cast3[,c(data_all_multivariate_full_cols_id,
                                                                                    colnames(data_summary_rank_non_winsorize_tone_cast3)[!(colnames(data_summary_rank_non_winsorize_tone_cast3) %in% data_all_multivariate_full_cols_id)])]

data_summary_rank_non_winsorize_tone_out <- data_summary_rank_non_winsorize_tone_out[order(data_summary_rank_non_winsorize_tone_out[,identifier]),]
row.names(data_summary_rank_non_winsorize_tone_out) <- seq(nrow(data_summary_rank_non_winsorize_tone_out))

write.csv(data_summary_rank_non_winsorize_tone_out,file=paste(top_bot_out_path,"//","Rank_Non_Winsorize_Tone",".csv",sep=""),row.names=FALSE)

rm2(data_summary_rank_non_winsorize_tone_cast_data)
rm2(data_summary_rank_non_winsorize_tone_cast0a,data_summary_rank_non_winsorize_tone_cast0b,data_summary_rank_non_winsorize_tone_cast0c)
rm2(data_summary_rank_non_winsorize_tone_cast1,data_summary_rank_non_winsorize_tone_cast2,data_summary_rank_non_winsorize_tone_cast3)

rm2(data_summary_rank_non_winsorize_trim4,data_summary_rank_non_winsorize)
rm2(data_summary_rank_non_winsorize_readability_out,data_summary_rank_non_winsorize_similarity_out,data_summary_rank_non_winsorize_tone_out)

rm2(data_summary_rank_lookup)


###############################################################################
cat("REGRESSION - FINALIZE DATA","\n")
###############################################################################

#data_reg0 <- data_all_multivariate_full_trim
data_reg0 <- data_all_multivariate_full_trim[,!(colnames(data_all_multivariate_full_trim) %in% c("Fund_Name","Strategy"))]

data_reg <- data_reg0
#data_reg <- data_reg0[data_reg0[,paste(strat_col,"2",sep="")]!="OTHER2",]

rm2(data_reg0)
#rm2(data_all_multivariate_full)

# a <- data_all[data_all[,identifier]==5445,]
# a <- data_reg[,c(identifier,"yr","age_y","AUM","total_fee",multivariate_vars_continuous_tone,multivariate_vars_binary_fund,multivariate_vars_binary_tone)]
# a_na <- a[rowSums(is.na(a)) > 0,]
# rm(a,a_na)

#cor_data <- data_reg[,c("age_y_log","age_y","age_m_log","age_m","AUM_USm","AUM_USm_log","Fund_Size_USm","Fund_Size_USm_log","Firm_Size_combcol","Firm_Size_combcol_log","Fund_to_Firm_Size")]
#cor_data <- data_reg[,c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios","per_litigious","per_negative","per_positive","per_uncertainty")]
#cor_test <- cor(as.matrix(cor_data),use="pairwise.complete.obs")

# b <-  data_reg[,c(identifier,"age_y_log","age_y","age_m_log","age_m")]

# c <-  data_reg[,c(identifier,revision_cols)]


###############################################################################
cat("REGRESSION - VARIABLES","\n")
###############################################################################

#pattern_str <- "kink_percent_PCT_ANYAVG_CUTOFF + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF + num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF + num_pairs_percent_PCT_ANYAVG_CUTOFF + per_negative_percent_PCT_ANYAVG_CUTOFF"
#quality_str <- "num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF + num_pairs_percent_PCT_ANYAVG_CUTOFF + per_negative_percent_PCT_ANYAVG_CUTOFF"
#nonquality_str <- "kink_percent_PCT_ANYAVG_CUTOFF  + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF"
#pb_str <- "kink_percent_PCT_ANYAVG_CUTOFF  + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF + num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF"
#read_str <- "ARI_ios + Coleman_Liau_ios + Flesch_Kincaid_ios + FOG_ios + SMOG_ios + avg_grade_level_ios"

#sim_type_all <- c("050pct","500pct","750pct","900pct")

strat_dvs <- paste("factor(",strat_col,")",sep="")
#strat_dvs <- paste("factor(",paste(strat_col,"2",sep=""),")",sep="")

# Continuous Dependents
#dep_input_cont_var <- c("quality_score_trim0","quality_score_trim1","quality_score_trim2","quality_score_trim3")
#dep_input_cont_var <- c("quality_score_trim0","quality_score_trim1")
dep_input_cont_var <- c("quality_score_trim1")
dep_input_cont0 <- ldply(.data=dep_input_cont_var,.fun=function(x,expand_col,cols_out){
  out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
  colnames(out) <- cols_out
  return(out)
},expand_col=c(90,95,99),cols_out=c("score","pct"))

dep_input_cont1 <- adply(.data=dep_input_cont0,.margins=1,.fun=function(x,expand_col,cols_out){
  out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
  colnames(out) <- cols_out
  return(out)
},expand_col=c("any","avg"),cols_out=c(colnames(dep_input_cont0),"any_avg"))

dep_input_cont2 <- adply(.data=dep_input_cont1,.margins=1,.fun=function(x,expand_col,cols_out){
  out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
  colnames(out) <- cols_out
  return(out)
},expand_col=sprintf("%03d",c(24,36,48,60)),cols_out=c(colnames(dep_input_cont1),"cutoff"))

dep_input_cont <- data.frame(dep_input_cont2,dep_var=NA,stringsAsFactors=F)
dep_input_cont[,"dep_var"] <- paste(dep_input_cont[,"score"],dep_input_cont[,"pct"],dep_input_cont[,"any_avg"],dep_input_cont[,"cutoff"],sep="_")

rm2(dep_input_cont_var,dep_input_cont0,dep_input_cont1,dep_input_cont2)


# Binary Dependents
dep_input_bin_var <- c("per_positive_percent","num_zero_percent","per_repeats_percent","uniform_percent","string_percent",
                       "num_pairs_percent","per_negative_percent","ar_1_percent","indexrsq_percent","kink_percent")
dep_input_bin0 <- ldply(.data=dep_input_bin_var,.fun=function(x,expand_col,cols_out){
  out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
  colnames(out) <- cols_out
  return(out)
},expand_col=c(90,95,99),cols_out=c("score","pct"))

dep_input_bin1 <- adply(.data=dep_input_bin0,.margins=1,.fun=function(x,expand_col,cols_out){
  out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
  colnames(out) <- cols_out
  return(out)
},expand_col=c("any","avg"),cols_out=c(colnames(dep_input_bin0),"any_avg"))

dep_input_bin2 <- adply(.data=dep_input_bin1,.margins=1,.fun=function(x,expand_col,cols_out){
  out <- data.frame(sapply(x,rep.int,times=length(expand_col)),temp_col=expand_col,stringsAsFactors=F)
  colnames(out) <- cols_out
  return(out)
},expand_col=sprintf("%03d",c(24,36,48,60)),cols_out=c(colnames(dep_input_bin1),"cutoff"))

dep_input_bin3 <- data.frame(dep_input_bin2,dep_var=NA,stringsAsFactors=F)
dep_input_bin3[,"dep_var"] <- paste(dep_input_bin3[,"score"],dep_input_bin3[,"pct"],dep_input_bin3[,"any_avg"],dep_input_bin3[,"cutoff"],sep="_")

dep_input_bin4 <- data.frame(matrix(NA,ncol=5,nrow=length(revision_cols),dimnames=list(c(),c("score","pct","any_avg","cutoff","dep_var"))),stringsAsFactors=F)
dep_input_bin4[,"dep_var"] <- revision_cols

dep_input_bin <- rbind(dep_input_bin3,dep_input_bin4)                           

rm2(dep_input_bin_var,dep_input_bin0,dep_input_bin1,dep_input_bin2,dep_input_bin3,dep_input_bin4)


###############################################################################
cat("REGRESSION - QUALITY SCORE - SELECTION","\n")
###############################################################################

output_dir_reg1 <- paste(output_directory,"reg_selection_cont","\\",sep="")
create_directory(output_dir_reg1,remove=1)

data_year_groups1 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups1[1,] <- c(beg_year,end_year)

dep_var1 <- unlist(dep_input_cont[,"dep_var"])
#controls1 <- c("age_y","AUM","total_fee",multivariate_vars_continuous_tone,multivariate_vars_binary_fund,multivariate_vars_binary_tone)
controls1 <- c("age_y","AUM","total_fee",multivariate_vars_continuous_tone,multivariate_vars_binary_fund)
model_type1 <- "pooling"
note1 <- "all_sim"
#sim_type1 <- c("900pct")
#sim_type1 <- c("050pct","900pct")
#sim_type1 <- c("050pct","750pct","900pct")
#sim_type1 <- c("050pct","500pct","750pct","900pct")
sim_type1 <- c("050pct","100pct","250pct","500pct","750pct","900pct")

regression_equations1_1 <- c("avg_grade_level_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls1)
regression_equations1_2 <- c("avg_grade_level_XXX","all_similarity_YYYpct_XXX",controls1)
regression_equations1_3 <- c("FOG_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls1)
#regression_equations1_3 <- c("Coleman_Liau_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls1)
regression_equations1_4 <- c("FOG_XXX","all_similarity_YYYpct_XXX",controls1)
#regression_equations1_4 <- c("Coleman_Liau_XXX","all_similarity_YYYpct_XXX",controls1)
regression_equations1_5 <- c("avg_grade_level_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls1,strat_dvs)
regression_equations1_6 <- c("avg_grade_level_XXX","all_similarity_YYYpct_XXX",controls1,strat_dvs)
regression_equations1_7 <- c("FOG_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls1,strat_dvs)
#regression_equations1_7 <- c("Coleman_Liau_XXX","Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",controls1,strat_dvs)
regression_equations1_8 <- c("FOG_XXX","all_similarity_YYYpct_XXX",controls1,strat_dvs)
#regression_equations1_8 <- c("Coleman_Liau_XXX","all_similarity_YYYpct_XXX",controls1,strat_dvs)

regression_equations1 <- list(regression_equations1_1,regression_equations1_2,regression_equations1_3,regression_equations1_4,regression_equations1_5,regression_equations1_6,regression_equations1_7,regression_equations1_8)
rm(regression_equations1_1,regression_equations1_2,regression_equations1_3,regression_equations1_4,regression_equations1_5,regression_equations1_6,regression_equations1_7,regression_equations1_8)

for (k in 1:nrow(data_year_groups1))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups1[k,1],"END YEAR:",data_year_groups1[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups1[k,1] & data_all[,"yr"]<=data_year_groups1[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups1[k,1] & data_reg[,"yr"]<=data_year_groups1[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var1))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type1))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var1[i],data_year_groups1[k,1],data_year_groups1[k,2],note1,sim_type1[j],sep="_")
      
      #FORWARD SELECTION
      
      se_fs <- rep(list(list()),length(regression_equations1))
      pval_fs <- rep(list(list()),length(regression_equations1))
      
      for (l in 1:length(regression_equations1))
      {
        # l <- 1
        # l <- 8
        
        ind_vars_reg0 <- regression_equations1[[l]]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type1[j],ind_vars_reg0,ignore.case=T)
        
        ind_vars_reg0 <- mapply(function(m,d){if(!is.na(m)&&m!=-1L){d<-regmatches(d,m)[[1]]};return(d)},gregexpr("(?<=factor\\().*?(?=\\))",ind_vars_reg0,perl=T),ind_vars_reg0)
        
        model_data <- data_temp[,c(dep_var1[i],ind_vars_reg0)]
        model_data_trim <- model_data[complete.cases(model_data),]
        
        reg0_forward <- lm(as.formula(paste(dep_var1[i],"1",sep="~")),data=model_data_trim,na.action=na.omit)
        reg0 <- stepAIC(reg0_forward, direction="forward",scope=paste("~",paste(ind_vars_reg0,sep="",collapse=" + "),sep=""),trace=F)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var1[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type1)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #se_fs[[l]] <- reg0_rse[,2]
        se_fs[[l]] <- reg0_rse[,4]
        pval_fs[[l]] <- reg0_rse[,4]
        
        assign(paste("reg_fs",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm(ind_vars_reg0,model_data,model_data_trim,reg0_forward,reg0,reg0_rse)
      }
      rm(l)
      
      #BACKWARD ELIMINATION
      
      se_be <- rep(list(list()),length(regression_equations1))
      pval_be <- rep(list(list()),length(regression_equations1))
      
      for (l in 1:length(regression_equations1))
      {
        # l <- 1
        
        ind_vars_reg0 <- regression_equations1[[l]]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type1[j],ind_vars_reg0,ignore.case=T)
        
        ind_vars_reg0 <- mapply(function(m,d){if(!is.na(m)&&m!=-1L){d<-regmatches(d,m)[[1]]};return(d)},gregexpr("(?<=factor\\().*?(?=\\))",ind_vars_reg0,perl=T),ind_vars_reg0)
        
        model_data <- data_temp[,c(dep_var1[i],ind_vars_reg0)]
        model_data_trim <- model_data[complete.cases(model_data),]
        
        reg0_backward <- lm(as.formula(paste(dep_var1[i],paste(ind_vars_reg0,sep="",collapse=" + "),sep="~")),data=model_data_trim,na.action=na.omit)
        reg0 <- stepAIC(reg0_backward, direction="backward",scope=paste("~",paste(ind_vars_reg0,sep="",collapse=" + "),sep=""),trace=F)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var1[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type1)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #se_be[[l]] <- reg0_rse[,2]
        se_be[[l]] <- reg0_rse[,4]
        pval_be[[l]] <- reg0_rse[,4]
        
        assign(paste("reg_be",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm(ind_vars_reg0,model_data,model_data_trim,reg0_backward,reg0,reg0_rse)
      }
      rm(l)
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg_fs",seq(1,length(regression_equations1)),sep="",collapse=","),",",paste("reg_be",seq(1,length(regression_equations1)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",c(paste(seq(1,length(regression_equations1)),"FS",sep="_"),paste(seq(1,length(regression_equations1)),"BE",sep="_")),")",sep=""),
              override.se=c(se_fs,se_be),override.pval=c(pval_fs,pval_be),stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg1,out_file_name,".doc",sep=""))
      
      rm(se_fs,se_be,pval_fs,pval_be,out_file_name)
      eval(parse(text=paste("rm(",paste("reg_fs",seq(1,length(regression_equations1)),sep="",collapse=","),")",sep="")))
      eval(parse(text=paste("rm(",paste("reg_be",seq(1,length(regression_equations1)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg1,data_year_groups1,dep_var1,model_type1,note1,sim_type1,controls1,regression_equations1)


###############################################################################
cat("REGRESSION - QUALITY SCORE - ALL SIM","\n")
###############################################################################

output_dir_reg2 <- paste(output_directory,"reg_all_sim_cont","\\",sep="")
create_directory(output_dir_reg2,remove=1)

data_year_groups2 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups2[1,] <- c(beg_year,end_year)

dep_var2 <- unlist(dep_input_cont[,"dep_var"])
#controls2 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin + Revision_DV + Sharpe_Ratio "
#controls2 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
#controls2 <- "age_y + AUM + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
#controls2 <- "age_y + AUM + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
controls2 <- "age_y_log + AUM_USm_log + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls2 <- "age_y + AUM + Lockup_bin"
model_type2 <- "pooling"
note2 <- "all_sim"
#sim_type2 <- c("900pct")
#sim_type2 <- c("050pct","900pct")
#sim_type2 <- c("050pct","750pct","900pct")
#sim_type2 <- c("050pct","500pct","750pct","900pct")
sim_type2 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#read2 <- "avg_grade_level_XXX"
#read2 <- "FOG_XXX"
#read2 <- "Coleman_Liau_XXX"
read2 <- "words_ios"
strat2 <- "all_similarity_YYYpct_XXX"

regression_equations2 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
regression_equations2[1,] <- c(read2,NA,NA,NA,NA,NA)
regression_equations2[2,] <- c(strat2,NA,NA,NA,NA,NA)
regression_equations2[3,] <- c(paste(read2,strat2,sep=" + "),NA,NA,NA,NA,NA)
regression_equations2[4,] <- c(read2,NA,controls2,NA,NA,NA)
regression_equations2[5,] <- c(strat2,NA,controls2,NA,NA,NA)
regression_equations2[6,] <- c(paste(read2,strat2,sep=" + "),NA,controls2,NA,NA,NA)
regression_equations2[7,] <- c(read2,NA,controls2,NA,strat_dvs,NA)
regression_equations2[8,] <- c(strat2,NA,controls2,NA,strat_dvs,NA)
regression_equations2[9,] <- c(paste(read2,strat2,sep=" + "),NA,controls2,NA,strat_dvs,NA)
for (i in 1:ncol(regression_equations2))
{
  #regression_equations2[i,] <-  gsub("PCT","90",regression_equations2[i,])
  #regression_equations2[i,] <-  gsub("ANYAVG","any",regression_equations2[i,])
  #regression_equations2[i,] <-  gsub("CUTOFF","024",regression_equations2[i,])
  regression_equations2[i,] <-  unknown_to_NA(regression_equations2[i,],unknowns_strings)
}
for (i in 1:nrow(regression_equations2))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations2[i,1:(ncol(regression_equations2)-1)],use.names=F))))
  regression_equations2[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}

for (k in 1:nrow(data_year_groups2))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups2[k,1],"END YEAR:",data_year_groups2[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups2[k,1] & data_all[,"yr"]<=data_year_groups2[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups2[k,1] & data_reg[,"yr"]<=data_year_groups2[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var2))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type2))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var2[i],data_year_groups2[k,1],data_year_groups2[k,2],note2,sim_type2[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations2) )
      se <- rep(list(list()),nrow(regression_equations2))
      pval <- rep(list(list()),nrow(regression_equations2))
      
      for (l in 1:nrow(regression_equations2))
      {
        # l <- 1
        # l <- 7
        # l <- 8
        
        ind_vars_reg0 <- regression_equations2[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type2[j],ind_vars_reg0,ignore.case=T)
        
        ind_vars_reg0_list <- gsub("^\\s+|\\s+$","",unlist(strsplit(ind_vars_reg0,split="\\+")),perl=T)
        ind_vars_reg0_list <- mapply(function(m,d){if(!is.na(m)&&m!=-1L){d<-regmatches(d,m)[[1]]};return(d)},gregexpr("(?<=factor\\().*?(?=\\))",ind_vars_reg0_list,perl=T),ind_vars_reg0_list)
        
        model_data <- data_temp[,c(dep_var2[i],ind_vars_reg0_list)]
        
        reg0 <- lm(as.formula(paste(dep_var2[i],ind_vars_reg0,sep="~")),model_data)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var2[i],ind_vars_reg0,sep="~")),data=model_data,model=model_type2)
        #reg0_rse <- cl.plm(model_data,reg0,model_data[,identifier])
        #reg0_rse <- mcl.plm(model_data,reg0,model_data[,identifier],model_data[,"yr"])
        #reg0_rse <- mcl(model_data,reg0,model_data[,identifier],model_data[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,model_data,ind_vars_reg0_list,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations2)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations2)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg2,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations2)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg2,data_year_groups2,dep_var2,model_type2,note2,sim_type2,controls2,read2,strat2,regression_equations2,temp_char_vec,k)


###############################################################################
cat("REGRESSION - QUALITY SCORE - CAT SIM","\n")
###############################################################################

output_dir_reg3 <- paste(output_directory,"reg_cat_sim_cont","\\",sep="")
create_directory(output_dir_reg3,remove=1)

data_year_groups3 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups3[1,] <- c(beg_year,end_year)

dep_var3 <- unlist(dep_input_cont[,"dep_var"])
#controls3 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin + Revision_DV + Sharpe_Ratio "
#controls3 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
#controls3 <- "age_y + AUM + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
controls3 <- "age_y_log + AUM_USm_log + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls3 <- "age_y + AUM + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls3 <- "age_y + AUM + Lockup_bin"
model_type3 <- "pooling"
note3 <- "cat_sim"
#sim_type3 <- c("900pct")
#sim_type3 <- c("050pct","900pct")
#sim_type3 <- c("050pct","750pct","900pct")
#sim_type3 <- c("050pct","500pct","750pct","900pct")
sim_type3 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#read3 <- "avg_grade_level_XXX"
#read3 <- "FOG_XXX"
#read3 <- "Coleman_Liau_XXX"
read3 <- "words_ios"
strat3 <- "Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX"

regression_equations3 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
regression_equations3[1,] <- c(read3,NA,NA,NA,NA,NA)
regression_equations3[2,] <- c(strat3,NA,NA,NA,NA,NA)
regression_equations3[3,] <- c(paste(read3,strat3,sep=" + "),NA,NA,NA,NA,NA)
regression_equations3[4,] <- c(read3,NA,controls3,NA,NA,NA)
regression_equations3[5,] <- c(strat3,NA,controls3,NA,NA,NA)
regression_equations3[6,] <- c(paste(read3,strat3,sep=" + "),NA,controls3,NA,NA,NA)
regression_equations3[7,] <- c(read3,NA,controls3,NA,strat_dvs,NA)
regression_equations3[8,] <- c(strat3,NA,controls3,NA,strat_dvs,NA)
regression_equations3[9,] <- c(paste(read3,strat3,sep=" + "),NA,controls3,NA,strat_dvs,NA)
for (i in 1:ncol(regression_equations3))
{
  #regression_equations3[i,] <-  gsub("PCT","90",regression_equations3[i,])
  #regression_equations3[i,] <-  gsub("ANYAVG","any",regression_equations3[i,])
  #regression_equations3[i,] <-  gsub("CUTOFF","024",regression_equations3[i,])
  regression_equations3[i,] <-  unknown_to_NA(regression_equations3[i,],unknowns_strings)
}
for (i in 1:nrow(regression_equations3))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations3[i,1:(ncol(regression_equations3)-1)],use.names=F))))
  regression_equations3[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}


for (k in 1:nrow(data_year_groups3))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups3[k,1],"END YEAR:",data_year_groups3[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups3[k,1] & data_all[,"yr"]<=data_year_groups3[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups3[k,1] & data_reg[,"yr"]<=data_year_groups3[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var3))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type3))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var3[i],data_year_groups3[k,1],data_year_groups3[k,2],note3,sim_type3[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations3) )
      se <- rep(list(list()),nrow(regression_equations3))
      pval <- rep(list(list()),nrow(regression_equations3))
      
      for (l in 1:nrow(regression_equations3))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations3[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type3[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- lm(as.formula(paste(dep_var3[i],ind_vars_reg0,sep="~")),data_temp)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var3[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type3)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations3)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations3)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg3,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations3)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg3,data_year_groups3,dep_var3,model_type3,note3,sim_type3,controls3,read3,strat3,regression_equations3,temp_char_vec,k)


###############################################################################
cat("REGRESSION - QUALITY SCORE - TONE","\n")
###############################################################################

output_dir_reg5 <- paste(output_directory,"reg_tone_cont","\\",sep="")
create_directory(output_dir_reg5,remove=1)

data_year_groups5 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups5[1,] <- c(beg_year,end_year)

dep_var5 <- unlist(dep_input_cont[,"dep_var"])
#controls5 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin + Revision_DV + Sharpe_Ratio "
#controls5 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
#controls5 <- "age_y + AUM + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
controls5 <- "age_y_log + AUM_USm_log + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls5 <- "age_y + AUM + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls5 <- "age_y + AUM + Lockup_bin"
model_type5 <- "pooling"
note5 <- "tone"
#sim_type5 <- c("900pct")
#sim_type5 <- c("050pct","900pct")
#sim_type5 <- c("050pct","750pct","900pct")
#sim_type5 <- c("050pct","500pct","750pct","900pct")
sim_type5 <- c("050pct","100pct","250pct","500pct","750pct","900pct")

regression_equations5 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
# regression_equations5[1,] <- c("per_litigious",NA,controls5,NA,NA,NA)
# regression_equations5[2,] <- c("per_modalstrong",NA,controls5,NA,NA,NA)
# regression_equations5[3,] <- c("per_modalweak",NA,controls5,NA,NA,NA)
# regression_equations5[4,] <- c("per_negative",NA,controls5,NA,NA,NA)
# regression_equations5[5,] <- c("per_positive",NA,controls5,NA,NA,NA)
# regression_equations5[6,] <- c("per_uncertainty",NA,controls5,NA,NA,NA)
# regression_equations5[7,] <- c("per_litigious + per_modalstrong + per_modalweak + per_negative + per_positive + per_uncertainty",NA,controls5,NA,NA,NA)
# regression_equations5[8,] <- c("per_litigious + per_modalstrong + per_modalweak + per_negative + per_positive + per_uncertainty",NA,controls5,NA,strat_dvs,NA)

regression_equations5[1,] <- c("per_litigious",NA,NA,NA,NA,NA)
regression_equations5[2,] <- c("per_negative",NA,NA,NA,NA,NA)
regression_equations5[3,] <- c("per_positive",NA,NA,NA,NA,NA)
regression_equations5[4,] <- c("per_uncertainty",NA,NA,NA,NA,NA)
regression_equations5[5,] <- c("per_litigious",NA,controls5,NA,NA,NA)
regression_equations5[6,] <- c("per_negative",NA,controls5,NA,NA,NA)
regression_equations5[7,] <- c("per_positive",NA,controls5,NA,NA,NA)
regression_equations5[8,] <- c("per_uncertainty",NA,controls5,NA,NA,NA)
regression_equations5[9,] <- c("per_litigious + per_negative + per_positive + per_uncertainty",NA,controls5,NA,NA,NA)
regression_equations5[10,] <- c("per_litigious + per_negative + per_positive + per_uncertainty",NA,controls5,NA,strat_dvs,NA)

regression_equations5 <- unknown_to_NA(regression_equations5,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations5))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations5[i,1:(ncol(regression_equations5)-1)],use.names=F))))
  regression_equations5[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}
# for (i in 1:ncol(regression_equations5))
# {
#   regression_equations5[i,] <-  gsub("PCT","90",regression_equations5[i,])
#   regression_equations5[i,] <-  gsub("ANYAVG","any",regression_equations5[i,])
#   regression_equations5[i,] <-  gsub("CUTOFF","024",regression_equations5[i,])
# }

for (k in 1:nrow(data_year_groups5))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups5[k,1],"END YEAR:",data_year_groups5[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups5[k,1] & data_all[,"yr"]<=data_year_groups5[k,2]),]
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups5[k,1] & data_reg[,"yr"]<=data_year_groups5[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var5))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type5))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var5[i],data_year_groups5[k,1],data_year_groups5[k,2],note5,sim_type5[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations5) )
      se <- rep(list(list()),nrow(regression_equations5))
      pval <- rep(list(list()),nrow(regression_equations5))
      
      for (l in 1:nrow(regression_equations5))
      {
        # l <- 1
        
        ind_vars_reg0 <- regression_equations5[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type5[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- lm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")),data_temp)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type5)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations5)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations5)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg5,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations5)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(output_dir_reg5,data_year_groups5,dep_var5,model_type5,note5,sim_type5,controls5,regression_equations5,temp_char_vec,k)


###############################################################################
cat("REGRESSION - PROBIT/LOGIT - SELECTION","\n")
###############################################################################




###############################################################################
cat("REGRESSION - PROBIT/LOGIT - ALL SIM","\n")
###############################################################################

output_dir_reg7 <- paste(output_directory,"reg_all_sim_bin","\\",sep="")
create_directory(output_dir_reg7,remove=1)

data_year_groups7 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups7[1,] <- c(beg_year,end_year)

#dep_var7 <- unlist(dep_input_bin[,"dep_var"])
dep_var7 <- unlist(dep_input_bin[grep("Revision",dep_input_bin[,"dep_var"]),"dep_var"])
#controls7 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin + Revision_DV + Sharpe_Ratio "
#controls7 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
#controls7 <- "age_y + AUM + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
controls7 <- "age_y_log + AUM_USm_log + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls7 <- "age_y + AUM + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls7 <- "age_y + AUM + Lockup_bin"
model_type7 <- "pooling"
note7 <- "all_sim"
#sim_type7 <- c("900pct")
#sim_type7 <- c("050pct","900pct")
#sim_type7 <- c("050pct","750pct","900pct")
#sim_type7 <- c("050pct","500pct","750pct","900pct")
sim_type7 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#read7 <- "avg_grade_level_XXX"
#read7 <- "FOG_XXX"
#read7 <- "Coleman_Liau_XXX"
read7 <- "words_ios"
strat7 <- "all_similarity_YYYpct_XXX"
family7 <- "logit"
#family7 <- "probit"

regression_equations7 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
regression_equations7[1,] <- c(read7,NA,NA,NA,NA,NA)
regression_equations7[2,] <- c(strat7,NA,NA,NA,NA,NA)
regression_equations7[3,] <- c(paste(read7,strat7,sep=" + "),NA,NA,NA,NA,NA)
regression_equations7[4,] <- c(read7,NA,controls7,NA,NA,NA)
regression_equations7[5,] <- c(strat7,NA,controls7,NA,NA,NA)
regression_equations7[6,] <- c(paste(read7,strat7,sep=" + "),NA,controls7,NA,NA,NA)
regression_equations7[7,] <- c(read7,NA,controls7,NA,strat_dvs,NA)
regression_equations7[8,] <- c(strat7,NA,controls7,NA,strat_dvs,NA)
regression_equations7[9,] <- c(paste(read7,strat7,sep=" + "),NA,controls7,NA,strat_dvs,NA)
for (i in 1:ncol(regression_equations7))
{
  #regression_equations7[i,] <-  gsub("PCT","90",regression_equations7[i,])
  #regression_equations7[i,] <-  gsub("ANYAVG","any",regression_equations7[i,])
  #regression_equations7[i,] <-  gsub("CUTOFF","024",regression_equations7[i,])
  regression_equations7[i,] <-  unknown_to_NA(regression_equations7[i,],unknowns_strings)
}
for (i in 1:nrow(regression_equations7))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations7[i,1:(ncol(regression_equations7)-1)],use.names=F))))
  regression_equations7[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}

for (k in 1:nrow(data_year_groups7))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups7[k,1],"END YEAR:",data_year_groups7[k,2],"\n")
  
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups7[k,1] & data_reg[,"yr"]<=data_year_groups7[k,2]),]
  
  for (i in 1:length(dep_var7))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type7))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var7[i],data_year_groups7[k,1],data_year_groups7[k,2],note7,sim_type7[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations7) )
      se <- rep(list(list()),nrow(regression_equations7))
      pval <- rep(list(list()),nrow(regression_equations7))
      
      for (l in 1:nrow(regression_equations7))
      {
        # l <- 1
        
        ind_vars_reg0 <- regression_equations7[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type7[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- glm(as.formula(paste(dep_var7[i],ind_vars_reg0,sep="~")),family=binomial(link=family7),data=data_temp)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var7[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type7)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations7)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations7)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg7,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations7)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,i)
}
rm2(output_dir_reg7,data_year_groups7,dep_var7,model_type7,note7,sim_type7,controls7,read7,strat7,family7,regression_equations7,temp_char_vec,k)


###############################################################################
cat("REGRESSION - PROBIT/LOGIT - CAT SIM","\n")
###############################################################################

output_dir_reg8 <- paste(output_directory,"reg_cat_sim_bin","\\",sep="")
create_directory(output_dir_reg8,remove=1)

data_year_groups8 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups8[1,] <- c(beg_year,end_year)

#dep_var8 <- unlist(dep_input_bin[,"dep_var"])
dep_var8 <- unlist(dep_input_bin[grep("Revision",dep_input_bin[,"dep_var"]),"dep_var"])
#controls8 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin + Revision_DV + Sharpe_Ratio "
#controls8 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
#controls8 <- "age_y + AUM + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
controls8 <- "age_y_log + AUM_USm_log + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls8 <- "age_y + AUM + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls8 <- "age_y + AUM + Lockup_bin"
model_type8 <- "pooling"
note8 <- "cat_sim"
#sim_type8 <- c("900pct")
#sim_type8 <- c("050pct","900pct")
#sim_type8 <- c("050pct","750pct","900pct")
#sim_type8 <- c("050pct","500pct","750pct","900pct")
sim_type8 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#read8 <- "avg_grade_level_XXX"
#read8 <- "FOG_XXX"
#read8 <- "Coleman_Liau_XXX"
read8 <- "words_ios"
strat8 <- "Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX"
family8 <- "logit"
#family8 <- "probit"

regression_equations8 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
regression_equations8[1,] <- c(read8,NA,NA,NA,NA,NA)
regression_equations8[2,] <- c(strat8,NA,NA,NA,NA,NA)
regression_equations8[3,] <- c(paste(read8,strat8,sep=" + "),NA,NA,NA,NA,NA)
regression_equations8[4,] <- c(read8,NA,controls8,NA,NA,NA)
regression_equations8[5,] <- c(strat8,NA,controls8,NA,NA,NA)
regression_equations8[6,] <- c(paste(read8,strat8,sep=" + "),NA,controls8,NA,NA,NA)
regression_equations8[7,] <- c(read8,NA,controls8,NA,strat_dvs,NA)
regression_equations8[8,] <- c(strat8,NA,controls8,NA,strat_dvs,NA)
regression_equations8[9,] <- c(paste(read8,strat8,sep=" + "),NA,controls8,NA,strat_dvs,NA)
for (i in 1:ncol(regression_equations8))
{
  #regression_equations8[i,] <-  gsub("PCT","90",regression_equations8[i,])
  #regression_equations8[i,] <-  gsub("ANYAVG","any",regression_equations8[i,])
  #regression_equations8[i,] <-  gsub("CUTOFF","024",regression_equations8[i,])
  regression_equations8[i,] <-  unknown_to_NA(regression_equations8[i,],unknowns_strings)
}
for (i in 1:nrow(regression_equations8))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations8[i,1:(ncol(regression_equations8)-1)],use.names=F))))
  regression_equations8[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}

for (k in 1:nrow(data_year_groups8))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups8[k,1],"END YEAR:",data_year_groups8[k,2],"\n")
  
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups8[k,1] & data_reg[,"yr"]<=data_year_groups8[k,2]),]
  
  for (i in 1:length(dep_var8))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type8))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var8[i],data_year_groups8[k,1],data_year_groups8[k,2],note8,sim_type8[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations8) )
      se <- rep(list(list()),nrow(regression_equations8))
      pval <- rep(list(list()),nrow(regression_equations8))
      
      for (l in 1:nrow(regression_equations8))
      {
        # l <- 1
        
        ind_vars_reg0 <- regression_equations8[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type8[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- glm(as.formula(paste(dep_var8[i],ind_vars_reg0,sep="~")),family=binomial(link=family8),data=data_temp)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var8[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type8)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations8)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations8)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg8,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations8)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,i)
}
rm2(output_dir_reg8,data_year_groups8,dep_var8,model_type8,note8,sim_type8,controls8,read8,strat8,family8,regression_equations8,temp_char_vec,k)


###############################################################################
cat("REGRESSION - PROBIT/LOGIT - TONE","\n")
###############################################################################

output_dir_reg9 <- paste(output_directory,"reg_tone_bin","\\",sep="")
create_directory(output_dir_reg9,remove=1)

data_year_groups9 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups9[1,] <- c(beg_year,end_year)

#dep_var9 <- unlist(dep_input_bin[,"dep_var"])
dep_var9 <- unlist(dep_input_bin[grep("Revision",dep_input_bin[,"dep_var"]),"dep_var"])
#controls9 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin + Revision_DV + Sharpe_Ratio "
#controls9 <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
#controls9 <- "age_y + AUM + Domicile_onshore_bin + Flagship_bin + Lockup_bin + Leverage_bin"
controls9 <- "age_y_log + AUM_USm_log + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls9 <- "age_y + AUM + Lockup_bin + Hurdle_Rate_bin + Domicile_onshore_bin"
#controls9 <- "age_y + AUM + Lockup_bin"
model_type9 <- "pooling"
note9 <- "tone"
#sim_type9 <- c("900pct")
#sim_type9 <- c("050pct","900pct")
#sim_type9 <- c("050pct","750pct","900pct")
#sim_type9 <- c("050pct","500pct","750pct","900pct")
sim_type9 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
family9 <- "logit"
#family9 <- "probit"

regression_equations9 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
# regression_equations9[1,] <- c("per_litigious",NA,controls9,NA,NA,NA)
# regression_equations9[2,] <- c("per_modalstrong",NA,controls9,NA,NA,NA)
# regression_equations9[3,] <- c("per_modalweak",NA,controls9,NA,NA,NA)
# regression_equations9[4,] <- c("per_negative",NA,controls9,NA,NA,NA)
# regression_equations9[5,] <- c("per_positive",NA,controls9,NA,NA,NA)
# regression_equations9[6,] <- c("per_uncertainty",NA,controls9,NA,NA,NA)
# regression_equations9[7,] <- c("per_litigious + per_modalstrong + per_modalweak + per_negative + per_positive + per_uncertainty",NA,controls9,NA,NA,NA)
# regression_equations9[8,] <- c("per_litigious + per_modalstrong + per_modalweak + per_negative + per_positive + per_uncertainty",NA,controls9,NA,strat_dvs,NA)

regression_equations9[1,] <- c("per_litigious",NA,NA,NA,NA,NA)
regression_equations9[2,] <- c("per_negative",NA,NA,NA,NA,NA)
regression_equations9[3,] <- c("per_positive",NA,NA,NA,NA,NA)
regression_equations9[4,] <- c("per_uncertainty",NA,NA,NA,NA,NA)
regression_equations9[5,] <- c("per_litigious",NA,controls9,NA,NA,NA)
regression_equations9[6,] <- c("per_negative",NA,controls9,NA,NA,NA)
regression_equations9[7,] <- c("per_positive",NA,controls9,NA,NA,NA)
regression_equations9[8,] <- c("per_uncertainty",NA,controls9,NA,NA,NA)
regression_equations9[9,] <- c("per_litigious + per_negative + per_positive + per_uncertainty",NA,controls9,NA,NA,NA)
regression_equations9[10,] <- c("per_litigious + per_negative + per_positive + per_uncertainty",NA,controls9,NA,strat_dvs,NA)

regression_equations9 <- unknown_to_NA(regression_equations9,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations9))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations9[i,1:(ncol(regression_equations9)-1)],use.names=F))))
  regression_equations9[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}
# for (i in 1:ncol(regression_equations9))
# {
#   regression_equations9[i,] <-  gsub("PCT","90",regression_equations9[i,])
#   regression_equations9[i,] <-  gsub("ANYAVG","any",regression_equations9[i,])
#   regression_equations9[i,] <-  gsub("CUTOFF","024",regression_equations9[i,])
# }

for (k in 1:nrow(data_year_groups9))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups9[k,1],"END YEAR:",data_year_groups9[k,2],"\n")
  
  data_temp <- data_reg[(data_reg[,"yr"]>=data_year_groups9[k,1] & data_reg[,"yr"]<=data_year_groups9[k,2]),]
  
  for (i in 1:length(dep_var9))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type9))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var9[i],data_year_groups9[k,1],data_year_groups9[k,2],note9,sim_type9[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations9) )
      se <- rep(list(list()),nrow(regression_equations9))
      pval <- rep(list(list()),nrow(regression_equations9))
      
      for (l in 1:nrow(regression_equations9))
      {
        # l <- 1
        
        ind_vars_reg0 <- regression_equations9[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type9[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- glm(as.formula(paste(dep_var9[i],ind_vars_reg0,sep="~")),family=binomial(link=family9),data=data_temp)
        reg0_rse <- coeftest(reg0)
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var9[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type9)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        #se[[l]] <- reg0_rse[,2]
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations9)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations9)),")",sep=""),override.se=se,override.pval=pval,stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of ???? on Hedge Fund Scores – Multivariate",file=paste(output_dir_reg9,out_file_name,".doc",sep=""))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations9)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,i)
}
rm2(output_dir_reg9,data_year_groups9,dep_var9,model_type9,note9,sim_type9,controls9,family9,regression_equations9,temp_char_vec,k)
