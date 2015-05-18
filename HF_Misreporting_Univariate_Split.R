# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Univariate_Split.R
# Version: 1.0
# Date: 09.23.2014
# Purpose: Run Univariate Analysis
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

# String as factors is FALSE -- used for read.csv
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


if (Location == 1) {
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("F:/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R/",winslash = "\\",mustWork=T)
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\",mustWork=T)
  output_directory <- normalizePath("C:/Research_temp4/",winslash="\\",mustWork=T)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\",mustWork=T) 
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("//tsclient/F/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash = "\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash = "\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash = "\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R/",winslash = "\\",mustWork=T)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash = "\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash = "\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash = "\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash = "\\",mustWork=T)
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash = "\\",mustWork=T)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/",winslash = "\\",mustWork=T)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/",winslash = "\\",mustWork=T)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/",winslash = "\\",mustWork=T)
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES","\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS","\n")
###############################################################################

#source(file=paste(function_directory,"functions_db.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=F)
#source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=F)
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
external_packages <- c("data.table","plyr","gdata")
invisible(unlist(sapply(external_packages,load_external_packages,repo_str=repo,simplify=F,USE.NAMES=F)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("IMPORT DATA","\n")
###############################################################################

identifier <- "Fund_ID"

start_year <- 1994
#start_year <- 2007
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
cat("UNIVARIATE ANALYSIS - VARIABLES","\n")
###############################################################################

pattern_cols_sub <- c("per_positive_percent","num_zero_percent","per_repeats_percent","uniform_percent",
                      "string_percent","num_pairs_percent","per_negative_percent","ar_1_percent","indexrsq_percent",
                      "kink_percent","quality_score_trim0","quality_score_trim1","quality_score_trim2")

pattern_cols_all <- unique(unlist(lapply(pattern_cols_sub,function(x,cols){cols[grep(x,cols)]},cols=colnames(data_all))))


### All pattern cols

# pattern_cols_99 <- c("per_positive_percent_99","num_zero_percent_99","per_repeats_percent_99","uniform_percent_99",
#                      "string_percent_99","num_pairs_percent_99","per_negative_percent_99","ar_1_percent_99","indexrsq_percent_99",
#                      "kink_percent_99","quality_score_trim0_99","quality_score_trim1_99","quality_score_trim2_99")
# pattern_cols_99_any <- paste(pattern_cols_99,"any",sep="_")
# pattern_cols_99_avg <- paste(pattern_cols_99,"avg",sep="_")
pattern_cols_99 <- pattern_cols_all[grep("_99_",pattern_cols_all)]
pattern_cols_99_any <- pattern_cols_99[grep("_any_",pattern_cols_99)]
pattern_cols_99_avg <- pattern_cols_99[grep("_avg_",pattern_cols_99)]

# pattern_cols_95 <- c("per_positive_percent_95","num_zero_percent_95","per_repeats_percent_95","uniform_percent_95",
#                      "string_percent_95","num_pairs_percent_95","per_negative_percent_95","ar_1_percent_95","indexrsq_percent_95",
#                      "kink_percent_95","quality_score_trim0_95","quality_score_trim1_95","quality_score_trim2_95")
# pattern_cols_95_any <- paste(pattern_cols_95,"any",sep="_")
# pattern_cols_95_avg <- paste(pattern_cols_95,"avg",sep="_")
pattern_cols_95 <- pattern_cols_all[grep("_95_",pattern_cols_all)]
pattern_cols_95_any <- pattern_cols_95[grep("_any_",pattern_cols_95)]
pattern_cols_95_avg <- pattern_cols_95[grep("_avg_",pattern_cols_95)]

# pattern_cols_90 <- c("per_positive_percent_90","num_zero_percent_90","per_repeats_percent_90","uniform_percent_90",
#                      "string_percent_90","num_pairs_percent_90","per_negative_percent_90","ar_1_percent_90","indexrsq_percent_90",
#                      "kink_percent_90","quality_score_trim0_90","quality_score_trim1_90","quality_score_trim2_90")
# pattern_cols_90_any <- paste(pattern_cols_90,"any",sep="_")
# pattern_cols_90_avg <- paste(pattern_cols_90,"avg",sep="_")
pattern_cols_90 <- pattern_cols_all[grep("_90_",pattern_cols_all)]
pattern_cols_90_any <- pattern_cols_90[grep("_any_",pattern_cols_90)]
pattern_cols_90_avg <- pattern_cols_90[grep("_avg_",pattern_cols_90)]

pattern_cols_trim0 <- c(pattern_cols_99_any,pattern_cols_99_avg,pattern_cols_95_any,pattern_cols_95_avg,pattern_cols_90_any,pattern_cols_90_avg)

#pattern_cols_trim1 <- pattern_cols_trim0[!(pattern_cols_trim0 %in% c(pattern_cols_trim0[grep("per_positive",pattern_cols_trim0)],pattern_cols_trim0[grep("per_repeats",pattern_cols_trim0)]))]
#pattern_cols_trim2 <- pattern_cols_trim1[!(pattern_cols_trim1 %in% c(pattern_cols_trim1[grep("trim0",pattern_cols_trim1)],pattern_cols_trim1[grep("trim1",pattern_cols_trim1)]))]
#pattern_cols <- pattern_cols_trim2[grep("_90",pattern_cols_trim2)] 

pattern_cols_trim1 <- pattern_cols_trim0
pattern_cols_trim2 <- pattern_cols_trim1
pattern_cols <- pattern_cols_trim2

### Revision cols
revision_cols <- c("Revision_DV","Revision_1BP_DV","Revision_10BP_DV","Revision_50BP_DV","Revision_100BP_DV")


### Dep Vars (Text Vars)

#univariate_vars_dep <- pattern_cols[!(pattern_cols %in% pattern_cols[grep("quality_score",pattern_cols)])]
#univariate_vars_dep <- pattern_cols
univariate_vars_dep <- c(pattern_cols,revision_cols)


### Continuous Vars

univariate_vars_continuous_fund <- c("pflow","sd_lag1",
                                     "mktadjret",
                                     "mktadjret_sq",
                                     "age_y","total_fee","Sharpe_Ratio","Sortino_Ratio")

#"pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#"mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#"mktadjret_sq_lag1","mktadjret_sq_lag2","mktadjret_sq_lag3","mktadjret_sq_lag4",
#"AUM_log_lag1","AUM_log_lag2","AUM_log_lag3","AUM_log_lag4",

univariate_vars_continuous_text <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                                     "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                                     "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                                     "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")

#"all_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios",
#"all_similarity_250pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
#"all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",
#"all_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios",

univariate_vars_continuous_tone <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")

univariate_vars_continuous <- c(univariate_vars_continuous_fund,univariate_vars_continuous_text,univariate_vars_continuous_tone)

### Binary Vars

univariate_vars_binary_fund <- c("Listed_on_Exchange_bin","Hurdle_Rate_bin","Domicile_onshore_bin","Leverage_bin","Lockup_bin",
                                 "Flagship_bin","Closed_bin","Dead_bin")
#"high_water_mark_bin"

univariate_vars_binary_tone <- c("litigious_dv","modalstrong_dv","modalweak_dv","negative_dv","positive_dv","uncertainty_dv")

univariate_vars_binary <- c(univariate_vars_binary_fund,univariate_vars_binary_tone)

rm2(pattern_cols_99,pattern_cols_99_any,pattern_cols_99_avg)
rm2(pattern_cols_95,pattern_cols_95_any,pattern_cols_95_avg)
rm2(pattern_cols_90,pattern_cols_90_any,pattern_cols_90_avg)
rm2(pattern_cols_sub,pattern_cols_all)
rm2(pattern_cols_trim0,pattern_cols_trim1,pattern_cols_trim2)
rm2(pattern_cols,revision_cols)


###############################################################################
cat("FIND FIRST YEAR FOR EACH FUND","\n")
###############################################################################

data_all_univariate_lookup0 <- data_all[!is.na(data_all[,strat_col]),]

data_all_univariate_lookup1 <- unique(data_all_univariate_lookup0[,c(identifier,"yr")])
data_all_univariate_lookup1 <- data_all_univariate_lookup1[order(data_all_univariate_lookup1[,identifier],data_all_univariate_lookup1[,"yr"]),]
row.names(data_all_univariate_lookup1) <- seq(nrow(data_all_univariate_lookup1))

data_all_univariate_lookup <- data_all_univariate_lookup1

data_all_univariate_lookup <- as.data.table(data_all_univariate_lookup)
setkeyv(data_all_univariate_lookup,c(identifier,"yr"))
setorderv(data_all_univariate_lookup,c(identifier,"yr"),c(1,1))
data_all_univariate_lookup <- data_all_univariate_lookup[,.SD[c(1)],by=c(identifier)]
data_all_univariate_lookup <- as.data.frame(data_all_univariate_lookup,stringsAsFactors=F)

rm2(data_all_univariate_lookup0,data_all_univariate_lookup1)


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS (SETUP)","\n")
###############################################################################

#data_all_univariate_continuous_keep_trim <- unique(data_all[,c(identifier,"yr",univariate_vars_dep,univariate_vars_continuous)])
data_all_univariate_continuous_keep_trim <- unique(data_all[,c(identifier,"yr",univariate_vars_dep,univariate_vars_continuous_text,univariate_vars_continuous_tone)])

data_all_univariate_continuous1 <- merge(data_all_univariate_lookup,data_all_univariate_continuous_keep_trim,
                                         by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                                         all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
data_all_univariate_continuous1 <- data_all_univariate_continuous1[order(data_all_univariate_continuous1[,identifier],
                                                                         data_all_univariate_continuous1[,"yr"]),]
row.names(data_all_univariate_continuous1) <- seq(nrow(data_all_univariate_continuous1))

data_all_univariate_continuous2 <- as.data.table(data_all_univariate_continuous1)
setkeyv(data_all_univariate_continuous2,c(identifier,"yr"))
setorderv(data_all_univariate_continuous2,c(identifier,"yr"),c(1,1))
data_all_univariate_continuous2[,na_count := rowSums(is.na(.SD)),.SDcols=colnames(data_all_univariate_continuous2)]
setorderv(data_all_univariate_continuous2,c(identifier,"yr","na_count"),c(1,1,1))
setkeyv(data_all_univariate_continuous2,c(identifier,"yr"))
data_all_univariate_continuous2 <- data_all_univariate_continuous2[,.SD[c(1)],by=c(identifier,"yr")]
data_all_univariate_continuous2 <- as.data.frame(data_all_univariate_continuous2,stringsAsFactors=F)

# data_all_univariate_continuous <- data.frame(yr_month=NA,
#                                              data_all_univariate_continuous2[,c("yr",univariate_vars_dep,univariate_vars_continuous)],
#                                              stringsAsFactors=F)
data_all_univariate_continuous <- data.frame(yr_month=NA,
                                             data_all_univariate_continuous2[,c("yr",univariate_vars_dep,univariate_vars_continuous_text,univariate_vars_continuous_tone)],
                                             stringsAsFactors=F)
data_all_univariate_continuous[,"yr_month"] <- paste(data_all_univariate_continuous[,"yr"],"01",sep="_")

rm2(data_all_univariate_continuous_keep_trim,data_all_univariate_continuous1,data_all_univariate_continuous2)
rm2(data_all_univariate_lookup)


###############################################################################
cat("SPLIT DATA BY QUALITY SCORE MEDIAN","\n")
###############################################################################

data_all_univariate_continuous_split <- data.frame(data_all_univariate_continuous,qs_median=NA,qs_mean=NA,stringsAsFactors=F)

#data_all_univariate_continuous_split[,"qs_median"] <- cut(data_all_univariate_continuous_split[,"quality_score_trim1_90_any_024"],b=2)
data_all_univariate_continuous_split[,"qs_median"] <-  ifelse(data_all_univariate_continuous_split[,"quality_score_trim1_90_any_024"]>median(data_all_univariate_continuous_split[,"quality_score_trim1_90_any_024"],na.rm=F),1,0)
data_all_univariate_continuous_split[,"qs_mean"] <-  ifelse(data_all_univariate_continuous_split[,"quality_score_trim1_90_any_024"]>mean(data_all_univariate_continuous_split[,"quality_score_trim1_90_any_024"],na.rm=T),1,0)

data_all_univariate_continuous_median_h <- data_all_univariate_continuous_split[data_all_univariate_continuous_split[,"qs_median"]==1,]
data_all_univariate_continuous_median_l <- data_all_univariate_continuous_split[data_all_univariate_continuous_split[,"qs_median"]==0,]

data_all_univariate_continuous_mean_h <- data_all_univariate_continuous_split[data_all_univariate_continuous_split[,"qs_mean"]==1,]
data_all_univariate_continuous_mean_l <- data_all_univariate_continuous_split[data_all_univariate_continuous_split[,"qs_mean"]==0,]

#rm2(data_all_univariate_continuous,data_all_univariate_continuous_split)


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS - SETUP","\n")
###############################################################################

output_directory_univariate_continuous <- paste(output_directory,"univariate_split_continuous","\\",sep="")
create_directory(output_directory_univariate_continuous,remove=1)

univariate_continuous_quantiles <- 2

output_directory_univariate_continuous_merge <- paste(output_directory_univariate_continuous,"merge","\\",sep="")
create_directory(output_directory_univariate_continuous_merge,remove=1)

univariate_continuous_keep_var <- c("type","dep_var","cut_var","yr_month")
univariate_continuous_keep_quantile <- c("X1","X4")
univariate_continuous_keep_stat <- c("t_minus_b","t_stat","t_p_val_str")
univariate_continuous_keep_all <- c(univariate_continuous_keep_var,univariate_continuous_keep_quantile,univariate_continuous_keep_stat)

#univariate_continuous_type_year_agg <- c("year","agg")
#univariate_continuous_type_any_all <- c("any","avg")
#univariate_continuous_type_expand <- as.data.frame(expand.grid(univariate_continuous_type_year_agg,univariate_continuous_type_any_all,stringsAsFactors=F))
#colnames(univariate_continuous_type_expand) <- c("year_agg","any_all")                                                  

univariate_continuous_type_expand <- data.frame(year_agg=c("year","agg"),stringsAsFactors=F)


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS (EXECUTION) - HIGH","\n")
###############################################################################

#univariate_continuous_high_data <- "data_all_univariate_continuous_median_h"
univariate_continuous_high_data <- "data_all_univariate_continuous_mean_h"

univariate_continuous_high_parameters <- data.frame(matrix(NA,ncol=7,nrow=2,dimnames=list(c(),c("output_dir","note","data","vars","group_var","nums","type"))),stringsAsFactors=F)
univariate_continuous_high_parameters[1,] <- c(output_directory_univariate_continuous,"continuous_high",univariate_continuous_high_data,"XXX","yr_month",univariate_continuous_quantiles,"year")
univariate_continuous_high_parameters[2,] <- c(output_directory_univariate_continuous,"continuous_high",univariate_continuous_high_data,"XXX","yr_month",univariate_continuous_quantiles,"agg")

univariate_continuous_high_year_groups <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
#univariate_continuous_high_year_groups[1,] <- c(start_year,end_year)
univariate_continuous_high_year_groups[1,] <- c(2007,end_year)

univariate_continuous_high_diff <- alply(.data=univariate_continuous_high_parameters,.margins=1,.fun=function(x,vars_dep,vars_indep,identifier,year_groups,output_flag){
  
  # x <- univariate_continuous_high_parameters[1,]
  # x <- univariate_continuous_high_parameters[2,]
  # vars_dep <- univariate_vars_dep
  # vars_indep <- c(univariate_vars_continuous_text)
  # identifier <- identifier
  # year_groups <- univariate_continuous_high_year_groups
  # output_flag <- F
  
  var_temp <- vars_indep
  
  quantiles_diff_comb_temp0 <- llply(.data=var_temp,.fun=function(y,dep_vars_all,x,vars_indep,identifier,year_groups,output_flag){
    
    # y <- var_temp[[1]]
    # y <- var_temp[[2]]
    # dep_vars_all <- var_temp
    
    dep_var <- unlist(y)
    cat("DEP VAR:",dep_var,"\n")
    
    data <- get(x=unlist(x$data),envir=globalenv())
    group_var <- x$group_var
    
    #univariate_vars_indep_continuous <- colnames(data)[!(colnames(data) %in% c("yr",group_var,y))]
    #univariate_vars_indep_continuous <- colnames(data)[!(colnames(data) %in% c("yr",group_var,dep_vars_all))]
    univariate_vars_indep_continuous <- dep_var
    
    quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
    
    quantiles  <- univariate_bins_continuous(data=data,dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var)
    
    quantile_first_col <- "X1"
    quantile_last_col <- paste("X",x$nums,sep="")
    
    # Differences by group
    range_str <- "yearly"
    
    #quantiles_diff_yearly <- univariate_bins_diff(bins=quantiles,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    #                                              dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var,output_flag=output_flag)
    
    quantiles_diff_yearly <- univariate_bins_diff_split(bins=quantiles,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                        dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var,output_flag=output_flag)
    
    
    # Differences by year groups and group 
    
    quantiles_diff_group <- adply(.data=year_groups,.margins=1,.fun=function(w,group_var,bins,quantile_first_col,quantile_last_col,dep_var,dep_vars_all,vars_indep,parameters,quantile_var,output_flag){
      
      # w <- year_groups[1,]
      # group_var <- group_var
      # bins <- quantiles
      # parameters <- x
      
      Start_yr <- w$Start_yr
      End_yr <- w$End_yr
      
      range_str <- paste(Start_yr,End_yr,sep="_")
      
      if (group_var == "yr") {
        
        bins_trim <- bins[(bins[,group_var]>=Start_yr & bins[,group_var]<=End_yr),]
        
      } else if (group_var == "yr_month") {
        
        bins_trim <- bins[(as.integer(substr(as.character(bins[,group_var]),1,4))>=Start_yr & as.integer(substr(as.character(bins[,group_var]),1,4))<=End_yr),]
        
      } else {
        cat("ERROR IN GROUPS","\n")
      }
      bins_trim[,group_var] <- 9999
      
      #quantiles_diff_group  <- univariate_bins_diff(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
      #                                              dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var,output_flag=output_flag)
      
      
      quantiles_diff_group  <- univariate_bins_diff_split(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                          dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var,output_flag=output_flag)
      
      
      return(quantiles_diff_group)
      
    },group_var=group_var,bins=quantiles,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var,output_flag=output_flag,
    .expand=T,.progress="none")
    
    out <- list(data.frame(dep_var=dep_var,quantiles_diff_yearly,stringsAsFactors=F),
                data.frame(dep_var=dep_var,quantiles_diff_group,stringsAsFactors=F))
    
    rm(dep_var,quantiles_diff_yearly,quantiles_diff_group)
    rm(data,group_var,univariate_vars_indep_continuous,quantile_var,quantiles)
    rm(quantile_first_col,quantile_last_col,range_str)
    
    return(out)
    
  },x=x,dep_vars_all=vars_dep,vars_indep=vars_indep,identifier=identifier,year_groups=year_groups,output_flag=output_flag)
  
  
  quantiles_diff_yearly_temp <- ldply(.data=quantiles_diff_comb_temp0,.fun=function(z){return(z[[1]])})
  
  quantiles_diff_group_temp <- ldply(.data=quantiles_diff_comb_temp0,.fun=function(z){return(z[[2]])})
  
  out_comb <- list(data.frame(type=unlist(x$type),quantiles_diff_yearly_temp,stringsAsFactors=F),
                   data.frame(type=unlist(x$type),quantiles_diff_group_temp,stringsAsFactors=F))
  
  rm(quantiles_diff_comb_temp0,quantiles_diff_yearly_temp,quantiles_diff_group_temp)
  
  return(out_comb)
  
  
},vars_dep=univariate_vars_dep,vars_indep=c(univariate_vars_continuous_text),identifier=identifier,year_groups=univariate_continuous_high_year_groups,output_flag=F)


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS (OUTPUT) - HIGH","\n")
###############################################################################

## Yearly

univariate_continuous_high_diff_yearly <- ldply(.data=univariate_continuous_high_diff,.fun=function(z){return(z[[1]])})

a_ply(.data=univariate_continuous_type_expand,.margins=1,.fun=function(x,data,cols_keep,output_dir,note){
  
  # x <- univariate_continuous_type_expand[1,]
  # x <- as.data.frame(x,stringsAsFactors=F)
  # colnames(x) <- "year_agg"
  
  
  name_prefix_temp <- paste("quantiles",x[,"year_agg"],sep="_")
  name_suffix_temp <- paste(note,unique(data[,"nums"]),sep="_")
  
  #data_temp <- data[data[,"type"]==x[,"year_agg"],cols_keep]
  
  pivot_cols <- c("t_sig_freq","t_sig_dv","f_sig_freq","f_sig_dv")
  
  data_temp <- data.frame(data[data[,"type"]==x[,"year_agg"],],matrix(NA,ncol=length(pivot_cols),nrow=1,dimnames=list(c(),pivot_cols)),stringsAsFactors=F)
  
#   data_temp[,"pct"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-3+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"pct"] <- gsub("_","",data_temp[,"pct"])
#   data_temp[,"any_avg"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-7+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"any_avg"] <- substr(data_temp[,"any_avg"],0,3)
#   data_temp[,"cutoff"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-10+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"cutoff"] <- substr(data_temp[,"cutoff"],0,2)
#   data_temp[,"dep_var_trim"] <- substr(data_temp[,"dep_var"],0,nchar(data_temp[,"dep_var"])-11)
  
  data_temp[,"t_stat"] <- gsub("\\*","",data_temp[,"t_stat"]) 
  data_temp[,"t_stat"] <- gsub(pattern=" {2,}",replacement=" ",x=data_temp[,"t_stat"])
  #data_temp[,"t_stat"] <- gsub("^\\s+|\\s+$","",data_temp[,"t_stat"])
  data_temp[,"t_stat"] <- gsub("\\s+$","",data_temp[,"t_stat"])
  data_temp[,"t_stat"] <- gsub("\\s+$","",data_temp[,"t_stat"])
  
  data_temp[,"t_sig_freq"] <- nchar(data_temp[,"t_p_val_str"])-nchar(gsub("\\*","",data_temp[,"t_p_val_str"]))
  data_temp[,"t_sig_dv"] <- ifelse(data_temp[,"t_sig_freq"]>0,1,0)
  data_temp[,"f_sig_freq"] <- nchar(data_temp[,"f_p_val_str"])-nchar(gsub("\\*","",data_temp[,"f_p_val_str"]))
  data_temp[,"f_sig_dv"] <- ifelse(data_temp[,"f_sig_freq"]>0,1,0)

  write.csv(data_temp,file=paste(output_dir,name_prefix_temp,"_high_",name_suffix_temp,"_full",".csv",sep=""),na="",quote=T,row.names=F)
  
  write.csv(data_temp[,c(cols_keep[!(cols_keep %in% "t_stat")],pivot_cols)],file=paste(output_dir,name_prefix_temp,"_high_",name_suffix_temp,"_trim",".csv",sep=""),na="",quote=T,row.names=F)

},data=univariate_continuous_high_diff_yearly,cols_keep=univariate_continuous_keep_all,output_dir=output_directory_univariate_continuous_merge,note="yearly")

rm2(univariate_continuous_high_diff_yearly)


## Group

univariate_continuous_high_diff_group <- ldply(.data=univariate_continuous_high_diff,.fun=function(z){return(z[[2]])})

a_ply(.data=univariate_continuous_type_expand,.margins=1,.fun=function(x,data,cols_keep,output_dir,note){
  
  # x <- data.frame(year_agg=univariate_continuous_type_expand[1,])
  
  name_prefix_temp <- paste("quantiles",x[,"year_agg"],sep="_")
  name_suffix_temp <- paste(note,unique(data[,"nums"]),sep="_")
  
  pivot_cols <- c("t_sig_freq","t_sig_dv","f_sig_freq","f_sig_dv")
  
  data_temp <- data.frame(data[data[,"type"]==x[,"year_agg"],],matrix(NA,ncol=length(pivot_cols),nrow=1,dimnames=list(c(),pivot_cols)),stringsAsFactors=F)
  
#   data_temp[,"pct"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-3+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"pct"] <- gsub("_","",data_temp[,"pct"])
#   data_temp[,"any_avg"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-7+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"any_avg"] <- substr(data_temp[,"any_avg"],0,3)
#   data_temp[,"cutoff"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-10+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"cutoff"] <- substr(data_temp[,"cutoff"],0,2)
#   data_temp[,"dep_var_trim"] <- substr(data_temp[,"dep_var"],0,nchar(data_temp[,"dep_var"])-11)
  
  data_temp[,"t_stat"] <- gsub("\\*","",data_temp[,"t_stat"]) 
  data_temp[,"t_stat"] <- gsub(pattern=" {2,}",replacement=" ",x=data_temp[,"t_stat"])
  #data_temp[,"t_stat"] <- gsub("^\\s+|\\s+$","",data_temp[,"t_stat"])
  data_temp[,"t_stat"] <- gsub("\\s+$","",data_temp[,"t_stat"])
  data_temp[,"t_stat"] <- gsub("\\s+$","",data_temp[,"t_stat"])
  
  data_temp[,"t_sig_freq"] <- nchar(data_temp[,"t_p_val_str"])-nchar(gsub("\\*","",data_temp[,"t_p_val_str"]))
  data_temp[,"t_sig_dv"] <- ifelse(data_temp[,"t_sig_freq"]>0,1,0)
  data_temp[,"f_sig_freq"] <- nchar(data_temp[,"f_p_val_str"])-nchar(gsub("\\*","",data_temp[,"f_p_val_str"]))
  data_temp[,"f_sig_dv"] <- ifelse(data_temp[,"f_sig_freq"]>0,1,0)
  
  write.csv(data_temp,file=paste(output_dir,name_prefix_temp,"_high_",name_suffix_temp,"_full",".csv",sep=""),na="",quote=T,row.names=F)
  
  write.csv(data_temp[,c(cols_keep[!(cols_keep %in% "t_stat")],pivot_cols)],file=paste(output_dir,name_prefix_temp,"_high_",name_suffix_temp,"_trim",".csv",sep=""),na="",quote=T,row.names=F)
  
},data=univariate_continuous_high_diff_group,cols_keep=univariate_continuous_keep_all,output_dir=output_directory_univariate_continuous_merge,note="group")

rm2(univariate_continuous_high_diff_group)

rm2(univariate_continuous_high_diff,univariate_continuous_high_parameters,univariate_continuous_high_year_groups)
rm2(data_all_univariate_continuous_median_h,data_all_univariate_continuous_mean_h)


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS (EXECUTION) - LOW","\n")
###############################################################################

#univariate_continuous_low_data <- "data_all_univariate_continuous_median_l"
univariate_continuous_low_data <- "data_all_univariate_continuous_mean_l"

univariate_continuous_low_parameters <- data.frame(matrix(NA,ncol=7,nrow=2,dimnames=list(c(),c("output_dir","note","data","vars","group_var","nums","type"))),stringsAsFactors=F)
univariate_continuous_low_parameters[1,] <- c(output_directory_univariate_continuous,"continuous_low",univariate_continuous_low_data,"XXX","yr_month",univariate_continuous_quantiles,"year")
univariate_continuous_low_parameters[2,] <- c(output_directory_univariate_continuous,"continuous_low",univariate_continuous_low_data,"XXX","yr_month",univariate_continuous_quantiles,"agg")

univariate_continuous_low_year_groups <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
#univariate_continuous_low_year_groups[1,] <- c(start_year,end_year)
univariate_continuous_low_year_groups[1,] <- c(2007,end_year)

univariate_continuous_low_diff <- alply(.data=univariate_continuous_low_parameters,.margins=1,.fun=function(x,vars_dep,vars_indep,identifier,year_groups,output_flag){
  
  # x <- univariate_continuous_low_parameters[1,]
  # x <- univariate_continuous_low_parameters[2,]
  # vars_dep <- univariate_vars_dep
  # vars_indep <- c(univariate_vars_continuous_text)
  # identifier <- identifier
  # year_groups <- univariate_continuous_low_year_groups
  # output_flag <- F
  
  var_temp <- vars_indep
  
  quantiles_diff_comb_temp0 <- llply(.data=var_temp,.fun=function(y,dep_vars_all,x,vars_indep,identifier,year_groups,output_flag){
    
    # y <- var_temp[[1]]
    # y <- var_temp[[2]]
    # dep_vars_all <- var_temp
    
    dep_var <- unlist(y)
    cat("DEP VAR:",dep_var,"\n")
    
    data <- get(x=unlist(x$data),envir=globalenv())
    group_var <- x$group_var
    
    #univariate_vars_indep_continuous <- colnames(data)[!(colnames(data) %in% c("yr",group_var,y))]
    #univariate_vars_indep_continuous <- colnames(data)[!(colnames(data) %in% c("yr",group_var,dep_vars_all))]
    univariate_vars_indep_continuous <- dep_var
    
    quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
    
    quantiles  <- univariate_bins_continuous(data=data,dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var)
    
    quantile_first_col <- "X1"
    quantile_last_col <- paste("X",x$nums,sep="")
    
    # Differences by group
    range_str <- "yearly"
    
    #quantiles_diff_yearly <- univariate_bins_diff(bins=quantiles,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    #                                              dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var,output_flag=output_flag)
    
    quantiles_diff_yearly <- univariate_bins_diff_split(bins=quantiles,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                        dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var,output_flag=output_flag)
    
    
    # Differences by year groups and group 
    
    quantiles_diff_group <- adply(.data=year_groups,.margins=1,.fun=function(w,group_var,bins,quantile_first_col,quantile_last_col,dep_var,dep_vars_all,vars_indep,parameters,quantile_var,output_flag){
      
      # w <- year_groups[1,]
      # group_var <- group_var
      # bins <- quantiles
      # parameters <- x
      
      Start_yr <- w$Start_yr
      End_yr <- w$End_yr
      
      range_str <- paste(Start_yr,End_yr,sep="_")
      
      if (group_var == "yr") {
        
        bins_trim <- bins[(bins[,group_var]>=Start_yr & bins[,group_var]<=End_yr),]
        
      } else if (group_var == "yr_month") {
        
        bins_trim <- bins[(as.integer(substr(as.character(bins[,group_var]),1,4))>=Start_yr & as.integer(substr(as.character(bins[,group_var]),1,4))<=End_yr),]
        
      } else {
        cat("ERROR IN GROUPS","\n")
      }
      bins_trim[,group_var] <- 9999
      
      #quantiles_diff_group  <- univariate_bins_diff(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
      #                                              dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var,output_flag=output_flag)
      
      
      quantiles_diff_group  <- univariate_bins_diff_split(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                          dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var,output_flag=output_flag)
      
      
      return(quantiles_diff_group)
      
    },group_var=group_var,bins=quantiles,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var,output_flag=output_flag,
    .expand=T,.progress="none")
    
    out <- list(data.frame(dep_var=dep_var,quantiles_diff_yearly,stringsAsFactors=F),
                data.frame(dep_var=dep_var,quantiles_diff_group,stringsAsFactors=F))
    
    rm(dep_var,quantiles_diff_yearly,quantiles_diff_group)
    rm(data,group_var,univariate_vars_indep_continuous,quantile_var,quantiles)
    rm(quantile_first_col,quantile_last_col,range_str)
    
    return(out)
    
  },x=x,dep_vars_all=vars_dep,vars_indep=vars_indep,identifier=identifier,year_groups=year_groups,output_flag=output_flag)
  
  
  quantiles_diff_yearly_temp <- ldply(.data=quantiles_diff_comb_temp0,.fun=function(z){return(z[[1]])})
  
  quantiles_diff_group_temp <- ldply(.data=quantiles_diff_comb_temp0,.fun=function(z){return(z[[2]])})
  
  out_comb <- list(data.frame(type=unlist(x$type),quantiles_diff_yearly_temp,stringsAsFactors=F),
                   data.frame(type=unlist(x$type),quantiles_diff_group_temp,stringsAsFactors=F))
  
  rm(quantiles_diff_comb_temp0,quantiles_diff_yearly_temp,quantiles_diff_group_temp)
  
  return(out_comb)
  
  
},vars_dep=univariate_vars_dep,vars_indep=c(univariate_vars_continuous_text),identifier=identifier,year_groups=univariate_continuous_low_year_groups,output_flag=F)


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS (OUTPUT) - LOW","\n")
###############################################################################

## Yearly

univariate_continuous_low_diff_yearly <- ldply(.data=univariate_continuous_low_diff,.fun=function(z){return(z[[1]])})

a_ply(.data=univariate_continuous_type_expand,.margins=1,.fun=function(x,data,cols_keep,output_dir,note){
  
  # x <- univariate_continuous_type_expand[1,]
  
  name_prefix_temp <- paste("quantiles",x[,"year_agg"],sep="_")
  name_suffix_temp <- paste(note,unique(data[,"nums"]),sep="_")
  
  #data_temp <- data[data[,"type"]==x[,"year_agg"],cols_keep]
  
  pivot_cols <- c("t_sig_freq","t_sig_dv","f_sig_freq","f_sig_dv")
  
  data_temp <- data.frame(data[data[,"type"]==x[,"year_agg"],],matrix(NA,ncol=length(pivot_cols),nrow=1,dimnames=list(c(),pivot_cols)),stringsAsFactors=F)
  
#   data_temp[,"pct"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-3+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"pct"] <- gsub("_","",data_temp[,"pct"])
#   data_temp[,"any_avg"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-7+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"any_avg"] <- substr(data_temp[,"any_avg"],0,3)
#   data_temp[,"cutoff"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-10+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"cutoff"] <- substr(data_temp[,"cutoff"],0,2)
#   data_temp[,"dep_var_trim"] <- substr(data_temp[,"dep_var"],0,nchar(data_temp[,"dep_var"])-11)
  
  data_temp[,"t_stat"] <- gsub("\\*","",data_temp[,"t_stat"]) 
  data_temp[,"t_stat"] <- gsub(pattern=" {2,}",replacement=" ",x=data_temp[,"t_stat"])
  #data_temp[,"t_stat"] <- gsub("^\\s+|\\s+$","",data_temp[,"t_stat"])
  data_temp[,"t_stat"] <- gsub("\\s+$","",data_temp[,"t_stat"])
  data_temp[,"t_stat"] <- gsub("\\s+$","",data_temp[,"t_stat"])
  
  data_temp[,"t_sig_freq"] <- nchar(data_temp[,"t_p_val_str"])-nchar(gsub("\\*","",data_temp[,"t_p_val_str"]))
  data_temp[,"t_sig_dv"] <- ifelse(data_temp[,"t_sig_freq"]>0,1,0)
  data_temp[,"f_sig_freq"] <- nchar(data_temp[,"f_p_val_str"])-nchar(gsub("\\*","",data_temp[,"f_p_val_str"]))
  data_temp[,"f_sig_dv"] <- ifelse(data_temp[,"f_sig_freq"]>0,1,0)
  
  write.csv(data_temp,file=paste(output_dir,name_prefix_temp,"_low_",name_suffix_temp,"_full",".csv",sep=""),na="",quote=T,row.names=F)
  
  write.csv(data_temp[,c(cols_keep[!(cols_keep %in% "t_stat")],pivot_cols)],file=paste(output_dir,name_prefix_temp,"_low_",name_suffix_temp,"_trim",".csv",sep=""),na="",quote=T,row.names=F)
  
},data=univariate_continuous_low_diff_yearly,cols_keep=univariate_continuous_keep_all,output_dir=output_directory_univariate_continuous_merge,note="yearly")

rm2(univariate_continuous_low_diff_yearly)


## Group

univariate_continuous_low_diff_group <- ldply(.data=univariate_continuous_low_diff,.fun=function(z){return(z[[2]])})

a_ply(.data=univariate_continuous_type_expand,.margins=1,.fun=function(x,data,cols_keep,output_dir,note){
  
  # x <- data.frame(year_agg=univariate_continuous_type_expand[1,])
  
  name_prefix_temp <- paste("quantiles",x[,"year_agg"],sep="_")
  name_suffix_temp <- paste(note,unique(data[,"nums"]),sep="_")
  
  pivot_cols <- c("t_sig_freq","t_sig_dv","f_sig_freq","f_sig_dv")
  
  data_temp <- data.frame(data[data[,"type"]==x[,"year_agg"],],matrix(NA,ncol=length(pivot_cols),nrow=1,dimnames=list(c(),pivot_cols)),stringsAsFactors=F)
  
#   data_temp[,"pct"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-3+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"pct"] <- gsub("_","",data_temp[,"pct"])
#   data_temp[,"any_avg"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-7+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"any_avg"] <- substr(data_temp[,"any_avg"],0,3)
#   data_temp[,"cutoff"] <- substr(data_temp[,"dep_var"],nchar(data_temp[,"dep_var"])-10+1,nchar(data_temp[,"dep_var"]))
#   data_temp[,"cutoff"] <- substr(data_temp[,"cutoff"],0,2)
#   data_temp[,"dep_var_trim"] <- substr(data_temp[,"dep_var"],0,nchar(data_temp[,"dep_var"])-11)
  
  data_temp[,"t_stat"] <- gsub("\\*","",data_temp[,"t_stat"]) 
  data_temp[,"t_stat"] <- gsub(pattern=" {2,}",replacement=" ",x=data_temp[,"t_stat"])
  #data_temp[,"t_stat"] <- gsub("^\\s+|\\s+$","",data_temp[,"t_stat"])
  data_temp[,"t_stat"] <- gsub("\\s+$","",data_temp[,"t_stat"])
  data_temp[,"t_stat"] <- gsub("\\s+$","",data_temp[,"t_stat"])
  
  data_temp[,"t_sig_freq"] <- nchar(data_temp[,"t_p_val_str"])-nchar(gsub("\\*","",data_temp[,"t_p_val_str"]))
  data_temp[,"t_sig_dv"] <- ifelse(data_temp[,"t_sig_freq"]>0,1,0)
  data_temp[,"f_sig_freq"] <- nchar(data_temp[,"f_p_val_str"])-nchar(gsub("\\*","",data_temp[,"f_p_val_str"]))
  data_temp[,"f_sig_dv"] <- ifelse(data_temp[,"f_sig_freq"]>0,1,0)
  
  write.csv(data_temp,file=paste(output_dir,name_prefix_temp,"_low_",name_suffix_temp,"_full",".csv",sep=""),na="",quote=T,row.names=F)
  
  write.csv(data_temp[,c(cols_keep[!(cols_keep %in% "t_stat")],pivot_cols)],file=paste(output_dir,name_prefix_temp,"_low_",name_suffix_temp,"_trim",".csv",sep=""),na="",quote=T,row.names=F)
  
},data=univariate_continuous_low_diff_group,cols_keep=univariate_continuous_keep_all,output_dir=output_directory_univariate_continuous_merge,note="group")

rm2(univariate_continuous_low_diff_group)

rm2(univariate_continuous_low_diff,univariate_continuous_low_parameters,univariate_continuous_low_year_groups)
rm2(data_all_univariate_continuous_median_l,data_all_univariate_continuous_mean_l)







rm2(univariate_vars_continuous_fund,univariate_vars_continuous_text,univariate_vars_continuous_tone)
rm2(univariate_continuous_type_expand)
rm2(univariate_continuous_keep_var,univariate_continuous_keep_quantile,univariate_continuous_keep_stat,univariate_continuous_keep_all)
rm2(output_directory_univariate_continuous,output_directory_univariate_continuous_merge)
rm2(univariate_continuous_quantiles)

