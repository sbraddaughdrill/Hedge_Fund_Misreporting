# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Univariate.R
# Version: 1.0
# Date: 09.23.2014
# Purpose: Run Univariate Analysis
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

#source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=FALSE)
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
external_packages <- c("data.table","plyr","gdata")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm2(repo,external_packages,installed_packages)


###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "Fund_ID"

start_year <- 1994
#start_year <- 2007
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

#data_all0 <- data_all0[(data_all0[,"yr"]>=start_year & data_all0[,"yr"]<=end_year),]
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
cat("WINSORIZE", "\n")
###############################################################################

winsorize_vars <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                    "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                    "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                    "all_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios",
                    "all_similarity_250pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
                    "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",
                    "all_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios",
                    "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")

data_all <- data_all0
# for (i in 1:length(winsorize_vars))
# {
#   #i <- 1
#   #i <- 2
#   data_all[,winsorize_vars[i]] <- winsorize_both(data_all[,winsorize_vars[i]],q=0.025)
#   
# }
# rm(i)

rm2(data_all0,winsorize_vars)


###############################################################################
cat("UNIVARIATE ANALYSIS - VARIABLES", "\n")
###############################################################################

### Dep Vars (Text Vars)

univariate_vars_dep <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                         "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                         "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                         "all_similarity_100pct_ios","Primary_Investment_Strategy_combcol_similarity_100pct_ios",
                         "all_similarity_250pct_ios","Primary_Investment_Strategy_combcol_similarity_250pct_ios",
                         "all_similarity_500pct_ios","Primary_Investment_Strategy_combcol_similarity_500pct_ios",
                         "all_similarity_750pct_ios","Primary_Investment_Strategy_combcol_similarity_750pct_ios",
                         "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios",
                         "per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")

### All pattern cols

pattern_cols_99 <- c("per_positive_percent_99","num_zero_percent_99","per_repeats_percent_99","uniform_percent_99",
                     "string_percent_99","num_pairs_percent_99","per_negative_percent_99","ar_1_percent_99","indexrsq_percent_99",
                     "kink_percent_99","quality_score_trim0_99","quality_score_trim1_99","quality_score_trim2_99")
pattern_cols_95 <- c("per_positive_percent_95","num_zero_percent_95","per_repeats_percent_95","uniform_percent_95",
                     "string_percent_95","num_pairs_percent_95","per_negative_percent_95","ar_1_percent_95","indexrsq_percent_95",
                     "kink_percent_95","quality_score_trim0_95","quality_score_trim1_95","quality_score_trim2_95")
pattern_cols_90 <- c("per_positive_percent_90","num_zero_percent_90","per_repeats_percent_90","uniform_percent_90",
                     "string_percent_90","num_pairs_percent_90","per_negative_percent_90","ar_1_percent_90","indexrsq_percent_90",
                     "kink_percent_90","quality_score_trim0_90","quality_score_trim1_90","quality_score_trim2_90")

pattern_cols_trim0 <- c(pattern_cols_99,pattern_cols_95,pattern_cols_90)

pattern_cols_trim1 <- pattern_cols_trim0[!(pattern_cols_trim0 %in% c(pattern_cols_trim0[grep("per_positive",pattern_cols_trim0)],pattern_cols_trim0[grep("per_repeats",pattern_cols_trim0)]))]
#pattern_cols_trim1 <- pattern_cols_trim0[!(pattern_cols_trim0 %in% c(pattern_cols_trim0[grep("per_positive",pattern_cols_trim0)]))]

pattern_cols_trim2 <- pattern_cols_trim1[!(pattern_cols_trim1 %in% c(pattern_cols_trim1[grep("trim0",pattern_cols_trim1)],pattern_cols_trim1[grep("trim1",pattern_cols_trim1)]))]
#pattern_cols_trim2 <- pattern_cols_trim1[!(pattern_cols_trim1 %in% c(pattern_cols_trim1[grep("trim0",pattern_cols_trim1)],pattern_cols_trim1[grep("trim2",pattern_cols_trim1)]))]

pattern_cols <- pattern_cols_trim2[grep("_90", pattern_cols_trim2)] 

### Continuous Vars

univariate_vars_continuous_fund <- c("pflow","sdpct_flow_lag1",
                                     "mktadjret",
                                     "mktadjret_sq",
                                     "age_y","total_fee","Sharpe_Ratio","Sortino_Ratio")


#"pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#"mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#"mktadjret_sq_lag1","mktadjret_sq_lag2","mktadjret_sq_lag3","mktadjret_sq_lag4",
#"AUM_log_lag1","AUM_log_lag2","AUM_log_lag3","AUM_log_lag4",

univariate_vars_continuous_pattern <- pattern_cols[(pattern_cols %in% pattern_cols[grep("quality_score",pattern_cols)])]

#univariate_vars_continuous <- c(univariate_vars_continuous_fund,univariate_vars_continuous_pattern)
univariate_vars_continuous <- c(univariate_vars_continuous_fund)


### Binary Vars

univariate_vars_binary_fund <- c("Listed_on_Exchange_bin","Hurdle_Rate_bin","Domicile_onshore_bin","Leverage_bin","Lockup_bin",
                                 "Flagship_bin","Closed_bin","Dead_bin")
#"high_water_mark_bin"

univariate_vars_binary_pattern <- pattern_cols[!(pattern_cols %in% pattern_cols[grep("quality_score",pattern_cols)])]

univariate_vars_binary <- c(univariate_vars_binary_fund,univariate_vars_binary_pattern)

rm2(pattern_cols_99,pattern_cols_95,pattern_cols_90)
rm2(pattern_cols_trim0,pattern_cols_trim1,pattern_cols_trim2)


###############################################################################
cat("UNIVARIATE ANALYSIS - CONTINUOUS", "\n")
###############################################################################

output_directory_univariate_continuous <- paste(output_directory,"univariate_continuous","\\",sep="")
create_directory(output_directory_univariate_continuous,remove=1)


data_all_univariate_continuous <- data_all[,c("yr","yr_month",univariate_vars_dep,univariate_vars_continuous)]

univariate_continuous_quantiles <- 4

univariate_continuous_parameters <- data.frame(matrix(NA,ncol=7,nrow=2,dimnames=list(c(),c("output_dir","note","data","vars","group_var","nums","type"))),stringsAsFactors=FALSE)
univariate_continuous_parameters[1,] <- c(output_directory_univariate_continuous,"continuous","data_all_univariate_continuous","XXX","yr_month",univariate_continuous_quantiles,"year")
univariate_continuous_parameters[2,] <- c(output_directory_univariate_continuous,"continuous","data_all_univariate_continuous","XXX","yr_month",univariate_continuous_quantiles,"agg")

univariate_continuous_year_groups <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=FALSE)
#univariate_continuous_year_groups[1,] <- c(start_year,end_year)
univariate_continuous_year_groups[1,] <- c(2007,end_year)

a_ply(.data=univariate_continuous_parameters,.margins=1,.fun = function(x,vars_dep,vars_indep,identifier,year_groups){
  
  # x <- univariate_continuous_parameters[1,]
  # x <- univariate_continuous_parameters[2,]
  # vars_dep <- univariate_vars_dep
  # vars_indep <- univariate_vars_continuous
  # identifier <- identifier
  # year_groups <- univariate_continuous_year_groups
  
  l_ply(.data=vars_dep, .fun = function(y,dep_vars_all,x,vars_indep,identifier,year_groups){
    
    # y <- vars_dep[[1]]
    # dep_vars_all <- vars_dep
    
    dep_var <- unlist(y)
    cat("DEP VAR:",dep_var, "\n")
    
    data <- get(x=unlist(x$data), envir = globalenv())
    group_var <- x$group_var
    
    univariate_vars_indep_continuous <- colnames(data)[!(colnames(data) %in% c("yr",group_var,y))]
    
    quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
    
    quantiles_pct_flow  <- univariate_bins_continuous(data=data,dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var)
    
    quantile_first_col <- "X1"
    quantile_last_col <- paste("X",x$nums,sep="")
    
    # Differences by group
    range_str <- "yearly"
    
    quantiles_pct_flow_diff_overall <- univariate_bins_diff(bins=quantiles_pct_flow,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                            dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var)
    
    # Differences by year groups and group 
    
    a_ply(.data=year_groups, .margins=1, .fun = function(w,group_var,bins,quantile_first_col,quantile_last_col,dep_var,dep_vars_all,vars_indep,parameters,quantile_var){
      
      # w <- year_groups[1,]
      # group_var <- group_var
      # bins <- quantiles_pct_flow
      # parameters <- x
      
      Start_yr <- w$Start_yr
      End_yr <- w$End_yr
      
      range_str <- paste(Start_yr,End_yr,sep="_")
      
      if (group_var == "yr") {
        
        bins_trim <- bins[(bins[,group_var]>=Start_yr & bins[,group_var]<=End_yr),]
        
      } else if (group_var == "yr_month") {
        
        bins_trim <- bins[(as.integer(substr(as.character(bins[,group_var]),1,4))>=Start_yr & as.integer(substr(as.character(bins[,group_var]),1,4))<=End_yr),]
        
      } else {
        cat("ERROR IN GROUPS", "\n")
      }
      bins_trim[,group_var] <- 9999
      
      quantiles_pct_flow_diff_group  <- univariate_bins_diff(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                             dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var)
      
    }, group_var=group_var,bins=quantiles_pct_flow,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_continuous,parameters=x,quantile_var=quantile_var, 
    .expand = TRUE, .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
    
    rm(dep_var,data,group_var,univariate_vars_indep_continuous,quantile_var,quantiles_pct_flow)
    rm(quantile_first_col,quantile_last_col,range_str,quantiles_pct_flow_diff_overall)
    
  }, x=x, dep_vars_all=vars_dep,vars_indep=vars_indep,identifier=identifier, year_groups=year_groups,
  .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
  
},vars_dep=univariate_vars_dep,vars_indep=univariate_vars_continuous,identifier=identifier,year_groups=univariate_continuous_year_groups,.progress = "none")

rm2(output_directory_univariate_continuous,univariate_continuous_quantiles,univariate_continuous_parameters)


###############################################################################
cat("UNIVARIATE ANALYSIS - BINARY", "\n")
###############################################################################

output_directory_univariate_binary <- paste(output_directory,"univariate_binary","\\",sep="")
create_directory(output_directory_univariate_binary,remove=1)


data_all_univariate_binary <- data_all[,c("yr","yr_month",univariate_vars_dep,univariate_vars_binary)]

univariate_binary_quantiles <- 2

univariate_binary_parameters <- data.frame(matrix(NA,ncol=7,nrow=2,dimnames=list(c(),c("output_dir","note","data","vars","group_var","nums","type"))),stringsAsFactors=FALSE)
univariate_binary_parameters[1,] <- c(output_directory_univariate_binary,"binary","data_all_univariate_binary","XXX","yr_month",univariate_binary_quantiles,"year")
univariate_binary_parameters[2,] <- c(output_directory_univariate_binary,"binary","data_all_univariate_binary","XXX","yr_month",univariate_binary_quantiles,"agg")

univariate_binary_year_groups <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=FALSE)
#univariate_binary_year_groups[1,] <- c(start_year,end_year)
univariate_binary_year_groups[1,] <- c(2007,end_year)

a_ply(.data=univariate_binary_parameters,.margins=1,.fun = function(x,vars_dep,vars_indep,identifier,year_groups){
  
  # x <- univariate_binary_parameters[1,]
  # x <- univariate_binary_parameters[2,]
  # vars_dep <- univariate_vars_dep
  # vars_indep <- univariate_vars_binary
  # identifier <- identifier
  # year_groups <- univariate_binary_year_groups
  
  l_ply(.data=vars_dep, .fun = function(y,dep_vars_all,x,vars_indep,identifier,year_groups){
    
    # y <- vars_dep[[1]]
    # dep_vars_all <- vars_dep
    
    dep_var <- unlist(y)
    cat("DEP VAR:",dep_var, "\n")
    
    data <- get(x=unlist(x$data), envir = globalenv())
    group_var <- x$group_var
    
    univariate_vars_indep_binary <- colnames(data)[!(colnames(data) %in% c("yr",group_var,dep_vars_all))]
    
    quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
    
    quantiles_pct_flow  <- univariate_bins_binary(data=data,dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_binary,parameters=x,quantile_var=quantile_var)
    
    quantile_first_col <- "X0"
    quantile_last_col <- paste("X",(as.integer(x$nums)-1),sep="")
    
    # Differences by group
    range_str <- "yearly"
    
    quantiles_pct_flow_diff_overall <- univariate_bins_diff(bins=quantiles_pct_flow,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                            dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_binary,parameters=x,quantile_var=quantile_var)
    
    # Differences by year groups and group 
    
    a_ply(.data=year_groups, .margins=1, .fun = function(w,group_var,bins,quantile_first_col,quantile_last_col,dep_var,dep_vars_all,vars_indep,parameters,quantile_var){
      
      # w <- year_groups[1,]
      # group_var <- group_var
      # bins <- quantiles_pct_flow
      # parameters <- x
      
      Start_yr <- w$Start_yr
      End_yr <- w$End_yr
      
      range_str <- paste(Start_yr,End_yr,sep="_")
      
      if (group_var == "yr") {
        
        bins_trim <- bins[(bins[,group_var]>=Start_yr & bins[,group_var]<=End_yr),]
        
      } else if (group_var == "yr_month") {
        
        bins_trim <- bins[(as.integer(substr(as.character(bins[,group_var]),1,4))>=Start_yr & as.integer(substr(as.character(bins[,group_var]),1,4))<=End_yr),]
        
      } else {
        cat("ERROR IN GROUPS", "\n")
      }
      bins_trim[,group_var] <- 9999
      
      quantiles_pct_flow_diff_group  <- univariate_bins_diff(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                             dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var)
      
    }, group_var=group_var,bins=quantiles_pct_flow,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_binary,parameters=x,quantile_var=quantile_var, 
    .expand = TRUE, .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
    
    
    rm(dep_var,data,group_var,univariate_vars_indep_binary,quantile_var,quantiles_pct_flow)
    rm(quantile_first_col,quantile_last_col,range_str,quantiles_pct_flow_diff_overall)
    
  }, x=x, dep_vars_all=vars_dep,vars_indep=vars_indep,identifier=identifier, year_groups=year_groups,
  .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
  
},vars_dep=univariate_vars_dep,vars_indep=univariate_vars_binary,identifier=identifier,year_groups=univariate_binary_year_groups,.progress = "none")

rm2(output_directory_univariate_binary,univariate_binary_quantiles,univariate_binary_parameters)


###############################################################################
cat("UNIVARIATE ANALYSIS - QUALITY SCORE", "\n")
###############################################################################

univariate_vars_manual <- univariate_vars_continuous_pattern

output_directory_univariate_manual <- paste(output_directory,"univariate_manual","\\",sep="")
create_directory(output_directory_univariate_manual,remove=1)


data_all_univariate_manual <- data_all[,c("yr","yr_month",univariate_vars_dep,univariate_vars_continuous_pattern)]

univariate_manual_quantiles <- 4

univariate_manual_parameters <- data.frame(matrix(NA,ncol=7,nrow=2,dimnames=list(c(),c("output_dir","note","data","vars","group_var","nums","type"))),stringsAsFactors=FALSE)
univariate_manual_parameters[1,] <- c(output_directory_univariate_manual,"manual","data_all_univariate_manual","XXX","yr_month",univariate_manual_quantiles,"year")
univariate_manual_parameters[2,] <- c(output_directory_univariate_manual,"manual","data_all_univariate_manual","XXX","yr_month",univariate_manual_quantiles,"agg")

univariate_manual_year_groups <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=FALSE)
#univariate_manual_year_groups[1,] <- c(start_year,end_year)
univariate_manual_year_groups[1,] <- c(2007,end_year)

#max(data_all[,"quality_score_trim2_90"])
#min(data_all[,"quality_score_trim2_90"])
#mean(data_all[,"quality_score_trim2_90"])
#median(data_all[,"quality_score_trim2_90"])
#t(quantile(data_all[,"quality_score_trim2_90"],c(0.01,0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95,0.99)))
#t(quantile(data_all[,"quality_score_trim2_90"],seq(.01,1,.01)))

univariate_manual_breaks <- data.frame(matrix(NA,ncol=3,nrow=length(univariate_vars_manual)*univariate_manual_quantiles,dimnames=list(c(),c("indep_var","start_break","end_break"))),stringsAsFactors=FALSE)
univariate_manual_breaks[1,] <- c(univariate_vars_manual[1],0,1)
univariate_manual_breaks[2,] <- c(univariate_vars_manual[1],2,2)
univariate_manual_breaks[3,] <- c(univariate_vars_manual[1],3,3)
univariate_manual_breaks[4,] <- c(univariate_vars_manual[1],4,7)


a_ply(.data=univariate_manual_parameters,.margins=1,.fun = function(x,vars_dep,vars_indep,identifier,year_groups,breaks){
  
  # x <- univariate_manual_parameters[1,]
  # x <- univariate_manual_parameters[2,]
  # vars_dep <- univariate_vars_dep
  # vars_indep <- univariate_vars_manual
  # identifier <- identifier
  # year_groups <- univariate_manual_year_groups
  # breaks <- univariate_manual_breaks
  
  l_ply(.data=vars_dep, .fun = function(y,dep_vars_all,x,vars_indep,identifier,year_groups,breaks){
    
    # y <- vars_dep[[1]]
    # dep_vars_all <- vars_dep
    
    dep_var <- unlist(y)
    cat("DEP VAR:",dep_var, "\n")
    
    data <- get(x=unlist(x$data), envir = globalenv())
    group_var <- x$group_var
    
    #univariate_vars_indep_manual <- colnames(data)[!(colnames(data) %in% c("yr",group_var,y))]
    univariate_vars_indep_manual <- colnames(data)[!(colnames(data) %in% c("yr",group_var,dep_vars_all))]
    
    quantile_var <- c("quantile_var_indep1","quantile_var_indep2")
    
    quantiles_pct_flow  <- univariate_bins_manual(data=data,dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_manual,parameters=x,quantile_var=quantile_var,breaks=breaks)
    
    quantile_first_col <- "X1"
    quantile_last_col <- paste("X",x$nums,sep="")
    
    # Differences by group
    range_str <- "yearly"
    
    quantiles_pct_flow_diff_overall <- univariate_bins_diff(bins=quantiles_pct_flow,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                            dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_manual,parameters=x,quantile_var=quantile_var)
    
    # Differences by year groups and group 
    
    a_ply(.data=year_groups, .margins=1, .fun = function(w,group_var,bins,quantile_first_col,quantile_last_col,dep_var,dep_vars_all,vars_indep,parameters,quantile_var){
      
      # w <- year_groups[1,]
      # group_var <- group_var
      # bins <- quantiles_pct_flow
      # parameters <- x
      
      Start_yr <- w$Start_yr
      End_yr <- w$End_yr
      
      range_str <- paste(Start_yr,End_yr,sep="_")
      
      if (group_var == "yr") {
        
        bins_trim <- bins[(bins[,group_var]>=Start_yr & bins[,group_var]<=End_yr),]
        
      } else if (group_var == "yr_month") {
        
        bins_trim <- bins[(as.integer(substr(as.character(bins[,group_var]),1,4))>=Start_yr & as.integer(substr(as.character(bins[,group_var]),1,4))<=End_yr),]
        
      } else {
        cat("ERROR IN GROUPS", "\n")
      }
      bins_trim[,group_var] <- 9999
      
      quantiles_pct_flow_diff_group  <- univariate_bins_diff(bins=bins_trim,range_str=range_str,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
                                                             dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=vars_indep,parameters=parameters,quantile_var=quantile_var)
      
    }, group_var=group_var,bins=quantiles_pct_flow,quantile_first_col=quantile_first_col,quantile_last_col=quantile_last_col,
    dep_var=dep_var,dep_vars_all=dep_vars_all,vars_indep=univariate_vars_indep_manual,parameters=x,quantile_var=quantile_var, 
    .expand = TRUE, .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
    
    rm(dep_var,data,group_var,univariate_vars_indep_manual,quantile_var,quantiles_pct_flow)
    rm(quantile_first_col,quantile_last_col,range_str,quantiles_pct_flow_diff_overall)
    
  }, x=x, dep_vars_all=vars_dep,vars_indep=vars_indep,identifier=identifier, year_groups=year_groups,breaks=breaks,
  .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL)
  
},vars_dep=univariate_vars_dep,vars_indep=univariate_vars_manual,identifier=identifier,year_groups=univariate_manual_year_groups,breaks=univariate_manual_breaks,.progress = "none")

rm2(output_directory_univariate_manual,univariate_manual_quantiles,univariate_manual_parameters)



