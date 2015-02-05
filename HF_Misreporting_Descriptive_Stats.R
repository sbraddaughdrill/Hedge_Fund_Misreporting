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


descriptive_stats_execute <- function(x,vars,identifier,output_dir,stats_keep){
  
  # x <- descriptive_overall_groups_parameters[1,]
  # x <- descriptive_overall_groups_parameters[2,]
  # x <- descriptive_overall_groups_parameters[8,]
  # x <- descriptive_overall_groups_parameters[11,]
  # vars <- descriptive_overall_vars_model_vars_all
  # identifier <- identifier
  # output_dir <- output_directory_descrip_stats_overall
  # stats_keep <- c("n","mean","sd","min","quartile1","median","quartile3","max")
  
  #Start_yr <- unlist(x$Start_yr)
  #End_yr <- unlist(x$End_yr)
  #note <- unlist(x$note)
  #vars <- unlist(x$vars)
  
  #cat("START YEAR:",unlist(x$Start_yr),"END YEAR:",unlist(x$End_yr),"\n")
  
  data <- get(x=unlist(x$data), envir =  globalenv())
  model_vars <- get(x=unlist(x$vars), envir =  globalenv())
  
  descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,var=model_vars, stringsAsFactors=FALSE)
  descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
  
  out_file_name <- paste("descriptive_stats",unlist(x$Start_yr),unlist(x$End_yr),unlist(x$note),"overall",sep="_")
  
  data_temp <- data[(data[,"yr"]>=unlist(x$Start_yr) & data[,"yr"]<=unlist(x$End_yr)),]
  
  rm(data,model_vars)
  
  fund_count <- as.numeric(length(unique(data_temp[,identifier],comparables=FALSE)))
  
  data_temp_no_id1 <- data_temp[,!(colnames(data_temp) %in% identifier)]
  row.names(data_temp_no_id1) <- seq(nrow(data_temp_no_id1))
  
  vars_trim <- vars[vars[,"var"] %in% colnames(data_temp_no_id1),]
  vars_missing <- vars[!(vars[,"var"] %in% colnames(data_temp_no_id1)),]
  
  #descriptive_stats_temp_full_all_var <- describe2(data_temp_no_id1[,vars[,c("var")]])
  descriptive_stats_temp_full_all_var <- describe2(data_temp_no_id1[,vars_trim[,c("var")]])
  
  rm(data_temp_no_id1,vars_trim,vars_missing)
  
  descriptive_stats_temp_full <- descriptive_stats_temp_full_all_var[descriptive_stats_temp_full_all_var[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
  descriptive_stats_temp_full[,2:ncol(descriptive_stats_temp_full)] <- format(round(descriptive_stats_temp_full[,2:ncol(descriptive_stats_temp_full)],  digits = 8))
  descriptive_stats_temp_full <- rbind(descriptive_stats_temp_full,c("number_of_funds",fund_count,matrix("", ncol=(ncol(descriptive_stats_temp_full)-2), nrow=1)))
  row.names(descriptive_stats_temp_full) <- seq(nrow(descriptive_stats_temp_full))
  
  write.csv(descriptive_stats_temp_full,file=paste(output_dir,out_file_name,"_full.csv",sep=""),na="",quote=TRUE,row.names=FALSE)
  
  descriptive_stats_temp <- descriptive_stats_temp_full[c("var",stats_keep)]
  #descriptive_stats_temp <- descriptive_stats_temp[match(descriptive_overall_vars_model_vars_temp, descriptive_stats_temp[,c("var")]),]
  write.csv(descriptive_stats_temp,file=paste(output_dir,out_file_name,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
  
  #rm(Start_yr,End_yr,note,vars)
  rm(descriptive_overall_vars_model_vars_temp,out_file_name,descriptive_stats_temp_full,descriptive_stats_temp)
  rm(data_temp,fund_count,descriptive_stats_temp_full_all_var)
  
}


descriptive_stats_by_group_execute <- function(x,vars,identifier,output_dir,by_var,stat_types,col_order){
  
  # x <- descriptive_overall_groups_by_year_parameters[1,]
  # x <- descriptive_overall_groups_by_year_parameters[2,]
  # vars <- descriptive_overall_vars_model_vars_all
  # identifier <- identifier
  # output_dir <- output_directory_descrip_stats_by_year
  # by_var <- "yr"
  # stat_types <- c("mean","median")
  # col_order <- seq(start_year,end_year,1)
  
  #cat("START YEAR:",unlist(x$Start_yr),"END YEAR:",unlist(x$End_yr),"\n")
  
  require(plyr)
  
  data <- get(x=unlist(x$data), envir =  globalenv())
  model_vars <- get(x=unlist(x$vars), envir =  globalenv())
  
  descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,var=model_vars, stringsAsFactors=FALSE)
  descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
  
  out_file_name1 <- paste("descriptive_stats",unlist(x$Start_yr),unlist(x$End_yr),unlist(x$note),"overall",sep="_")
  
  data_temp <- data[(data[,"yr"]>=unlist(x$Start_yr) & data[,"yr"]<=unlist(x$End_yr)),]
  
  rm(data,model_vars)
  
  fund_count_group1 <- ddply(data_temp, by_var, 
                             function(x) {data.frame(var="number_of_funds", count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                     stringsAsFactors=FALSE)})
  
  fund_count_group2 <- data.frame(temp_var="ZZZ",var="number_of_funds", count=as.numeric(length(unique(data_temp[,identifier],comparables=FALSE))),
                                  stringsAsFactors=FALSE)
  colnames(fund_count_group2)[match("temp_var",names(fund_count_group2))] <- by_var
  
  fund_count_group <- rbind(fund_count_group1,fund_count_group2)
  
  rm(fund_count_group1,fund_count_group2)
  
  data_temp_no_id1 <- data_temp[,!(colnames(data_temp) %in% identifier)]
  row.names(data_temp_no_id1) <- seq(nrow(data_temp_no_id1))
  
  vars_trim <- vars[vars[,"var"] %in% colnames(data_temp_no_id1),]
  vars_missing <- vars[!(vars[,"var"] %in% colnames(data_temp_no_id1)),]
  
  #descriptive_stats_temp_full_all_var_year <- describeBy2(data_temp_no_id1[,c(by_var,vars[,c("var")])],by_var)
  descriptive_stats_temp_full_all_var_year <- describeBy2(data_temp_no_id1[,c(by_var,vars_trim[,c("var")])],by_var)
  
  #assign("descriptive_stats_temp_full_all_var_year", descriptive_stats_temp_full_all_var_year, envir = .GlobalEnv)
  
  rm(data_temp_no_id1,vars_trim,vars_missing)
  
  descriptive_stats_group_temp_full_trim <- descriptive_stats_temp_full_all_var_year[descriptive_stats_temp_full_all_var_year[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
  row.names(descriptive_stats_group_temp_full_trim) <- seq(nrow(descriptive_stats_group_temp_full_trim))
  
  rm(descriptive_stats_temp_full_all_var_year)
  
  l_ply(.data=stat_types, .fun = function(z,data_trim,by_var,model_vars_vec,count,out_name,col_order){
    
    # z <- stat_types[1]
    # data_trim <- descriptive_stats_group_temp_full_trim
    # by_var <- by_var
    # model_vars_vec <- descriptive_overall_vars_model_vars_temp
    # count <- fund_count_group
    # out_name <- paste(output_dir,out_file_name1,sep="")
    # col_order <- col_order
    
    require("reshape2")
    
    fund_count_group_temp <- count
    
    fund_count_group_temp[fund_count_group_temp[,by_var]=="ZZZ",by_var] <- "Overall"
    
    colnames(fund_count_group_temp)[match("count",names(fund_count_group_temp))] <- z
    
    out_file_name2 <- paste(out_name,z,sep="_")
    
    descriptive_stats_group_temp_trim <- rbind(data_trim[c(by_var,"var",z)],fund_count_group_temp)
    
    #descriptive_stats_group_temp <- suppressMessages(dcast(descriptive_stats_group_temp_trim, var~yr))
    descriptive_stats_group_temp <- suppressMessages(dcast(descriptive_stats_group_temp_trim, eval(parse(text=paste("var",by_var,sep="~")))))
    
    descriptive_stats_group_temp2 <- merge(descriptive_stats_group_temp, model_vars_vec, by.x=c("var"), by.y=c("var"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
    descriptive_stats_group_temp2 <- descriptive_stats_group_temp2[order(descriptive_stats_group_temp2[,"id"]),]
    row.names(descriptive_stats_group_temp2) <- seq(nrow(descriptive_stats_group_temp2))
    #colnames(descriptive_stats_group_temp2)[match("ZZZ",names(descriptive_stats_group_temp2))] <- "Full"
    
    descriptive_stats_group_temp2 <- descriptive_stats_group_temp2[,!(colnames(descriptive_stats_group_temp2) %in% c("id"))]
    row.names(descriptive_stats_group_temp2) <- seq(nrow(descriptive_stats_group_temp2))
    
    #Remove digits on number_of_funds
    descriptive_stats_group_temp3 <- descriptive_stats_group_temp2
    descriptive_stats_group_temp3[,2:ncol(descriptive_stats_group_temp3)] <- format(round(descriptive_stats_group_temp3[,2:ncol(descriptive_stats_group_temp3)],  digits = 8))
    
    for(i in which(descriptive_stats_group_temp3[,1]=="number_of_funds"))
    {
      descriptive_stats_group_temp3[i,2:ncol(descriptive_stats_group_temp3)] <- sprintf("%.0f",descriptive_stats_group_temp3[i,2:ncol(descriptive_stats_group_temp3)]) 
    }
    
    #descriptive_stats_group_temp3[,(2:ncol(descriptive_stats_group_temp3))] <- format(round(descriptive_stats_group_temp3[,(2:ncol(descriptive_stats_group_temp3))],  digits = 6))
    
    descriptive_stats_group_temp4 <- apply(descriptive_stats_group_temp3,2,function(x) {x <- ifelse(is.na(x),"", x)})
    descriptive_stats_group_temp4 <- as.data.frame(descriptive_stats_group_temp4,stringsAsFactors=FALSE)
    
    #descriptive_stats_group_temp5 <- descriptive_stats_strategy_temp4[,c(colnames(descriptive_stats_strategy_temp4[,!(colnames(descriptive_stats_strategy_temp4) %in% c("Others","Overall"))]),"Others","Overall")]
    descriptive_stats_group_temp5 <- descriptive_stats_group_temp4[,c("var",col_order,"Overall")]
    row.names(descriptive_stats_group_temp5) <- seq(nrow(descriptive_stats_group_temp5))
    
    write.csv(descriptive_stats_group_temp5,file=paste(out_file_name2,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    rm(fund_count_group_temp,out_file_name2,descriptive_stats_group_temp_trim)
    rm(descriptive_stats_group_temp,descriptive_stats_group_temp2,descriptive_stats_group_temp3)
    rm(descriptive_stats_group_temp4,descriptive_stats_group_temp5)
    
  }, data_trim=descriptive_stats_group_temp_full_trim, by_var=by_var,model_vars_vec=descriptive_overall_vars_model_vars_temp, 
  count=fund_count_group, out_name=paste(output_dir,out_file_name1,sep=""),col_order=col_order,
  .progress = "none", .inform = FALSE,.print = FALSE, .parallel = FALSE, .paropts = NULL)
  
  rm(descriptive_overall_vars_model_vars_temp,out_file_name1,descriptive_stats_group_temp_full_trim)
  rm(data_temp,fund_count_group)
  
}


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
# c("compare","cwhmisc","data.table","descr","fastmatch","formatR","gdata",
#   "gtools","Hmisc","installr","knitr","leaps","lmtest","markdown","memisc","mitools",
#   "pander","pbapply","PerformanceAnalytics","plm","psych","quantreg","R.oo","R2wd",
#   "reporttools","reshape2","rms","sandwich","sqldf","stargazer","stringr",
#   "texreg","taRifx","UsingR","xtable","zoo")
external_packages <- c("plyr","RSQLite")
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
#data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="")

###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "Fund_ID"

start_year <- 1994
end_year <- 2013

#descriptive_stats_tables <- ListTables(descriptive_stats_db)
#descriptive_stats_fields <- ListFields(descriptive_stats_db)

data_all0 <- read.csv(file=paste(output_directory,"data_all_tone",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

data_all0 <- data_all0[order(data_all0[,identifier],
                             data_all0[,"yr"],
                             data_all0[,"month"]),]
row.names(data_all0) <- seq(nrow(data_all0))


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
cat("DESCRIPTIVE STATISTICS - VARIABLES", "\n")
###############################################################################

### Create Monthly Level Data

descrip_stats_data_monthly0 <- data_all

descrip_stats_fund_vars_remove <- c("month","age_m","chgdt",
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

descrip_stats_data_monthly <- data.frame(descrip_stats_data_monthly0,year_group_id=NA,stringsAsFactors=FALSE)

descrip_stats_data_monthly[,"year_group_id"] <- ifelse((descrip_stats_data_monthly[,"yr"]>=1994 & descrip_stats_data_monthly[,"yr"]<=1999), 1, 
                                                       ifelse((descrip_stats_data_monthly[,"yr"]>=2000 & descrip_stats_data_monthly[,"yr"]<=2006), 2, 
                                                              ifelse((descrip_stats_data_monthly[,"yr"]>=2007 & descrip_stats_data_monthly[,"yr"]<=2013), 3, 
                                                                     descrip_stats_data_monthly[,"year_group_id"])))   

rm2(descrip_stats_data_monthly0)


### Create Yearly Level Data

monthly_level_cols <- c("date","yr_month","age_y","nflow","pflow",
                        "mktadjret","fund_ret_mkt_neg","vwretd","vwretd_annualized","vwretx","vwretx_annualized",
                        "mktadjret_sq","Monthly_Ret_sq","Monthly_Ret2_sq","Yearly_Ret2_sq",
                        "Monthly_Ret",unlist(lapply("Monthly_Ret",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                        "AUM",unlist(lapply("AUM",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)),
                        "AUM_log",unlist(lapply("AUM_log",function(x,lags){paste(x,"_lag",seq(1,lags),sep="")},lags=4)))

descrip_stats_data_yearly0 <-  descrip_stats_data_monthly[,!(colnames(descrip_stats_data_monthly) %in% monthly_level_cols)]
descrip_stats_data_yearly <- unique(descrip_stats_data_yearly0)
row.names(descrip_stats_data_yearly) <- seq(nrow(descrip_stats_data_yearly))

rm2(descrip_stats_data_yearly0)

### Create Aggregate Level Data
###
### NOT RELEVANT SINCE USING YEARLY HF DATA SNAPSHOTS!!! ####

descrip_stats_strategy_cols <- unique(descrip_stats_data_yearly[,"Primary_Investment_Strategy_combcol"])
descrip_stats_strategy_cols <- sort(descrip_stats_strategy_cols)
descrip_stats_strategy_cols <- c(descrip_stats_strategy_cols[!(descrip_stats_strategy_cols %in% c("Others"))],"Others")

descrip_stats_ios_read_cols <- c("sentences_ios","words_ios","chars_no_space_ios","num_syll_ios","sntc_per_word_ios",
                                 "avg_sentc_length_ios","avg_word_length_ios","avg_syll_word_ios","sntc_per100_ios",
                                 "syll_per100_ios","lett_per100_ios","fog_hard_words_ios",
                                 "ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                                 "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios")

descrip_stats_ios_sim_cols <- names(descrip_stats_data_yearly)[grep("pct_ios", names(descrip_stats_data_yearly))] 

descrip_stats_ios_tone_cols <- c("per_litigious", "per_modalstrong", "per_modalweak", "per_negative", "per_positive", "per_uncertainty")

descrip_stats_ios_quartile_cols <- c(names(descrip_stats_data_yearly)[grep("below_quartile1", names(descrip_stats_data_yearly))],
                                  names(descrip_stats_data_yearly)[grep("above_quartile3", names(descrip_stats_data_yearly))])

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

descrip_stats_pattern_cols_99 <- c("per_positive_percent_99","num_zero_percent_99","per_repeats_percent_99","uniform_percent_99",
                                   "string_percent_99","num_pairs_percent_99","per_negative_percent_99","ar_1_percent_99","indexrsq_percent_99",
                                   "kink_percent_99","quality_score_trim0_99","quality_score_trim1_99","quality_score_trim2_99")
descrip_stats_pattern_cols_95 <- c("per_positive_percent_95","num_zero_percent_95","per_repeats_percent_95","uniform_percent_95",
                                   "string_percent_95","num_pairs_percent_95","per_negative_percent_95","ar_1_percent_95","indexrsq_percent_95",
                                   "kink_percent_95","quality_score_trim0_95","quality_score_trim1_95","quality_score_trim2_95")
descrip_stats_pattern_cols_90 <- c("per_positive_percent_90","num_zero_percent_90","per_repeats_percent_90","uniform_percent_90",
                                   "string_percent_90","num_pairs_percent_90","per_negative_percent_90","ar_1_percent_90","indexrsq_percent_90",
                                   "kink_percent_90","quality_score_trim0_90","quality_score_trim1_90","quality_score_trim2_90")
descrip_stats_pattern_cols <- c(descrip_stats_pattern_cols_99,descrip_stats_pattern_cols_95,descrip_stats_pattern_cols_90)
#descrip_stats_pattern_cols_trim <- descrip_stats_pattern_cols[!(descrip_stats_pattern_cols %in% c(descrip_stats_pattern_cols[grep("per_positive", descrip_stats_pattern_cols)],descrip_stats_pattern_cols[grep("per_repeats", descrip_stats_pattern_cols)]))]
descrip_stats_pattern_cols_trim <- descrip_stats_pattern_cols[!(descrip_stats_pattern_cols %in% c(descrip_stats_pattern_cols[grep("per_positive", descrip_stats_pattern_cols)]))]

rm2(descrip_stats_pattern_cols_99,descrip_stats_pattern_cols_95,descrip_stats_pattern_cols_90)

yearly_level_cols <- c("yr","year_group_id","sdnet_flow","sdpct_flow",descrip_stats_ios_read_cols,
                       descrip_stats_ios_sim_cols,descrip_stats_ios_quartile_cols)

descrip_stats_data_aggregate0 <-  descrip_stats_data_yearly[,!(colnames(descrip_stats_data_yearly) %in% yearly_level_cols)]
#descrip_stats_data_aggregate <- unique(descrip_stats_data_aggregate0)
descrip_stats_data_aggregate <- data.frame(yr=NA,year_group_id=NA,unique(descrip_stats_data_aggregate0),stringsAsFactors=FALSE)
descrip_stats_data_aggregate[,"yr"] <- 9999
descrip_stats_data_aggregate[,"year_group_id"] <- 9999
row.names(descrip_stats_data_aggregate) <- seq(nrow(descrip_stats_data_aggregate))

rm2(descrip_stats_data_aggregate0)


### Model Parameters
descriptive_overall_vars_model1_vars <- c("pflow","sdpct_flow","mktadjret","mktadjret_sq","fund_ret_mkt_neg",
                                          "AUM","age_y",
                                          "total_fee","Management_Fee_bin","Performance_Fee_bin","Other_Fee_bin",
                                          "Sharpe_Ratio","Sortino_Ratio",
                                          "Listed_on_Exchange_bin","Hurdle_Rate_bin",
                                          "Flagship_bin","Closed_bin","Dead_bin")

#"domicile_onshore_bin","leverage_bin","lock_up_bin","minimum_investment_size","high_water_mark_bin"


descriptive_overall_vars_model1 <- list(note="PA",data="descrip_stats_data_monthly",vars=descriptive_overall_vars_model1_vars)

descriptive_overall_vars_model2_vars <- c(descrip_stats_ios_read_cols,descrip_stats_ios_sim_cols,descrip_stats_ios_tone_cols)
descriptive_overall_vars_model2 <- list(note="PB",data="descrip_stats_data_yearly",vars=descriptive_overall_vars_model2_vars)

descriptive_overall_vars_model3_vars <- c(descrip_stats_pattern_cols)
descriptive_overall_vars_model3 <- list(note="PC",data="descrip_stats_data_aggregate",vars=descriptive_overall_vars_model3_vars)

descriptive_overall_vars_model <- list(descriptive_overall_vars_model1,descriptive_overall_vars_model2,descriptive_overall_vars_model3)

#descriptive_overall_vars_model_note <- sapply(descriptive_overall_vars_model, "[[", "note")
#descriptive_overall_vars_model_data <- sapply(descriptive_overall_vars_model, "[[", "data")
descriptive_overall_vars_model_vars <- sapply(descriptive_overall_vars_model, "[[", "vars")

descriptive_overall_vars_model_vars_all <- unlist(descriptive_overall_vars_model_vars)
descriptive_overall_vars_model_vars_all <- data.frame(id=NA,
                                                      descriptive_overall_vars_model_vars_all, 
                                                      stringsAsFactors=FALSE)
descriptive_overall_vars_model_vars_all[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_all))
colnames(descriptive_overall_vars_model_vars_all)[2] <- "var"

descriptive_overall_vars_model_vars_all <- descriptive_overall_vars_model_vars_all[order(descriptive_overall_vars_model_vars_all[,"id"],
                                                                                         descriptive_overall_vars_model_vars_all[,"var"]),]
row.names(descriptive_overall_vars_model_vars_all) <- seq(nrow(descriptive_overall_vars_model_vars_all))


###############################################################################
cat("DESCRIPTIVE STATISTICS - OVERALL", "\n")
###############################################################################

output_directory_descrip_stats_overall <- paste(output_directory,"descriptive_stats_overall","\\",sep="")
create_directory(output_directory_descrip_stats_overall,remove=1)

descriptive_overall_groups_parameters <-  data.frame(matrix(NA, ncol=5, nrow=11, dimnames=list(c(), c("Start_yr","End_yr","note","data","vars"))), stringsAsFactors=FALSE)
descriptive_overall_groups_parameters[01,] <- c(start_year,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
descriptive_overall_groups_parameters[02,] <- c(start_year,1999    ,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
descriptive_overall_groups_parameters[03,] <- c(2000      ,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
descriptive_overall_groups_parameters[04,] <- c(2000      ,2006    ,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
descriptive_overall_groups_parameters[05,] <- c(2007      ,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
descriptive_overall_groups_parameters[06,] <- c(start_year,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
descriptive_overall_groups_parameters[07,] <- c(start_year,1999    ,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
descriptive_overall_groups_parameters[08,] <- c(2000      ,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
descriptive_overall_groups_parameters[09,] <- c(2000      ,2006    ,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
descriptive_overall_groups_parameters[10,] <- c(2007      ,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
descriptive_overall_groups_parameters[11,] <- c(9999      ,9999    ,"PC","descrip_stats_data_aggregate","descriptive_overall_vars_model3_vars")

a_ply(.data=descriptive_overall_groups_parameters,.margins=1,.fun = descriptive_stats_execute,
      vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_overall,
      stats_keep=c("n","mean","sd","min","quartile1","median","quartile3","max"),.progress = "text")

rm2(output_directory_descrip_stats_overall,descriptive_overall_groups_parameters)


###############################################################################
cat("DESCRIPTIVE STATISTICS - BY YEAR ", "\n")
###############################################################################

output_directory_descrip_stats_by_year <- paste(output_directory,"descriptive_stats_yearly","\\",sep="")
create_directory(output_directory_descrip_stats_by_year,remove=1)

# descriptive_overall_groups_by_year <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), stringsAsFactors=FALSE)
# descriptive_overall_groups_by_year[1,] <- c(start_year,end_year)

descriptive_overall_groups_by_year_parameters <-  data.frame(matrix(NA, ncol=5, nrow=2, dimnames=list(c(), c("Start_yr","End_yr","note","data","vars"))), stringsAsFactors=FALSE)
descriptive_overall_groups_by_year_parameters[1,] <- c(start_year,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
descriptive_overall_groups_by_year_parameters[2,] <- c(start_year,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")


# descriptive_overall_groups_by_year_parameters0 <- alply(.data=descriptive_overall_groups_by_year, .margins=1, .fun = function(x,parameters){
#   
#   llply(.data=parameters, .fun = function(y,x){y_out <- c(x,y)}, x=x)
#   
# }, parameters=descriptive_overall_vars_model, .expand = TRUE, .progress = "none")
# 
# descriptive_overall_groups_by_year_parameters <- do.call(rbind,descriptive_overall_groups_by_year_parameters0)


#l_ply(.data=descriptive_overall_groups_by_year_parameters, .fun = descriptive_stats_by_group_execute,
#      vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_by_year,
#      by_var="yr",stat_types=c("mean","median"),.progress = "text")

a_ply(.data=descriptive_overall_groups_by_year_parameters,.margins=1,.fun = descriptive_stats_by_group_execute,
      vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_by_year,
      by_var="yr",stat_types=c("mean","median"), col_order=seq(start_year,end_year,1),.progress = "text")

rm2(output_directory_descrip_stats_by_year,descriptive_overall_groups_by_year_parameters)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY YEAR GROUP", "\n")
###############################################################################

output_directory_descrip_stats_by_year_group <- paste(output_directory,"descriptive_stats_yearly_group","\\",sep="")
create_directory(output_directory_descrip_stats_by_year_group,remove=1)

#descriptive_overall_groups_by_year_group <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), stringsAsFactors=FALSE)
#descriptive_overall_groups_by_year_group[1,] <- c(start_year,end_year)
#descriptive_stats_by_var_year <- "year_group_id"
#descriptive_stats_by_year_group <- c("mean","median")

descriptive_overall_groups_by_year_group_parameters <-  data.frame(matrix(NA, ncol=5, nrow=2, dimnames=list(c(), c("Start_yr","End_yr","note","data","vars"))), stringsAsFactors=FALSE)
descriptive_overall_groups_by_year_group_parameters[1,] <- c(start_year,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
descriptive_overall_groups_by_year_group_parameters[2,] <- c(start_year,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")


a_ply(.data=descriptive_overall_groups_by_year_group_parameters,.margins=1,.fun = descriptive_stats_by_group_execute,
      vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_by_year_group,
      by_var="year_group_id",stat_types=c("mean","median"), col_order=seq(1,3,1),.progress = "text")


rm2(output_directory_descrip_stats_by_year_group,descriptive_overall_groups_by_year_group_parameters)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY STRATEGY", "\n")
###############################################################################

output_directory_descrip_stats_by_strategy <- paste(output_directory,"descriptive_stats_strategy","\\",sep="")
create_directory(output_directory_descrip_stats_by_strategy,remove=1)

#descriptive_overall_groups_by_strategy <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), stringsAsFactors=FALSE)
#descriptive_overall_groups_by_strategy[1,] <- c(start_year,end_year)
#descriptive_overall_groups_by_strategy[2,] <- c(1994,1999)
#descriptive_overall_groups_by_strategy[3,] <- c(2000,2011)
#descriptive_overall_groups_by_strategy[4,] <- c(2000,2005)
#descriptive_overall_groups_by_strategy[5,] <- c(2006,2011)
#descriptive_stats_by_var_strategy <- "main_investment_strategy"
#descriptive_stats_by_strategy <- c("mean","median")

descriptive_overall_groups_by_strategy_parameters <-  data.frame(matrix(NA, ncol=5, nrow=3, dimnames=list(c(), c("Start_yr","End_yr","note","data","vars"))), stringsAsFactors=FALSE)
descriptive_overall_groups_by_strategy_parameters[1,] <- c(start_year,end_year,"PA","descrip_stats_data_monthly","descriptive_overall_vars_model1_vars")
descriptive_overall_groups_by_strategy_parameters[2,] <- c(start_year,end_year,"PB","descrip_stats_data_yearly","descriptive_overall_vars_model2_vars")
descriptive_overall_groups_by_strategy_parameters[3,] <- c(9999,9999,"PC","descrip_stats_data_aggregate","descriptive_overall_vars_model3_vars")


a_ply(.data=descriptive_overall_groups_by_strategy_parameters,.margins=1,.fun = descriptive_stats_by_group_execute,
      vars=descriptive_overall_vars_model_vars_all,identifier=identifier,output_dir=output_directory_descrip_stats_by_strategy,
      by_var="main_investment_strategy",stat_types=c("mean","median"), col_order=descrip_stats_strategy_cols,.progress = "text")

rm2(output_directory_descrip_stats_by_strategy,descriptive_overall_groups_by_strategy_parameters)

rm2(descriptive_overall_vars_model1_vars, descriptive_overall_vars_model2_vars, descriptive_overall_vars_model3_vars)
rm2(monthly_level_cols,yearly_level_cols,descrip_stats_fund_vars_remove,descrip_stats_ios_vars_remove)
rm2(descriptive_overall_vars_model,descriptive_overall_vars_model1,descriptive_overall_vars_model2,descriptive_overall_vars_model3)
rm2(descriptive_overall_vars_model_vars,descriptive_overall_vars_model_vars_all)


###############################################################################
cat("CORRELATION MATRIX - IOS (PA), FLAGS (PB), & COMBINED (PC) ", "\n")
###############################################################################

corr_decimals <- 2

output_directory_correlation <- paste(output_directory,"correlation","\\",sep="")
create_directory(output_directory_correlation,remove=1)

### Panel A

corr_text_vars_ios_sim <- descrip_stats_ios_sim_cols[grep("_900pct_ios", descrip_stats_ios_sim_cols)] 
corr_text_vars_ios <- c(corr_text_vars_ios_sim,"ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios","avg_grade_level_ios")

#correlation_stars_PA <- corstar(data_all[,corr_text_vars_ios],round=corr_decimals)
correlation_stars_PA0 <- corstarsl(data_all[,corr_text_vars_ios],round=corr_decimals)

correlation_stars_PA <- matrix("", ncol=nrow(correlation_stars_PA0), nrow=nrow(correlation_stars_PA0), 
                               dimnames=list(rownames(correlation_stars_PA0), rownames(correlation_stars_PA0)))

correlation_stars_PA0 <- data.frame(lapply(correlation_stars_PA0, as.character), stringsAsFactors=FALSE)

for (i in 1:ncol(correlation_stars_PA0))
{
  
  temp_col_name <- colnames(correlation_stars_PA0)[i]
  correlation_stars_PA[,temp_col_name] <- correlation_stars_PA0[,temp_col_name]
  
  rm(temp_col_name)
}
rm(i)

diag(correlation_stars_PA) <- paste(format(1.0, digits = corr_decimals, nsmall=corr_decimals),"***",sep="")

correlation_stars_PA <- data.frame(var=row.names(correlation_stars_PA),correlation_stars_PA, stringsAsFactors=FALSE)
row.names(correlation_stars_PA) <- seq(nrow(correlation_stars_PA))

write.csv(correlation_stars_PA,file=paste(output_directory_correlation,"correlation_stars_PA.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(correlation_stars_PA0,correlation_stars_PA)


### Panel B

corr_text_vars_pattern0 <- descrip_stats_pattern_cols_trim[grep("_90", descrip_stats_pattern_cols_trim)] 

#corr_text_vars_pattern1 <- corr_text_vars_pattern0[!(corr_text_vars_pattern0 %in% c(corr_text_vars_pattern0[grep("trim0", corr_text_vars_pattern0)],corr_text_vars_pattern0[grep("trim1", corr_text_vars_pattern0)]))]
corr_text_vars_pattern1 <- corr_text_vars_pattern0[!(corr_text_vars_pattern0 %in% c(corr_text_vars_pattern0[grep("trim0", corr_text_vars_pattern0)],corr_text_vars_pattern0[grep("trim2", corr_text_vars_pattern0)]))]

corr_text_vars_pattern <- corr_text_vars_pattern1

#correlation_stars_PB <- corstar(data_all[,corr_text_vars_pattern],round=corr_decimals)
correlation_stars_PB0 <- corstarsl(data_all[,corr_text_vars_pattern],round=corr_decimals)

correlation_stars_PB <- matrix("", ncol=nrow(correlation_stars_PB0), nrow=nrow(correlation_stars_PB0), 
                               dimnames=list(rownames(correlation_stars_PB0), rownames(correlation_stars_PB0)))

correlation_stars_PB0 <- data.frame(lapply(correlation_stars_PB0, as.character), stringsAsFactors=FALSE)

for (i in 1:ncol(correlation_stars_PB0))
{
  
  temp_col_name <- colnames(correlation_stars_PB0)[i]
  correlation_stars_PB[,temp_col_name] <- correlation_stars_PB0[,temp_col_name]
  
  rm(temp_col_name)
}
rm(i)

diag(correlation_stars_PB) <- paste(format(1.0, digits = corr_decimals, nsmall=corr_decimals),"***",sep="")

correlation_stars_PB <- data.frame(var=row.names(correlation_stars_PB),correlation_stars_PB, stringsAsFactors=FALSE)
row.names(correlation_stars_PB) <- seq(nrow(correlation_stars_PB))

write.csv(correlation_stars_PB,file=paste(output_directory_correlation,"correlation_stars_PB.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(correlation_stars_PB0,correlation_stars_PB)


### Panel C

corr_tone_vars_ios <- c("per_litigious", "per_modalstrong", "per_modalweak", "per_negative", "per_positive", "per_uncertainty")

#correlation_stars_PC <- corstar(data_all[,corr_tone_vars_ios],round=corr_decimals)
correlation_stars_PC0 <- corstarsl(data_all[,corr_tone_vars_ios],round=corr_decimals)

correlation_stars_PC <- matrix("", ncol=nrow(correlation_stars_PC0), nrow=nrow(correlation_stars_PC0), 
                               dimnames=list(rownames(correlation_stars_PC0), rownames(correlation_stars_PC0)))

correlation_stars_PC0 <- data.frame(lapply(correlation_stars_PC0, as.character), stringsAsFactors=FALSE)

for (i in 1:ncol(correlation_stars_PC0))
{
  
  temp_col_name <- colnames(correlation_stars_PC0)[i]
  correlation_stars_PC[,temp_col_name] <- correlation_stars_PC0[,temp_col_name]
  
  rm(temp_col_name)
}
rm(i)

diag(correlation_stars_PC) <- paste(format(1.0, digits = corr_decimals, nsmall=corr_decimals),"***",sep="")

correlation_stars_PC <- data.frame(var=row.names(correlation_stars_PC),correlation_stars_PC, stringsAsFactors=FALSE)
row.names(correlation_stars_PC) <- seq(nrow(correlation_stars_PC))

write.csv(correlation_stars_PC,file=paste(output_directory_correlation,"correlation_stars_PC.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(correlation_stars_PC0,correlation_stars_PC)


### Panel D

corr_text_vars_comb <- c(corr_text_vars_pattern,corr_text_vars_ios,corr_tone_vars_ios)

#correlation_stars_PD <- corstar(data_all[,corr_text_vars_comb],round=corr_decimals)
correlation_stars_PD0 <- corstarsl(data_all[,corr_text_vars_comb],round=corr_decimals)

correlation_stars_PD <- matrix("", ncol=nrow(correlation_stars_PD0), nrow=nrow(correlation_stars_PD0), 
                               dimnames=list(rownames(correlation_stars_PD0), rownames(correlation_stars_PD0)))

correlation_stars_PD0 <- data.frame(lapply(correlation_stars_PD0, as.character), stringsAsFactors=FALSE)

for (i in 1:ncol(correlation_stars_PD0))
{
  
  temp_col_name <- colnames(correlation_stars_PD0)[i]
  correlation_stars_PD[,temp_col_name] <- correlation_stars_PD0[,temp_col_name]
  
  rm(temp_col_name)
}
rm(i)

diag(correlation_stars_PD) <- paste(format(1.0, digits = corr_decimals, nsmall=corr_decimals),"***",sep="")

correlation_stars_PD <- data.frame(var=row.names(correlation_stars_PD),correlation_stars_PD, stringsAsFactors=FALSE)
row.names(correlation_stars_PD) <- seq(nrow(correlation_stars_PD))

write.csv(correlation_stars_PD,file=paste(output_directory_correlation,"correlation_stars_PD.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(correlation_stars_PD0,correlation_stars_PD)

rm2(corr_text_vars_ios_sim,corr_text_vars_ios,corr_text_vars_pattern, corr_tone_vars_ios)
rm2(descrip_stats_strategy_cols,descrip_stats_ios_read_cols,descrip_stats_ios_sim_cols,descrip_stats_ios_tone_cols)
rm2(corr_text_vars_pattern0,corr_text_vars_pattern1,descrip_stats_pattern_cols,descrip_stats_pattern_cols_trim)  
rm2(output_directory_correlation,corr_decimals)

#rm2(descrip_stats_data_monthly,descrip_stats_data_yearly,descrip_stats_data_aggregate)


