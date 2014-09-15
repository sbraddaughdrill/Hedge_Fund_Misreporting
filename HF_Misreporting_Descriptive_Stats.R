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
Location <- 3


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

start_year <- 1994
end_year <- 2011

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

data_all <- read.csv(file=paste(output_directory,"data_all",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)


###############################################################################
cat("DESCRIPTIVE STATISTICS - VARIABLES", "\n")
###############################################################################

descrip_stats_data0 <- data_all

descrip_stats_fund_vars_remove <- c("month",
                                    "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
                                    "nflow_lag1","nflow_lag2","nflow_lag3","nflow_lag4",
                                    "pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                                    "age_m","chgdt",
                                    "mktadjret_lag1_sq","mktadjret_lag2_sq","mktadjret_lag3_sq","mktadjret_lag4_sq",
                                    "sdnet_flow_lag1","sdpct_flow_lag1")

descrip_stats_data0 <- descrip_stats_data0[,!(colnames(descrip_stats_data0) %in% descrip_stats_fund_vars_remove)]
row.names(descrip_stats_data0) <- seq(nrow(descrip_stats_data0))

descrip_stats_ios_vars_remove <- c("month",
                                   "punct_ios","conjunctions_ios","prepositions_ios","normalized_space_ios", 
                                   "pronouns_ios","ttr_ios")

descrip_stats_data0 <- descrip_stats_data0[,!(colnames(descrip_stats_data0) %in% descrip_stats_ios_vars_remove)]
row.names(descrip_stats_data0) <- seq(nrow(descrip_stats_data0))

descrip_stats_data <- data.frame(descrip_stats_data0,
                                 year_group_id=NA,
                                 stringsAsFactors=FALSE)

descrip_stats_data[,"year_group_id"] <- ifelse((descrip_stats_data[,"yr"]>=1994 & descrip_stats_data[,"yr"]<=1999), 1, 
                                               ifelse((descrip_stats_data[,"yr"]>=2000 & descrip_stats_data[,"yr"]<=2005), 2, 
                                                      ifelse((descrip_stats_data[,"yr"]>=2006 & descrip_stats_data[,"yr"]<=2011), 3, 
                                                             descrip_stats_data[,"year_group_id"])))   

rm2(descrip_stats_data0)

descrip_stats_ios_sim_cols <- names(descrip_stats_data)[grep("pct_ios", names(descrip_stats_data))] 

descriptive_overall_vars_model1 <- list(note="PA",
                                        vars=c("pflow","sdpct_flow","exret","mktadjret","mktadjret_sq","fund_ret_mkt_neg",
                                               "aum","age_y",
                                               "total_fee","management_fee","performance_fee","other_fee",
                                               "sharpe_ratio","sortino_ratio","minimum_investment_size",
                                               "listed_on_exchange_bin","hurdle_rate_bin","high_water_mark_bin",
                                               "domicile_onshore_bin","leverage_bin","lock_up_bin",
                                               "flagship_bin","closed_bin","dead_bin"))
descriptive_overall_vars_model2 <- list(note="PB",
                                        vars=c("sentences_ios","words_ios","chars_no_space_ios","num_syll_ios","sntc_per_word_ios",
                                               "avg_sentc_length_ios","avg_word_length_ios","avg_syll_word_ios","sntc_per100_ios",
                                               "syll_per100_ios","lett_per100_ios","fog_hard_words_ios",
                                               "ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                                               "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",descrip_stats_ios_sim_cols))
descriptive_overall_vars_model3 <- list(note="PC",
                                        vars=c("int_nonloading_ff_12","int_loading_ff_12","int_nonloading_ffm_12","int_loading_ffm_12","int_nonloading_ffml_12","int_loading_ffml_12",
                                               "int_nonloading_hf7_12","int_loading_hf7_12","int_nonloading_hf8_12","int_loading_hf8_12",
                                               "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
                                               "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24",
                                               "int_nonloading_ff_36","int_loading_ff_36","int_nonloading_ffm_36","int_loading_ffm_36","int_nonloading_ffml_36","int_loading_ffml_36",
                                               "int_nonloading_hf7_36","int_loading_hf7_36","int_nonloading_hf8_36","int_loading_hf8_36",
                                               "int_nonloading_ff_48","int_loading_ff_48","int_nonloading_ffm_48","int_loading_ffm_48","int_nonloading_ffml_48","int_loading_ffml_48",
                                               "int_nonloading_hf7_48","int_loading_hf7_48","int_nonloading_hf8_48","int_loading_hf8_48",
                                               "int_nonloading_ff_60","int_loading_ff_60","int_nonloading_ffm_60","int_loading_ffm_60","int_nonloading_ffml_60","int_loading_ffml_60",
                                               "int_nonloading_hf7_60","int_loading_hf7_60","int_nonloading_hf8_60","int_loading_hf8_60"))
descriptive_overall_vars_model4 <- list(note="PD",
                                        vars=c("int_nonloading_ff_12_trim","int_loading_ff_12_trim","int_nonloading_ffm_12_trim","int_loading_ffm_12_trim","int_nonloading_ffml_12_trim","int_loading_ffml_12_trim",
                                               "int_nonloading_hf7_12_trim","int_loading_hf7_12_trim","int_nonloading_hf8_12_trim","int_loading_hf8_12_trim",
                                               "int_nonloading_ff_24_trim","int_loading_ff_24_trim","int_nonloading_ffm_24_trim","int_loading_ffm_24_trim","int_nonloading_ffml_24_trim","int_loading_ffml_24_trim",
                                               "int_nonloading_hf7_24_trim","int_loading_hf7_24_trim","int_nonloading_hf8_24_trim","int_loading_hf8_24_trim",
                                               "int_nonloading_ff_36_trim","int_loading_ff_36_trim","int_nonloading_ffm_36_trim","int_loading_ffm_36_trim","int_nonloading_ffml_36_trim","int_loading_ffml_36_trim",
                                               "int_nonloading_hf7_36_trim","int_loading_hf7_36_trim","int_nonloading_hf8_36_trim","int_loading_hf8_36_trim",
                                               "int_nonloading_ff_48_trim","int_loading_ff_48_trim","int_nonloading_ffm_48_trim","int_loading_ffm_48_trim","int_nonloading_ffml_48_trim","int_loading_ffml_48_trim",
                                               "int_nonloading_hf7_48_trim","int_loading_hf7_48_trim","int_nonloading_hf8_48_trim","int_loading_hf8_48_trim",
                                               "int_nonloading_ff_60_trim","int_loading_ff_60_trim","int_nonloading_ffm_60_trim","int_loading_ffm_60_trim","int_nonloading_ffml_60_trim","int_loading_ffml_60_trim",
                                               "int_nonloading_hf7_60_trim","int_loading_hf7_60_trim","int_nonloading_hf8_60_trim","int_loading_hf8_60_trim"))

descriptive_overall_vars_model <- list(descriptive_overall_vars_model1,descriptive_overall_vars_model2,
                                       descriptive_overall_vars_model3,descriptive_overall_vars_model4)

descriptive_overall_vars_model_note <- sapply(descriptive_overall_vars_model, "[[", "note")
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
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB)", "\n")
###############################################################################

descriptive_overall_groups <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                         stringsAsFactors=FALSE)

descriptive_overall_groups[1,] <- c(start_year,end_year)
descriptive_overall_groups[2,] <- c(1994,1999)
descriptive_overall_groups[3,] <- c(2000,2011)
descriptive_overall_groups[4,] <- c(2000,2005)
descriptive_overall_groups[5,] <- c(2006,2011)

# descriptive_overall_groups1 <- list(Start_yr=start_year,End_yr=end_year)
# descriptive_overall_groups2 <- list(Start_yr=1994,End_yr=1999)
# descriptive_overall_groups3 <- list(Start_yr=2000,End_yr=2011)
# descriptive_overall_groups4 <- list(Start_yr=2000,End_yr=2005)
# descriptive_overall_groups5 <- list(Start_yr=2006,End_yr=2011)
# 
# descriptive_overall_groups <- list(descriptive_overall_groups1,
#                                    descriptive_overall_groups2,
#                                    descriptive_overall_groups3,
#                                    descriptive_overall_groups4,
#                                    descriptive_overall_groups5)
# 
# descriptive_overall_groups_Start_yr <- sapply(descriptive_overall_groups, "[[", "Start_yr")
# descriptive_overall_groups_End_yr <- sapply(descriptive_overall_groups, "[[", "End_yr")

output_directory_descrip_stats_overall <- paste(output_directory,"descriptive_stats_overall","\\",sep="")
create_directory(output_directory_descrip_stats_overall,remove=1)

for (k in 1:nrow(descriptive_overall_groups))
{
  #k <- 1
  #k <- 2
  
  cat("START YEAR:", descriptive_overall_groups[k,1], "END YEAR:", descriptive_overall_groups[k,2],"\n")
  
  data_temp <- descrip_stats_data[(descrip_stats_data[,"yr"]>=descriptive_overall_groups[k,1] & descrip_stats_data[,"yr"]<=descriptive_overall_groups[k,2]),]
  
  fund_count <- as.numeric(length(unique(data_temp[,identifier],comparables=FALSE)))
  
  data_temp_no_id1 <- data_temp[,!(colnames(data_temp) %in% identifier)]
  row.names(data_temp_no_id1) <- seq(nrow(data_temp_no_id1))
  
  descriptive_stats_temp_full_all_var <- describe2(data_temp_no_id1[,descriptive_overall_vars_model_vars_all[,c("var")]])
  
  for (l in 1:length(descriptive_overall_vars_model))
  {
    #l <- 1
    #l <- 2
    
    
    descriptive_overall_vars_model_note_temp <- unlist(descriptive_overall_vars_model_note[l])
    descriptive_overall_vars_model_vars_temp <- unlist(descriptive_overall_vars_model_vars[l])
    
    
    
    descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,
                                                           descriptive_overall_vars_model_vars_temp, 
                                                           stringsAsFactors=FALSE)
    descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    colnames(descriptive_overall_vars_model_vars_temp)[2] <- "var"
    
    descriptive_overall_vars_model_vars_temp <- descriptive_overall_vars_model_vars_temp[order(descriptive_overall_vars_model_vars_temp[,"id"],
                                                                                               descriptive_overall_vars_model_vars_temp[,"var"]),]
    row.names(descriptive_overall_vars_model_vars_temp) <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    
    out_file_name <- paste("descriptive_stats",descriptive_overall_groups[k,1],descriptive_overall_groups[k,2],descriptive_overall_vars_model_note_temp,"overall",sep="_")
    
    descriptive_stats_temp_full <- descriptive_stats_temp_full_all_var[descriptive_stats_temp_full_all_var[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
    descriptive_stats_temp_full[,2:ncol(descriptive_stats_temp_full)] <- format(round(descriptive_stats_temp_full[,2:ncol(descriptive_stats_temp_full)],  digits = 8))
    descriptive_stats_temp_full <- rbind(descriptive_stats_temp_full,c("number_of_funds",fund_count,matrix("", ncol=(ncol(descriptive_stats_temp_full)-2), nrow=1)))
    row.names(descriptive_stats_temp_full) <- seq(nrow(descriptive_stats_temp_full))
    
    write.csv(descriptive_stats_temp_full,file=paste(output_directory_descrip_stats_overall,out_file_name,"_full.csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    descriptive_stats_temp <- descriptive_stats_temp_full[c("var","n","quartile1","median","mean","quartile3","sd")]
    #descriptive_stats_temp <- descriptive_stats_temp[match(descriptive_overall_vars_model_vars_temp, descriptive_stats_temp[,c("var")]),]
    write.csv(descriptive_stats_temp,file=paste(output_directory_descrip_stats_overall,out_file_name,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
    
    rm2(descriptive_overall_vars_model_note_temp,descriptive_overall_vars_model_vars_temp,out_file_name)
    rm2(descriptive_stats_temp_full,descriptive_stats_temp)
    
  }
  rm2(data_temp,fund_count,data_temp_no_id1,descriptive_stats_temp_full_all_var,l)
  
}
rm2(descriptive_overall_groups,output_directory_descrip_stats_overall,k)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY YEAR ", "\n")
###############################################################################

descriptive_overall_groups_by_year <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                 stringsAsFactors=FALSE)

descriptive_overall_groups_by_year[1,] <- c(start_year,end_year)

descriptive_stats_by_var_year <- "yr"

descriptive_stats_by_year <- c("mean","median")

output_directory_descrip_stats_by_year <- paste(output_directory,"descriptive_stats_yearly","\\",sep="")
create_directory(output_directory_descrip_stats_by_year,remove=1)

for (k in 1:nrow(descriptive_overall_groups_by_year))
{
  #k <- 1
  
  cat("START YEAR:", descriptive_overall_groups_by_year[k,1], "END YEAR:", descriptive_overall_groups_by_year[k,2],"\n")
  
  data_temp <- descrip_stats_data[(descrip_stats_data[,"yr"]>=descriptive_overall_groups_by_year[k,1] & descrip_stats_data[,"yr"]<=descriptive_overall_groups_by_year[k,2]),]
  
  fund_count_yr1 <- ddply(data_temp, descriptive_stats_by_var_year, function(x) {data.frame(var="number_of_funds", 
                                                                                            count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                                                            stringsAsFactors=FALSE)})
  fund_count_yr2 <- data.frame(temp_var="ZZZ",
                               var="number_of_funds", 
                               count=as.numeric(length(unique(data_temp[,identifier],comparables=FALSE))),
                               stringsAsFactors=FALSE)
  colnames(fund_count_yr2)[match("temp_var",names(fund_count_yr2))] <- descriptive_stats_by_var_year
  
  fund_count_yr <- rbind(fund_count_yr1,fund_count_yr2)
  
  rm(fund_count_yr1,fund_count_yr2)
  
  data_temp_no_id1 <- data_temp[,!(colnames(data_temp) %in% identifier)]
  row.names(data_temp_no_id1) <- seq(nrow(data_temp_no_id1))
  
  #descriptive_stats_temp_full_all_var_year <- describeBy2(descrip_stats_fund2,"yr")
  
  descriptive_stats_temp_full_all_var_year <- describeBy2(data_temp_no_id1[,c(descriptive_stats_by_var_year,descriptive_overall_vars_model_vars_all[,c("var")])],descriptive_stats_by_var_year)
  
  assign("descriptive_stats_temp_full_all_var_year", descriptive_stats_temp_full_all_var_year, envir = .GlobalEnv)
  
  for (l in 1:length(descriptive_overall_vars_model))
  {
    #l <- 1
    #l <- 2
    
    descriptive_overall_vars_model_note_temp <- unlist(descriptive_overall_vars_model_note[l])
    descriptive_overall_vars_model_vars_temp <- unlist(descriptive_overall_vars_model_vars[l])
    descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,
                                                           descriptive_overall_vars_model_vars_temp, 
                                                           stringsAsFactors=FALSE)
    descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    colnames(descriptive_overall_vars_model_vars_temp)[2] <- "var"
    
    descriptive_overall_vars_model_vars_temp <- descriptive_overall_vars_model_vars_temp[order(descriptive_overall_vars_model_vars_temp[,"id"],
                                                                                               descriptive_overall_vars_model_vars_temp[,"var"]),]
    row.names(descriptive_overall_vars_model_vars_temp) <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    
    out_file_name1 <- paste("descriptive_stats",descriptive_overall_groups_by_year[k,1],descriptive_overall_groups_by_year[k,2],descriptive_overall_vars_model_note_temp,"year",sep="_")
    
    descriptive_stats_yr_temp_full_trim <- descriptive_stats_temp_full_all_var_year[descriptive_stats_temp_full_all_var_year[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
    row.names(descriptive_stats_yr_temp_full_trim) <- seq(nrow(descriptive_stats_yr_temp_full_trim))
    
    for (m in 1:length(descriptive_stats_by_year))
    {
      #m <- 1
      #m <- 2
      
      fund_count_yr_temp <- fund_count_yr
      colnames(fund_count_yr_temp)[match("count",names(fund_count_yr_temp))] <- descriptive_stats_by_year[m]
      
      out_file_name2 <- paste(out_file_name1,descriptive_stats_by_year[m],sep="_")
      
      descriptive_stats_yr_temp_trim <- rbind(descriptive_stats_yr_temp_full_trim[c(descriptive_stats_by_var_year,"var",descriptive_stats_by_year[m])],
                                              fund_count_yr_temp)
      
      #descriptive_stats_yr_temp <- suppressMessages(dcast(descriptive_stats_yr_temp_trim, var~yr))
      descriptive_stats_yr_temp <- suppressMessages(dcast(descriptive_stats_yr_temp_trim, 
                                                          eval(parse(text=paste("var",descriptive_stats_by_var_year,sep="~")))))
      
      descriptive_stats_yr_temp2 <- merge(descriptive_stats_yr_temp, descriptive_overall_vars_model_vars_temp, 
                                          by.x=c("var"), by.y=c("var"), 
                                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      descriptive_stats_yr_temp2 <- descriptive_stats_yr_temp2[order(descriptive_stats_yr_temp2[,"id"]),]
      row.names(descriptive_stats_yr_temp2) <- seq(nrow(descriptive_stats_yr_temp2))
      colnames(descriptive_stats_yr_temp2)[match("ZZZ",names(descriptive_stats_yr_temp2))] <- "Full"
      
      descriptive_stats_yr_temp2 <- descriptive_stats_yr_temp2[,!(colnames(descriptive_stats_yr_temp2) %in% c("id"))]
      row.names(descriptive_stats_yr_temp2) <- seq(nrow(descriptive_stats_yr_temp2))
      
      #Remove digits on number_of_funds
      descriptive_stats_yr_temp3 <- descriptive_stats_yr_temp2
      descriptive_stats_yr_temp3[,2:ncol(descriptive_stats_yr_temp3)] <- format(round(descriptive_stats_yr_temp3[,2:ncol(descriptive_stats_yr_temp3)],  digits = 8))
      
      for(i in which(descriptive_stats_yr_temp3[,1]=="number_of_funds"))
      {
        descriptive_stats_yr_temp3[i,2:ncol(descriptive_stats_yr_temp3)] <- sprintf("%.0f",descriptive_stats_yr_temp3[i,2:ncol(descriptive_stats_yr_temp3)]) 
      }
      
      #descriptive_stats_yr_temp3[,(2:ncol(descriptive_stats_yr_temp3))] <- format(round(descriptive_stats_yr_temp3[,(2:ncol(descriptive_stats_yr_temp3))],  digits = 6))
      
      descriptive_stats_yr_temp4 <- apply(descriptive_stats_yr_temp3,2,function(x) {x <- ifelse(is.na(x),"", x)})
      descriptive_stats_yr_temp4 <- as.data.frame(descriptive_stats_yr_temp4,stringsAsFactors=FALSE)
      
      
      write.csv(descriptive_stats_yr_temp4,file=paste(output_directory_descrip_stats_by_year,out_file_name2,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
      
      rm2(fund_count_yr_temp,out_file_name2,descriptive_stats_yr_temp_trim)
      rm2(descriptive_stats_yr_temp,descriptive_stats_yr_temp2,descriptive_stats_yr_temp3,descriptive_stats_yr_temp4)
      
    }
    rm2(descriptive_overall_vars_model_note_temp,descriptive_overall_vars_model_vars_temp,out_file_name1,m)
    rm2(descriptive_stats_yr_temp_full_trim)
    
  }
  rm2(data_temp,fund_count_yr,data_temp_no_id1,l)
  rm2(descriptive_stats_temp_full_all_var_year)
  
}
rm2(descriptive_stats_by_var_year,descriptive_overall_groups_by_year,descriptive_stats_by_year,output_directory_descrip_stats_by_year,k)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY YEAR GROUP", "\n")
###############################################################################

descriptive_overall_groups_by_year_group <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                       stringsAsFactors=FALSE)

descriptive_overall_groups_by_year_group[1,] <- c(start_year,end_year)

descriptive_stats_by_var_year <- "year_group_id"

descriptive_stats_by_year_group <- c("mean","median")

output_directory_descrip_stats_by_year_group <- paste(output_directory,"descriptive_stats_yearly_group","\\",sep="")
create_directory(output_directory_descrip_stats_by_year_group,remove=1)

for (k in 1:nrow(descriptive_overall_groups_by_year_group))
{
  #k <- 1
  
  cat("START YEAR:", descriptive_overall_groups_by_year_group[k,1], "END YEAR:", descriptive_overall_groups_by_year_group[k,2],"\n")
  
  data_temp_year_group <- descrip_stats_data[(descrip_stats_data[,"yr"]>=descriptive_overall_groups_by_year_group[k,1] & descrip_stats_data[,"yr"]<=descriptive_overall_groups_by_year_group[k,2]),]
  
  fund_count_yr_group1 <- ddply(data_temp_year_group, descriptive_stats_by_var_year, function(x) {data.frame(var="number_of_funds", 
                                                                                                             count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                                                                             stringsAsFactors=FALSE)})
  fund_count_yr_group2 <- data.frame(temp_var="ZZZ",
                                     var="number_of_funds", 
                                     count=as.numeric(length(unique(data_temp_year_group[,identifier],comparables=FALSE))),
                                     stringsAsFactors=FALSE)
  colnames(fund_count_yr_group2)[match("temp_var",names(fund_count_yr_group2))] <- descriptive_stats_by_var_year
  
  fund_count_yr_group <- rbind(fund_count_yr_group1,fund_count_yr_group2)
  
  rm(fund_count_yr_group1,fund_count_yr_group2)
  
  data_temp_year_group_no_id <- data_temp_year_group[,!(colnames(data_temp_year_group) %in% identifier)]
  row.names(data_temp_year_group_no_id) <- seq(nrow(data_temp_year_group_no_id))
  
  #descriptive_stats_temp_full_all_var_year_group <- describeBy2(descrip_stats_fund2,"yr")
  
  descriptive_stats_temp_full_all_var_year_group <- describeBy2(data_temp_year_group_no_id[,c(descriptive_stats_by_var_year,descriptive_overall_vars_model_vars_all[,c("var")])],descriptive_stats_by_var_year)
  
  assign("descriptive_stats_temp_full_all_var_year_group", descriptive_stats_temp_full_all_var_year_group, envir = .GlobalEnv)
  
  for (l in 1:length(descriptive_overall_vars_model))
  {
    #l <- 1
    #l <- 2
    
    descriptive_overall_vars_model_note_temp <- unlist(descriptive_overall_vars_model_note[l])
    descriptive_overall_vars_model_vars_temp <- unlist(descriptive_overall_vars_model_vars[l])
    descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,
                                                           descriptive_overall_vars_model_vars_temp, 
                                                           stringsAsFactors=FALSE)
    descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    colnames(descriptive_overall_vars_model_vars_temp)[2] <- "var"
    
    descriptive_overall_vars_model_vars_temp <- descriptive_overall_vars_model_vars_temp[order(descriptive_overall_vars_model_vars_temp[,"id"],
                                                                                               descriptive_overall_vars_model_vars_temp[,"var"]),]
    row.names(descriptive_overall_vars_model_vars_temp) <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    
    out_file_name1 <- paste("descriptive_stats",descriptive_overall_groups_by_year_group[k,1],descriptive_overall_groups_by_year_group[k,2],descriptive_overall_vars_model_note_temp,"year",sep="_")
    
    descriptive_stats_yr_temp_full_trim <- descriptive_stats_temp_full_all_var_year_group[descriptive_stats_temp_full_all_var_year_group[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
    row.names(descriptive_stats_yr_temp_full_trim) <- seq(nrow(descriptive_stats_yr_temp_full_trim))
    
    for (m in 1:length(descriptive_stats_by_year_group))
    {
      #m <- 1
      #m <- 2
      
      fund_count_yr_group_temp <- fund_count_yr_group
      colnames(fund_count_yr_group_temp)[match("count",names(fund_count_yr_group_temp))] <- descriptive_stats_by_year_group[m]
      
      out_file_name2 <- paste(out_file_name1,descriptive_stats_by_year_group[m],sep="_")
      
      descriptive_stats_yr_temp_trim <- rbind(descriptive_stats_yr_temp_full_trim[c(descriptive_stats_by_var_year,"var",descriptive_stats_by_year_group[m])],
                                              fund_count_yr_group_temp)
      
      #descriptive_stats_yr_temp <- suppressMessages(dcast(descriptive_stats_yr_temp_trim, var~yr))
      descriptive_stats_yr_temp <- suppressMessages(dcast(descriptive_stats_yr_temp_trim, 
                                                          eval(parse(text=paste("var",descriptive_stats_by_var_year,sep="~")))))
      
      descriptive_stats_yr_temp2 <- merge(descriptive_stats_yr_temp, descriptive_overall_vars_model_vars_temp, 
                                          by.x=c("var"), by.y=c("var"), 
                                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      descriptive_stats_yr_temp2 <- descriptive_stats_yr_temp2[order(descriptive_stats_yr_temp2[,"id"]),]
      row.names(descriptive_stats_yr_temp2) <- seq(nrow(descriptive_stats_yr_temp2))
      colnames(descriptive_stats_yr_temp2)[match("ZZZ",names(descriptive_stats_yr_temp2))] <- "Full"
      
      descriptive_stats_yr_temp2 <- descriptive_stats_yr_temp2[,!(colnames(descriptive_stats_yr_temp2) %in% c("id"))]
      row.names(descriptive_stats_yr_temp2) <- seq(nrow(descriptive_stats_yr_temp2))
      
      #Remove digits on number_of_funds
      descriptive_stats_yr_temp3 <- descriptive_stats_yr_temp2
      descriptive_stats_yr_temp3[,2:ncol(descriptive_stats_yr_temp3)] <- format(round(descriptive_stats_yr_temp3[,2:ncol(descriptive_stats_yr_temp3)],  digits = 8))
      
      for(i in which(descriptive_stats_yr_temp3[,1]=="number_of_funds"))
      {
        descriptive_stats_yr_temp3[i,2:ncol(descriptive_stats_yr_temp3)] <- sprintf("%.0f",descriptive_stats_yr_temp3[i,2:ncol(descriptive_stats_yr_temp3)]) 
      }
      
      #descriptive_stats_yr_temp3[,(2:ncol(descriptive_stats_yr_temp3))] <- format(round(descriptive_stats_yr_temp3[,(2:ncol(descriptive_stats_yr_temp3))],  digits = 6))
      
      descriptive_stats_yr_temp4 <- apply(descriptive_stats_yr_temp3,2,function(x) {x <- ifelse(is.na(x),"", x)})
      descriptive_stats_yr_temp4 <- as.data.frame(descriptive_stats_yr_temp4,stringsAsFactors=FALSE)
      
      
      write.csv(descriptive_stats_yr_temp4,file=paste(output_directory_descrip_stats_by_year_group,out_file_name2,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
      
      rm2(fund_count_yr_group_temp,out_file_name2,descriptive_stats_yr_temp_trim)
      rm2(descriptive_stats_yr_temp,descriptive_stats_yr_temp2,descriptive_stats_yr_temp3,descriptive_stats_yr_temp4)
      
    }
    rm2(descriptive_overall_vars_model_note_temp,descriptive_overall_vars_model_vars_temp,out_file_name1,m)
    rm2(descriptive_stats_yr_temp_full_trim)
    
  }
  rm2(data_temp_year_group,fund_count_yr_group,data_temp_year_group_no_id,l)
  rm2(descriptive_stats_temp_full_all_var_year_group)
}
rm2(descriptive_stats_by_var_year,descriptive_overall_groups_by_year_group,descriptive_stats_by_year_group,output_directory_descrip_stats_by_year_group,k)


###############################################################################
cat("DESCRIPTIVE STATISTICS - FUND ATTRIBUTES (PA) & IOS (PB) BY STRATEGY", "\n")
###############################################################################

descriptive_overall_groups_by_strategy <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                                     stringsAsFactors=FALSE)

descriptive_overall_groups_by_strategy[1,] <- c(start_year,end_year)
#descriptive_overall_groups_by_strategy[2,] <- c(1994,1999)
#descriptive_overall_groups_by_strategy[3,] <- c(2000,2011)
#descriptive_overall_groups_by_strategy[4,] <- c(2000,2005)
#descriptive_overall_groups_by_strategy[5,] <- c(2006,2011)

descriptive_stats_by_var_strategy <- "main_investment_strategy"

descriptive_stats_by_strategy <- c("mean","median")

output_directory_descrip_stats_by_strategy <- paste(output_directory,"descriptive_stats_strategy","\\",sep="")
create_directory(output_directory_descrip_stats_by_strategy,remove=1)

for (k in 1:nrow(descriptive_overall_groups_by_strategy))
{
  #k <- 1
  
  cat("START YEAR:", descriptive_overall_groups_by_strategy[k,1], "END YEAR:", descriptive_overall_groups_by_strategy[k,2],"\n")
  
  data_temp <- descrip_stats_data[(descrip_stats_data[,"yr"]>=descriptive_overall_groups_by_strategy[k,1] & descrip_stats_data[,"yr"]<=descriptive_overall_groups_by_strategy[k,2]),]
  
  fund_count_yr1 <- ddply(data_temp, descriptive_stats_by_var_strategy, function(x) {data.frame(var="number_of_funds", 
                                                                                                count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                                                                stringsAsFactors=FALSE)})
  fund_count_yr2 <- data.frame(temp_var="ZZZ",
                               var="number_of_funds", 
                               count=as.numeric(length(unique(data_temp[,identifier],comparables=FALSE))),
                               stringsAsFactors=FALSE)
  colnames(fund_count_yr2)[match("temp_var",names(fund_count_yr2))] <- descriptive_stats_by_var_strategy
  
  fund_count_yr <- rbind(fund_count_yr1,fund_count_yr2)
  
  rm(fund_count_yr1,fund_count_yr2)
  
  data_temp_no_id1 <- data_temp[,!(colnames(data_temp) %in% identifier)]
  row.names(data_temp_no_id1) <- seq(nrow(data_temp_no_id1))
  
  #descriptive_stats_temp_full_all_var_strategy <- describeBy2(descrip_stats_fund2,"yr")
  descriptive_stats_temp_full_all_var_strategy <- describeBy2(data_temp_no_id1[,c(descriptive_stats_by_var_strategy,descriptive_overall_vars_model_vars_all[,c("var")])],descriptive_stats_by_var_strategy)
  
  assign("descriptive_stats_temp_full_all_var_strategy", descriptive_stats_temp_full_all_var_strategy, envir = .GlobalEnv)
  
  for (l in 1:length(descriptive_overall_vars_model))
  {
    #l <- 1
    #l <- 2
    
    descriptive_overall_vars_model_note_temp <- unlist(descriptive_overall_vars_model_note[l])
    descriptive_overall_vars_model_vars_temp <- unlist(descriptive_overall_vars_model_vars[l])
    descriptive_overall_vars_model_vars_temp <- data.frame(id=NA,
                                                           descriptive_overall_vars_model_vars_temp, 
                                                           stringsAsFactors=FALSE)
    descriptive_overall_vars_model_vars_temp[,c("id")] <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    colnames(descriptive_overall_vars_model_vars_temp)[2] <- "var"
    
    descriptive_overall_vars_model_vars_temp <- descriptive_overall_vars_model_vars_temp[order(descriptive_overall_vars_model_vars_temp[,"id"],
                                                                                               descriptive_overall_vars_model_vars_temp[,"var"]),]
    row.names(descriptive_overall_vars_model_vars_temp) <- seq(nrow(descriptive_overall_vars_model_vars_temp))
    
    out_file_name1 <- paste("descriptive_stats",descriptive_overall_groups_by_strategy[k,1],descriptive_overall_groups_by_strategy[k,2],descriptive_overall_vars_model_note_temp,"strategy",sep="_")
    
    descriptive_stats_strategy_temp_full_trim <- descriptive_stats_temp_full_all_var_strategy[descriptive_stats_temp_full_all_var_strategy[,"var"] %in% descriptive_overall_vars_model_vars_temp[,c("var")],]
    row.names(descriptive_stats_strategy_temp_full_trim) <- seq(nrow(descriptive_stats_strategy_temp_full_trim))
    
    for (m in 1:length(descriptive_stats_by_strategy))
    {
      #m <- 1
      #m <- 2
      
      fund_count_yr_temp <- fund_count_yr
      colnames(fund_count_yr_temp)[match("count",names(fund_count_yr_temp))] <- descriptive_stats_by_strategy[m]
      
      out_file_name2 <- paste(out_file_name1,descriptive_stats_by_strategy[m],sep="_")
      
      descriptive_stats_strategy_temp_trim <- rbind(descriptive_stats_strategy_temp_full_trim[c(descriptive_stats_by_var_strategy,"var",descriptive_stats_by_strategy[m])],
                                                    fund_count_yr_temp)
      
      #descriptive_stats_strategy_temp <- suppressMessages(dcast(descriptive_stats_strategy_temp_trim, var~yr))
      descriptive_stats_strategy_temp <- suppressMessages(dcast(descriptive_stats_strategy_temp_trim, 
                                                                eval(parse(text=paste("var",descriptive_stats_by_var_strategy,sep="~")))))
      
      descriptive_stats_strategy_temp2 <- merge(descriptive_stats_strategy_temp, descriptive_overall_vars_model_vars_temp, 
                                                by.x=c("var"), by.y=c("var"), 
                                                all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      descriptive_stats_strategy_temp2 <- descriptive_stats_strategy_temp2[order(descriptive_stats_strategy_temp2[,"id"]),]
      row.names(descriptive_stats_strategy_temp2) <- seq(nrow(descriptive_stats_strategy_temp2))
      colnames(descriptive_stats_strategy_temp2)[match("ZZZ",names(descriptive_stats_strategy_temp2))] <- "Full"
      
      descriptive_stats_strategy_temp2 <- descriptive_stats_strategy_temp2[,!(colnames(descriptive_stats_strategy_temp2) %in% c("id"))]
      row.names(descriptive_stats_strategy_temp2) <- seq(nrow(descriptive_stats_strategy_temp2))
      
      #Remove digits on number_of_funds
      descriptive_stats_strategy_temp3 <- descriptive_stats_strategy_temp2
      descriptive_stats_strategy_temp3[,2:ncol(descriptive_stats_strategy_temp3)] <- format(round(descriptive_stats_strategy_temp3[,2:ncol(descriptive_stats_strategy_temp3)],  digits = 8))
      
      for(i in which(descriptive_stats_strategy_temp3[,1]=="number_of_funds"))
      {
        #i <- 25
        
        descriptive_stats_strategy_temp3[i,2:ncol(descriptive_stats_strategy_temp3)] <- sprintf("%.0f",descriptive_stats_strategy_temp3[i,2:ncol(descriptive_stats_strategy_temp3)])
        
      }
      
      descriptive_stats_strategy_temp4 <- apply(descriptive_stats_strategy_temp3,2,function(x) {x <- ifelse(is.na(x),"", x)})
      descriptive_stats_strategy_temp4 <- as.data.frame(descriptive_stats_strategy_temp4,stringsAsFactors=FALSE)
      
      
      descriptive_stats_strategy_temp5 <- descriptive_stats_strategy_temp4[,c(colnames(descriptive_stats_strategy_temp4[,!(colnames(descriptive_stats_strategy_temp4) %in% c("Others","Full"))]),
                                                                              "Others",
                                                                              "Full")]
      row.names(descriptive_stats_strategy_temp5) <- seq(nrow(descriptive_stats_strategy_temp5))
      
      write.csv(descriptive_stats_strategy_temp5,file=paste(output_directory_descrip_stats_by_strategy,out_file_name2,".csv",sep=""),na="",quote=TRUE,row.names=FALSE)
      
      rm2(fund_count_yr_temp,out_file_name2,descriptive_stats_strategy_temp_trim)
      rm2(descriptive_stats_strategy_temp,descriptive_stats_strategy_temp2,descriptive_stats_strategy_temp3)
      rm2(descriptive_stats_strategy_temp4,descriptive_stats_strategy_temp5)
      
    }
    rm2(descriptive_overall_vars_model_note_temp,descriptive_overall_vars_model_vars_temp,out_file_name1,m)
    rm2(descriptive_stats_strategy_temp_full_trim)
    
  }
  rm2(data_temp,fund_count_yr,data_temp_no_id1,l)
  rm2(descriptive_stats_temp_full_all_var_strategy)
  
}
rm2(descriptive_stats_by_var_strategy,descriptive_overall_groups_by_strategy,descriptive_stats_by_strategy,output_directory_descrip_stats_by_strategy,k)

rm2(descrip_stats_data,descrip_stats_fund_vars_remove,descrip_stats_ios_vars_remove)
rm2(descriptive_overall_vars_model1,descriptive_overall_vars_model2,descriptive_overall_vars_model3,descriptive_overall_vars_model4)
rm2(descriptive_overall_vars_model)
rm2(descriptive_overall_vars_model_note,descriptive_overall_vars_model_vars,descriptive_overall_vars_model_vars_all)


###############################################################################
cat("CORRELATION MATRIX (PANEL A & B)", "\n")
###############################################################################

corr_decimals <- 3

corr_text_vars_ios_sim <- descrip_stats_ios_sim_cols[grep("_900pct_ios", descrip_stats_ios_sim_cols)] 

corr_text_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",corr_text_vars_ios_sim)

#correlation_stars_PA <- corstar(data_all[,corr_text_vars_ios],round=corr_decimals)
correlation_stars_PA0 <- corstarsl(data_all[,corr_text_vars_ios],round=corr_decimals)

correlation_stars_PA <- matrix("", ncol=nrow(correlation_stars_PA0), nrow=nrow(correlation_stars_PA0), 
                               dimnames=list(rownames(correlation_stars_PA0), rownames(correlation_stars_PA0)))

correlation_stars_PA0 <- data.frame(lapply(correlation_stars_PA0, as.character), stringsAsFactors=FALSE)

for (i in 1:ncol(correlation_stars_PA0))
{
  
  temp_col_name <- colnames(correlation_stars_PA0)[i]
  correlation_stars_PA[,temp_col_name] <- correlation_stars_PA0[,temp_col_name]
  
}

diag(correlation_stars_PA) <- paste(format(1.0, digits = corr_decimals, nsmall=corr_decimals),"***",sep="")

correlation_stars_PA <- data.frame(var=row.names(correlation_stars_PA),correlation_stars_PA, stringsAsFactors=FALSE)
row.names(correlation_stars_PA) <- seq(nrow(correlation_stars_PA))

output_directory_correlation <- paste(output_directory,"correlation","\\",sep="")
create_directory(output_directory_correlation,remove=1)

write.csv(correlation_stars_PA,file=paste(output_directory_correlation,"correlation_stars_PA.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(descrip_stats_ios_sim_cols)
rm2(corr_text_vars_ios_sim,corr_text_vars_ios,output_directory_correlation)
rm2(correlation_stars_PA0,correlation_stars_PA,corr_decimals,temp_col_name,i)
