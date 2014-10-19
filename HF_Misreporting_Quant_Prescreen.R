# TODO: Add comment
# 
# Author:  Brad
# File:    HF_Misreporting_Quant_Prescreen.R
# Version: 1.0
# Date:    08.26.2014
# Purpose: Merge the performance and text data
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
Location <- 4

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

source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_parse.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("benford.analysis","BenfordTests","data.table","gdata","ggplot2","MASS","plyr","quantmod",
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
descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="\\")
data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="\\")



###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "fund_id"

beg_year <- 1994
end_year <- 2011

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

#Fund Information
fund_table <- "EurekahedgeHF_Excel_aca_full14"
EurekahedgeHF_Excel_aca_full_import_vars_remove <- c("exposure_cash","exposure_commodities","exposure_currency","exposure_derivatives",
                                                     "exposure_equities","exposure_fixed_income","exposure_life_insurance",
                                                     "exposure_non_life_insurance","exposure_private_equity","exposure_real_estate",
                                                     "instrument_traded_cash","instrument_traded_commodities","instrument_traded_currency","instrument_traded_derivatives",
                                                     "instrument_traded_equities","instrument_traded_fixed_income","instrument_traded_life_insurance",
                                                     "instrument_traded_non_life_insurance","instrument_traded_private_equity","instrument_traded_real_estate",
                                                     "flagship","closed","dead","limited","invest_in_private_placements","managed_accounts_offered","ucits",
                                                     "management_fee_comments","management_fee_org",
                                                     "performance_fee_comments","performance_fee_org",
                                                     "other_fee_comments","other_fee_org",
                                                     "dividend_policy","dividend_policy_org","dividend_policy_comments",
                                                     "fund_closed","fund_closed_comments","fund_closed_org",
                                                     "high_water_mark","high_water_mark_comments","high_water_mark_org",
                                                     "hurdle_rate","hurdle_rate_comments","hurdle_rate_org",
                                                     "listed_on_exchange","listed_on_exchange_org","listed_on_exchange_comments",
                                                     "custodian","custodian1","custodian2","custodian3","custodian4","custodian5","custodian6",
                                                     "legal_advisor_offshore","legal_advisor_offshore1","legal_advisor_offshore2","legal_advisor_offshore3",
                                                     "legal_advisor_onshore","legal_advisor_onshore1","legal_advisor_onshore2","legal_advisor_onshore3","legal_advisor_onshore4",
                                                     "principal_prime_broker_broker","principal_prime_broker_broker1","principal_prime_broker_broker2","principal_prime_broker_broker3",
                                                     "principal_prime_broker_broker4","principal_prime_broker_broker5","principal_prime_broker_broker6","principal_prime_broker_broker7",
                                                     "principal_prime_broker_broker8",
                                                     "secondary_prime_broker_broker","secondary_prime_broker_broker1","secondary_prime_broker_broker2",
                                                     "secondary_prime_broker_broker3","secondary_prime_broker_broker4","secondary_prime_broker_broker5",
                                                     "secondary_prime_broker_broker6",
                                                     "synthetic_prime_broker","synthetic_prime_broker1","synthetic_prime_broker2","synthetic_prime_broker3","synthetic_prime_broker4",
                                                     "base_currency","minimum_investment_currency","reuters","strategy","secondary_investment_strategy",
                                                     "administrator","auditor","countries","equalisation_share_class","exchange_name","industry_focus","investment_geography",
                                                     "manager_profile",
                                                     "monthly_ret2","yearly_ret","limited_bin","synthetic_prime_broker_count")
EurekahedgeHF_Excel_aca_full_import_vars_keep0 <- descriptive_stats_fields[descriptive_stats_fields[,"table"]==fund_table,c("field")]
EurekahedgeHF_Excel_aca_full_import_vars_keep1 <- EurekahedgeHF_Excel_aca_full_import_vars_keep0[!(EurekahedgeHF_Excel_aca_full_import_vars_keep0 %in% EurekahedgeHF_Excel_aca_full_import_vars_remove)]
EurekahedgeHF_Excel_aca_full_import_vars_keep2 <- paste(EurekahedgeHF_Excel_aca_full_import_vars_keep1,sep="",collapse=", ")

#Text Information
text_table <- "text_stats_ios"
text_stats_ios_import_vars_remove <- c("lines_ios")
text_stats_ios_import_vars_keep0 <- descriptive_stats_fields[descriptive_stats_fields[,"table"]==text_table,c("field")]
text_stats_ios_import_vars_keep1 <- text_stats_ios_import_vars_keep0[!(text_stats_ios_import_vars_keep0 %in% text_stats_ios_import_vars_remove)]
text_stats_ios_import_vars_keep2 <- paste(text_stats_ios_import_vars_keep1,sep="",collapse=", ")

rm2(EurekahedgeHF_Excel_aca_full_import_vars_keep0,EurekahedgeHF_Excel_aca_full_import_vars_keep1,EurekahedgeHF_Excel_aca_full_import_vars_remove)
rm2(text_stats_ios_import_vars_keep0,text_stats_ios_import_vars_keep1,text_stats_ios_import_vars_remove)
rm2(descriptive_stats_tables,descriptive_stats_fields)


###############################################################################
cat("IMPORT AND FIX FUND DATA", "\n")
###############################################################################

query_EurekahedgeHF_Excel_aca_full <- ""
query_EurekahedgeHF_Excel_aca_full <- paste(query_EurekahedgeHF_Excel_aca_full, "select       ",EurekahedgeHF_Excel_aca_full_import_vars_keep2, sep=" ")
query_EurekahedgeHF_Excel_aca_full <- paste(query_EurekahedgeHF_Excel_aca_full, "from         ",fund_table, "                                ", sep=" ")
query_EurekahedgeHF_Excel_aca_full <- trim(gsub(" {2,}", " ", query_EurekahedgeHF_Excel_aca_full))

rm2(EurekahedgeHF_Excel_aca_full_import_vars_keep2)

#EurekahedgeHF_Excel_aca_full <- runsql("SELECT * FROM EurekahedgeHF_Excel_aca_full14",descriptive_stats_db)

EurekahedgeHF_Excel_aca_full <- data.frame(runsql(query_EurekahedgeHF_Excel_aca_full,descriptive_stats_db),
                                           total_fee=NA,
                                           fund_ret_mkt_neg=NA,
                                           yr_month=NA,
                                           stringsAsFactors=FALSE)

colnames(EurekahedgeHF_Excel_aca_full) <- tolower(colnames(EurekahedgeHF_Excel_aca_full))

EurekahedgeHF_Excel_aca_full2 <- unknown_to_NA(EurekahedgeHF_Excel_aca_full,unknowns_strings)

rm(EurekahedgeHF_Excel_aca_full)

EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,identifier]),]
EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,"yr"]),]
EurekahedgeHF_Excel_aca_full2 <- EurekahedgeHF_Excel_aca_full2[!is.na(EurekahedgeHF_Excel_aca_full2[,"month"]),]

#Create total fees, negative return, and yr_month
EurekahedgeHF_Excel_aca_full2[,"total_fee"] <- rowSums(EurekahedgeHF_Excel_aca_full2[,c("management_fee","performance_fee","other_fee")],na.rm=TRUE)
EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"] <- ifelse(EurekahedgeHF_Excel_aca_full2[,"mktadjret"]<0, EurekahedgeHF_Excel_aca_full2[,"mktadjret"], 0)
EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"] <- ifelse(is.na(EurekahedgeHF_Excel_aca_full2[,"mktadjret"]), NA, EurekahedgeHF_Excel_aca_full2[,"fund_ret_mkt_neg"])
EurekahedgeHF_Excel_aca_full2[,"yr_month"] <- paste(EurekahedgeHF_Excel_aca_full2[,"yr"],sprintf("%02d", EurekahedgeHF_Excel_aca_full2[,"month"]),sep="_")

#Trim Years
monthly_data_all_yr_trim <- EurekahedgeHF_Excel_aca_full2[(EurekahedgeHF_Excel_aca_full2[,"yr"]>=beg_year & EurekahedgeHF_Excel_aca_full2[,"yr"]<=end_year),]

rm2(EurekahedgeHF_Excel_aca_full2)

#Fix dates
monthly_data_all04 <- monthly_data_all_yr_trim

rm2(monthly_data_all_yr_trim)

monthly_data_all04_date_cols <- c("date_added","dead_date","inception_date","date","chgdt")
for (i in 1:length(monthly_data_all04_date_cols))
{
  #i <- 1
  #i <- 2
  monthly_data_all04[,monthly_data_all04_date_cols[i]] <- as.Date(monthly_data_all04[,monthly_data_all04_date_cols[i]], 
                                                                  format="%Y-%m-%d", 
                                                                  origin="1970-01-01")
}
rm2(monthly_data_all04_date_cols,i)

#Scale AUM and Minimum Invstment Size
monthly_data_all04[,"aum"] <- (as.numeric(monthly_data_all04[,"aum"])/1000000)
monthly_data_all04[,"aum_lag1"] <- (as.numeric(monthly_data_all04[,"aum_lag1"])/1000000)
monthly_data_all04[,"aum_lag2"] <- (as.numeric(monthly_data_all04[,"aum_lag2"])/1000000)
monthly_data_all04[,"aum_lag3"] <- (as.numeric(monthly_data_all04[,"aum_lag3"])/1000000)
monthly_data_all04[,"minimum_investment_size"] <- (as.numeric(monthly_data_all04[,"minimum_investment_size"])/1000000)

monthly_data_all04[,"aum_lag4"] <- (as.numeric(monthly_data_all04[,"aum_lag4"])/1000000)

#Strip out comments in parenetheses
monthly_data_all_strip_comments_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                          "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")

#Rename original columns
monthly_data_all05 <- rename.vars(monthly_data_all04, 
                                  monthly_data_all_strip_comments_cols, 
                                  paste(monthly_data_all_strip_comments_cols,"_org",sep=""))

rm2(monthly_data_all04)

strip_cols <- c(monthly_data_all_strip_comments_cols, 
                paste(monthly_data_all_strip_comments_cols,"_comments",sep=""))

monthly_data_all06 <-  data.frame(monthly_data_all05, 
                                  matrix(NA, ncol=length(strip_cols), nrow=nrow(monthly_data_all05), dimnames=list(c(), strip_cols)), 
                                  stringsAsFactors=FALSE)

monthly_data_all06 <- monthly_data_all06[,sort(colnames(monthly_data_all06), decreasing = FALSE)]

monthly_data_all06 <- strip_comments(monthly_data_all06,monthly_data_all_strip_comments_cols)
monthly_data_all06 <- as.data.frame(monthly_data_all06,stringsAsFactors=FALSE)

rm2(monthly_data_all05,monthly_data_all_strip_comments_cols,strip_cols)

#Get text before comments
monthly_data_all_yn_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                              "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all07 <- create_noncomments(monthly_data_all06,monthly_data_all_yn_cols)
monthly_data_all07 <- as.data.frame(monthly_data_all07,stringsAsFactors=FALSE)

rm2(monthly_data_all06,monthly_data_all_yn_cols)

#Check for uknowns
monthly_data_all_check_unknown_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                         "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all08 <- data.table(monthly_data_all07)[, (monthly_data_all_check_unknown_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), 
                                                     .SDcols = monthly_data_all_check_unknown_cols]
monthly_data_all08 <- as.data.frame(monthly_data_all08, stringsAsFactors=FALSE)

rm2(monthly_data_all07,monthly_data_all_check_unknown_cols)

#Change not specificied to NA
NA_Phrases <- c("NA","N/A","N\\A","NOT APPLICABLE","NOT APPILCABLE","NOT DEFINED","NOT DISCLOSED","NOT DISLCOSED","UNDISCLOSED",
                "TO BE ADVISED","TO BE ADVISE","TBA","SEE PROSPECTUS FOR FULL DETAILS","UPON REQUEST",
                "SUBJECT TO MANAGER'S DISCRETION")
monthly_data_all_not_specified_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                         "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all09 <- not_specified_to_na(monthly_data_all08,monthly_data_all_not_specified_cols,NA_Phrases)
monthly_data_all09 <- as.data.frame(monthly_data_all09,stringsAsFactors=FALSE)

rm2(monthly_data_all08,monthly_data_all_not_specified_cols,NA_Phrases)

#Change no phrases to NO
NO_Phrases <- c("NIL","NONE","NONE AFTER 12 MONTHS","NONE AFTER 1ST YEAR","NO DIVIDEND","NON DIVIDEND","LITTLE OR NO")

monthly_data_all_no_phrases_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                      "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all10 <- no_to_no(monthly_data_all09,monthly_data_all_no_phrases_cols,NO_Phrases)
monthly_data_all10 <- as.data.frame(monthly_data_all10,stringsAsFactors=FALSE)

rm2(monthly_data_all09,monthly_data_all_no_phrases_cols,NO_Phrases)

#Change yes phrases to YES
YES_Phrases <- c("RARELY","OCCASIONALLY")
monthly_data_all_yes_phrases_cols <- c("annualized_target_return","annualized_target_volatility","domicile","fund_size_us_m",
                                       "leverage","lock_up","redemption_frequency","redemption_notification_period","subscription_frequency")
monthly_data_all11 <- yes_to_yes(monthly_data_all10,monthly_data_all_yes_phrases_cols,YES_Phrases)
monthly_data_all11 <- as.data.frame(monthly_data_all11,stringsAsFactors=FALSE)

rm2(monthly_data_all10,monthly_data_all_yes_phrases_cols,YES_Phrases)

#Change Y/N to binary
monthly_data_all_yn_to_bin_cols <-  c("leverage", "lock_up")

bin_cols <- paste(monthly_data_all_yn_to_bin_cols,"_bin",sep="")

monthly_data_all12 <-  data.frame(monthly_data_all11, matrix(NA, ncol=length(bin_cols), nrow=nrow(monthly_data_all11), dimnames=list(c(), bin_cols)), stringsAsFactors=FALSE)

monthly_data_all12[,bin_cols] <-  monthly_data_all12[,monthly_data_all_yn_to_bin_cols]

monthly_data_all12 <- yn_to_binary(monthly_data_all12,bin_cols)
monthly_data_all12 <- as.data.frame(monthly_data_all12,stringsAsFactors=FALSE)

monthly_data_all12 <- data.table(monthly_data_all12)[, (bin_cols) := llply(.SD, vector_clean_na,unknowns=unknowns_strings,.progress = "text"), .SDcols = bin_cols]
monthly_data_all12 <- as.data.frame(monthly_data_all12,stringsAsFactors=FALSE)

rm2(monthly_data_all11,monthly_data_all_yn_to_bin_cols,bin_cols)

#Create domicile dummy
monthly_data_all13 <- data.frame(monthly_data_all12,
                                 domicile_onshore_bin=0,
                                 stringsAsFactors=FALSE)

rm2(monthly_data_all12)

monthly_data_all13[,"domicile"] <- gsub("[[:punct:]]", "", monthly_data_all13[,"domicile"])
monthly_data_all13[,"domicile"] <- trim(monthly_data_all13[,"domicile"])


monthly_data_all13[,"domicile_onshore_bin"] <- ifelse(is.na(monthly_data_all13[,"domicile"]), NA, 
                                                      ifelse(toupper(monthly_data_all13[,"domicile"])=="UNITED STATES", 1, 
                                                             ifelse(toupper(monthly_data_all13[,"domicile"])=="USA", 1, 
                                                                    ifelse(toupper(monthly_data_all13[,"domicile"])=="US", 1, monthly_data_all13[,"domicile_onshore_bin"]))))


#Convert size to numeric
monthly_data_all_num_cols <- c("fund_size_us_m")

for (i in 1:length(monthly_data_all_num_cols)) {
  #i <- 1
  monthly_data_all13[,monthly_data_all_num_cols[i]] <- destring(monthly_data_all13[,monthly_data_all_num_cols[i]])
  monthly_data_all13[,monthly_data_all_num_cols[i]] <- as.numeric(monthly_data_all13[,monthly_data_all_num_cols[i]])
}

#Remove unwanted columns
monthly_data_all14 <- monthly_data_all13[,!(colnames(monthly_data_all13) %in% c("lock_up","lock_up_comments","lock_up_org",
                                                                                "leverage","leverage_comments","leverage_org",
                                                                                "domicile","domicile_comments","domicile_org",
                                                                                "fund_size_us_m_comments","fund_size_us_m_org",
                                                                                "annualized_target_return_comments","annualized_target_return_org",
                                                                                "annualized_target_volatility_comments","annualized_target_volatility_org",
                                                                                "redemption_notification_period_comments","redemption_notification_period_org",
                                                                                "subscription_frequency_comments","subscription_frequency_org"))]

rm2(monthly_data_all13,monthly_data_all_num_cols)


#Make sure that fund has a strategy category

fund_type_remove <- monthly_data_all14[is.na(monthly_data_all14[,"main_investment_strategy"]),]
fund_type_remove1 <- unique(fund_type_remove[,identifier])
fund_type_remove2 <- !is.na(fund_type_remove1)

monthly_data_all15 <- monthly_data_all14[!(monthly_data_all14[,identifier] %in% fund_type_remove2),]

rm2(monthly_data_all14)


#Make sure funds have atleast 24 months of returns
firm <- count(monthly_data_all15, c(identifier))
firm_keep <- firm[firm[,"freq"]>=24,]
firm_keep <- firm_keep[!is.na(firm_keep[,c(identifier)]),]
row.names(firm_keep) <- seq(nrow(firm_keep))

monthly_data_all16 <- monthly_data_all15[(monthly_data_all15[,c(identifier)] %in% firm_keep[,c(identifier)]),]
row.names(monthly_data_all16) <- seq(nrow(monthly_data_all16))

rm(monthly_data_all15,firm,firm_keep)


#Trim AUM
monthly_data_all17 <- monthly_data_all16
monthly_data_all17 <- monthly_data_all16[!is.na(monthly_data_all16[,"aum"]),]

rm2(monthly_data_all16)

monthly_data_all18 <- monthly_data_all17
monthly_data_all18 <- monthly_data_all18[monthly_data_all18[,"aum"]>=0.1,]

rm2(monthly_data_all17)


#Finalize the data
monthly_data_all19 <- monthly_data_all18

rm2(monthly_data_all18)

monthly_data_all20 <- monthly_data_all19[rowSums(is.na(monthly_data_all19[,1:ncol(monthly_data_all19)]))<ncol(monthly_data_all19),]



monthly_data_all20 <- monthly_data_all20[order(monthly_data_all20[,identifier], 
                                               monthly_data_all20[,"yr"],
                                               monthly_data_all20[,"month"]),]

row.names(monthly_data_all20) <- seq(nrow(monthly_data_all20))

rm2(monthly_data_all19)

# aa <- unique(monthly_data_all11[,!(colnames(monthly_data_all11) %in% c("yr","month"))])
# 
# 
# bb <- unique(aa[,(colnames(aa) %in% c("subscription_frequency", "redemption_notification_period","redemption_frequency"))])
# 
# cols <- c("subscription_frequency", "redemption_notification_period","redemption_frequency")
# 
# bb1 <- data.frame(bb,
#                   matrix(0, ncol=length(cols)*3, nrow=nrow(bb), 
#                          dimnames=list(c(), c(paste(cols,"day",sep="_"),
#                                               paste(cols,"month",sep="_"),
#                                               paste(cols,"year",sep="_")))), 
#                   matrix(NA, ncol=length(cols)*3, nrow=nrow(bb), 
#                          dimnames=list(c(), c(paste(cols,"combined",sep="_"),
#                                               paste(cols,"converted",sep="_"),
#                                               paste(cols,"evaluated",sep="_")))), 
#                   stringsAsFactors=FALSE)
# 
# bb1 <- bb1[,sort(colnames(bb1))]
# 
# for (j in 1:length(cols)) {
#   
#   #j <- 1
#   #j <- 2
#   #j <- 3
#   
#   bb1[,paste(cols[j],"day",sep="_")] <- ifelse(is.na(bb1[,cols[j]]), NA, 
#                                                ifelse(grepl("day", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                       ifelse(grepl("daily", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                              bb1[,paste(cols[j],"day",sep="_")])))
#   
#   bb1[,paste(cols[j],"month",sep="_")] <- ifelse(is.na(bb1[,cols[j]]), NA, 
#                                                  ifelse(grepl("month", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                         ifelse(grepl("monthly", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                                bb1[,paste(cols[j],"month",sep="_")])))          
#   
#   bb1[,paste(cols[j],"year",sep="_")] <- ifelse(is.na(bb1[,cols[j]]), NA, 
#                                                 ifelse(grepl("year", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                        ifelse(grepl("annual", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#                                                               bb1[,paste(cols[j],"year",sep="_")])))    
#   
#   bb1[,paste(cols[j],"combined",sep="_")] <- rowSums(bb1[,c(paste(cols[j],"day",sep="_"),
#                                                             paste(cols[j],"month",sep="_"),
#                                                             paste(cols[j],"year",sep="_"))],na.rm=TRUE)
#   bb1[,paste(cols[j],"combined",sep="_")] <- ifelse((is.na(bb1[,paste(cols[j],"day",sep="_")]) 
#                                                      & is.na(bb1[,paste(cols[j],"month",sep="_")]) 
#                                                      & is.na(bb1[,paste(cols[j],"month",sep="_")])), NA, bb1[,paste(cols[j],"combined",sep="_")])
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("[[:alpha:]]", "", bb1[,cols[j]])
#   #bb1[,paste(cols[j],"converted",sep="_")] <- gsub("([0-9]+).*$", "\\1", bb1[,cols[j]])
#   #bb1[,paste(cols[j],"converted",sep="_")] <- gsub('.*-([0-9]+).*','\\1',bb1[,cols[j]])
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("'","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("+","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub(";","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("$","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("%","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub(",","",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("/","-",bb1[,paste(cols[j],"converted",sep="_")])
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("  "," ",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("  "," ",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("  "," ",bb1[,paste(cols[j],"converted",sep="_")])
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("  "," ",bb1[,paste(cols[j],"converted",sep="_")])
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- trim(bb1[,paste(cols[j],"converted",sep="_")])
#   
#   #bb1[,paste(cols[j],"converted",sep="_")] <- gsub(" ","1",bb1[,paste(cols[j],"converted",sep="_")]) 
#   
#   # bb1[,paste(cols[j],"converted",sep="_")] <- ifelse(is.na(bb1[,cols[j]]), NA, 
#   #                                                ifelse(grepl("year", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#   #                                                       ifelse(grepl("annual", bb1[,cols[j]], ignore.case = TRUE, perl=TRUE), 1, 
#   #                                                              bb1[,paste(cols[j],"year",sep="_")])))  
#   
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- ifelse(is.na(bb1[,paste(cols[j],"combined",sep="_")]), NA, 
#                                                      ifelse(bb1[,paste(cols[j],"combined",sep="_")]==0, NA, 
#                                                             ifelse(bb1[,paste(cols[j],"combined",sep="_")]>1, NA, 
#                                                                    bb1[,paste(cols[j],"converted",sep="_")])))    
#   bb1[,paste(cols[j],"converted",sep="_")] <- ifelse(bb1[,paste(cols[j],"converted",sep="_")]==" ", "1", bb1[,paste(cols[j],"converted",sep="_")])  
#   bb1[,paste(cols[j],"converted",sep="_")] <- ifelse(bb1[,paste(cols[j],"converted",sep="_")]=="-", NA, bb1[,paste(cols[j],"converted",sep="_")]) 
#   
#   bb1[,paste(cols[j],"converted",sep="_")] <- gsub("/[[:punct:]]?$/","",bb1[,paste(cols[j],"converted",sep="_")])
#   
# } 



###############################################################################
cat("IMPORT AND FIX TEXT DATA", "\n")
###############################################################################

query_text_stats_ios_full <- ""
query_text_stats_ios_full <- paste(query_text_stats_ios_full, "select       ",text_stats_ios_import_vars_keep2, sep=" ")
query_text_stats_ios_full <- paste(query_text_stats_ios_full, "from         ",text_table, "                  ", sep=" ")
query_text_stats_ios_full <- trim(gsub(" {2,}", " ", query_text_stats_ios_full))

#text_stats_ios_full <- runsql("SELECT * FROM text_stats_ios",descriptive_stats_db)

text_stats_ios_full <- data.frame(runsql(query_text_stats_ios_full,descriptive_stats_db),
                                  avg_grade_level_acf_ios=NA,
                                  avg_grade_level_ac_ios=NA,
                                  yr_month=NA,
                                  stringsAsFactors=FALSE)

#Create additonal average readbility measures
text_stats_ios_full[,"avg_grade_level_acf_ios"] <- rowMeans(text_stats_ios_full[,c("ari_ios","coleman_liau_ios","flesch_kincaid_ios")],na.rm=TRUE)
text_stats_ios_full[,"avg_grade_level_ac_ios"] <- rowMeans(text_stats_ios_full[,c("ari_ios","coleman_liau_ios")],na.rm=TRUE)
text_stats_ios_full[,"yr_month"] <- paste(text_stats_ios_full[,"yr"],sprintf("%02d", text_stats_ios_full[,"month"]),sep="_")

colnames(text_stats_ios_full) <- tolower(colnames(text_stats_ios_full))

text_stats_ios_full1 <- text_stats_ios_full[!(text_stats_ios_full[,identifier] %in% fund_type_remove2),]

rm2(text_stats_ios_full)

text_stats_ios_yr_trim <- text_stats_ios_full1[(text_stats_ios_full1[,"yr"]>=beg_year & text_stats_ios_full1[,"yr"]<=end_year),]

rm2(text_stats_ios_full1)

text_stats_ios_sim_cols <- names(text_stats_ios_yr_trim)[grep("_similarity", names(text_stats_ios_yr_trim))] 

text_stats_ios <- text_stats_ios_yr_trim
for (i in 1:length(text_stats_ios_sim_cols))
{
  #i <- 1
  
  text_stats_ios <- text_stats_ios[!(is.na(text_stats_ios[,text_stats_ios_sim_cols[i]])),]
  
}

text_stats_ios_trim <- text_stats_ios[((!is.na(text_stats_ios[,"ari_ios"])) & 
                                         (!is.na(text_stats_ios[,"coleman_liau_ios"])) & 
                                         (!is.na(text_stats_ios[,"flesch_kincaid_ios"])) & 
                                         (!is.na(text_stats_ios[,"fog_ios"])) & 
                                         (!is.na(text_stats_ios[,"smog_ios"]))),]

rm2(text_stats_ios_import_vars_keep2,text_stats_ios_yr_trim,text_stats_ios_sim_cols,text_stats_ios)


###############################################################################
cat("MERGE FUND AND TEXT DATA", "\n")
###############################################################################

data0 <- merge(monthly_data_all20[,!(colnames(monthly_data_all20) %in% c("main_investment_strategy"))], text_stats_ios_trim, 
               by.x=c(identifier,"yr","month","yr_month"), by.y=c(identifier,"yr","month","yr_month"), 
               all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

data0 <- data0[order(data0[,identifier],
                     data0[,"yr"],
                     data0[,"month"],
                     data0[,"yr_month"]),]
row.names(data0) <- seq(nrow(data0))

rm2(fund_table,text_table,fund_type_remove,fund_type_remove2)
#rm2(monthly_data_all20,text_stats_ios_trim)


###############################################################################
cat("IMPORT AND FIX FACTOR DATA", "\n")
###############################################################################


###############################################################################
cat("MERGE IN FACTORS AND ALPHAS", "\n")
###############################################################################


###############################################################################
cat("COMPUTE ALPHAS", "\n")
###############################################################################


###############################################################################
cat("MERGE IN FUNDS AND ALPHAS", "\n")
###############################################################################

data2_no_na <- data0

rm2(data0)

###############################################################################
cat("COMPUTE ADDITIONAL VARIABLES", "\n")
###############################################################################

#EXRET_squared
#EXRET_neg
#ALPHAS_squared
#ALPHAs_neg


###############################################################################
cat("WINSORIZE", "\n")
###############################################################################

winsorize_vars <- c("nflow","nflow_lag1","nflow_lag2","nflow_lag3","nflow_lag4",
                    "sdnet_flow","sdnet_flow_lag1",
                    "pflow","pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                    "sdpct_flow","sdpct_flow_lag1",
                    "exret","exret_lag1","exret_lag2","exret_lag3","exret_lag4",
                    "sharpe_ratio","sortino_ratio","minimum_investment_size")

data2 <- data2_no_na
for (i in 1:length(winsorize_vars))
{
  #i <- 1
  #i <- 2
  data2[,winsorize_vars[i]] <- 
    winsorize_both(data2[,winsorize_vars[i]],q=0.025)
  
} 
rm2(data2_no_na,winsorize_vars,i)


###############################################################################
cat("COMPUTE QUANTILES - IOS", "\n")
###############################################################################

descriptive_stats_by_var_year <- "yr"

fund_count_yr1 <- ddply(data2, descriptive_stats_by_var_year, function(x) {data.frame(var="number_of_funds", 
                                                                                      count=as.numeric(length(unique(x[,identifier],comparables=FALSE))),
                                                                                      stringsAsFactors=FALSE)})
fund_count_yr2 <- data.frame(temp_var="ZZZ",
                             var="number_of_funds", 
                             count=as.numeric(length(unique(data2[,identifier],comparables=FALSE))),
                             stringsAsFactors=FALSE)
colnames(fund_count_yr2)[match("temp_var",names(fund_count_yr2))] <- descriptive_stats_by_var_year

fund_count_yr <- rbind(fund_count_yr1,fund_count_yr2)

rm(fund_count_yr1,fund_count_yr2)

data_temp_no_id1 <- data2[,!(colnames(data2) %in% identifier)]
data_temp_no_id2 <- data_temp_no_id1[,!(colnames(data_temp_no_id1) %in% c("month","yr_month","chgdt","date","date_added","dead_date","inception_date"))]

rm(data_temp_no_id1)

data_temp_no_id3 <- data_temp_no_id2[,!(sapply(data_temp_no_id2, is.character))]
row.names(data_temp_no_id3) <- seq(nrow(data_temp_no_id3))

rm(data_temp_no_id2)

#descriptive_stats_temp_full_all_var_year <- describeBy2(descrip_stats_fund2,"yr")
#descriptive_stats_temp_full_all_var_year <- describeBy2(data_temp_no_id[,c(descriptive_stats_by_var_year,descriptive_overall_vars_model_vars_all[,c("var")])],descriptive_stats_by_var_year)

descriptive_stats_temp_full_all_var_year <- describeBy2(data_temp_no_id3, descriptive_stats_by_var_year)


###############################################################################
cat("COMPUTE DV FOR ABOVE AND BELOW SIMILARITY/READABILITY QUANTILE - IOS", "\n")
###############################################################################

quantile_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                       "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                       "all_similarity_050pct_ios","main_investment_strategy_similarity_050pct_ios",
                       "all_similarity_100pct_ios","main_investment_strategy_similarity_100pct_ios",
                       "all_similarity_250pct_ios","main_investment_strategy_similarity_250pct_ios",
                       "all_similarity_500pct_ios","main_investment_strategy_similarity_500pct_ios",
                       "all_similarity_750pct_ios","main_investment_strategy_similarity_750pct_ios",
                       "all_similarity_900pct_ios","main_investment_strategy_similarity_900pct_ios")


quantile_vars_data_ios <- descriptive_stats_temp_full_all_var_year[tolower(descriptive_stats_temp_full_all_var_year[,"var"]) %in% quantile_vars_ios,
                                                                   c("yr","var","quartile1","quartile3")] 

quantile_vars_dv_temp_ios <- lapply(quantile_vars_ios,quantile_dvs,
                                    data=data2,
                                    group_var=c(identifier,"yr","month"),quantile_data=quantile_vars_data_ios,
                                    quantile_col_low="quartile1",quantile_col_high="quartile3")

quantile_vars_dv_temp2_ios <- do.call(cbind, quantile_vars_dv_temp_ios)
quantile_vars_dv_temp2_ios <- quantile_vars_dv_temp2_ios[order(quantile_vars_dv_temp2_ios[,identifier],
                                                               quantile_vars_dv_temp2_ios[,"yr"],
                                                               quantile_vars_dv_temp2_ios[,"month"]),]
row.names(quantile_vars_dv_temp2_ios) <- seq(nrow(quantile_vars_dv_temp2_ios))

quantile_vars_dv_temp2_ios <- quantile_vars_dv_temp2_ios[,unique(colnames(quantile_vars_dv_temp2_ios))]

rm2(quantile_vars_ios,quantile_vars_data_ios,quantile_vars_dv_temp_ios)


###############################################################################
cat("MERGE QUANTILE DVs", "\n")
###############################################################################

quantile_vars_dv <- quantile_vars_dv_temp2_ios
quantile_vars_dv <- quantile_vars_dv[order(quantile_vars_dv[,identifier],
                                           quantile_vars_dv[,"yr"],
                                           quantile_vars_dv[,"month"]),]
row.names(quantile_vars_dv) <- seq(nrow(quantile_vars_dv))

data_all <- merge(data2, quantile_vars_dv, 
                  by.x=c(identifier,"yr","month"), by.y=c(identifier,"yr","month"),
                  all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"))

data_all <- data_all[order(data_all[,identifier],
                           data_all[,"yr"],
                           data_all[,"month"],
                           data_all[,"yr_month"]),]
row.names(data_all) <- seq(nrow(data_all))


###############################################################################
cat("OUTPUT DATA", "\n")
###############################################################################

ExportTable(data_fulll_db,"data_prescreen",data_all)
write.csv(data_all,file=paste(output_directory,"data_prescreen.csv",sep="\\"),na="",quote=TRUE,row.names=FALSE)

# aa <- unique(data_all[,c("fund_id","yr","ari_ios_below_quartile1","ari_ios_above_quartile3","coleman_liau_ios_below_quartile1",
#                          "coleman_liau_ios_above_quartile3","flesch_kincaid_ios_below_quartile1","flesch_kincaid_ios_above_quartile3",
#                          "fog_ios_below_quartile1","fog_ios_above_quartile3","smog_ios_below_quartile1","smog_ios_above_quartile3",
#                          "avg_grade_level_ios_below_quartile1","avg_grade_level_ios_above_quartile3","avg_grade_level_ac_ios_below_quartile1",
#                          "avg_grade_level_ac_ios_above_quartile3","avg_grade_level_acf_ios_below_quartile1",
#                          "avg_grade_level_acf_ios_above_quartile3","all_similarity_050pct_ios_below_quartile1",
#                          "all_similarity_050pct_ios_above_quartile3","main_investment_strategy_similarity_050pct_ios_below_quartile1",
#                          "main_investment_strategy_similarity_050pct_ios_above_quartile3","all_similarity_100pct_ios_below_quartile1",
#                          "all_similarity_100pct_ios_above_quartile3","main_investment_strategy_similarity_100pct_ios_below_quartile1",
#                          "main_investment_strategy_similarity_100pct_ios_above_quartile3","all_similarity_250pct_ios_below_quartile1",
#                          "all_similarity_250pct_ios_above_quartile3","main_investment_strategy_similarity_250pct_ios_below_quartile1",
#                          "main_investment_strategy_similarity_250pct_ios_above_quartile3","all_similarity_500pct_ios_below_quartile1",
#                          "all_similarity_500pct_ios_above_quartile3","main_investment_strategy_similarity_500pct_ios_below_quartile1",
#                          "main_investment_strategy_similarity_500pct_ios_above_quartile3","all_similarity_750pct_ios_below_quartile1",
#                          "all_similarity_750pct_ios_above_quartile3","main_investment_strategy_similarity_750pct_ios_below_quartile1",
#                          "main_investment_strategy_similarity_750pct_ios_above_quartile3","all_similarity_900pct_ios_below_quartile1",
#                          "all_similarity_900pct_ios_above_quartile3")])
# 
# bb <- count(aa,c("fund_id","yr"))
# 
# cc <- unique(bb[,"freq"])