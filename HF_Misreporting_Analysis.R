# TODO: Add comment
# 
# Author:  Brad
# File:    HF_Misreporting_Analysis.R
# Version: 1.0
# Date:    08.26.2014
# Purpose: Calculate Text Measures
#
###############################################################################

###############################################################################
# INITIAL SETUP;
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

# Clear workspace
rm(list = ls(all = TRUE))

# Limit History to not exceed 50 lines
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

# Set location (1=HOME,2=WORK,3=LAPTOP,4=CORALSEA FROM HOME,5=CORALSEA FROM WORK,6=CORALSEA FROM LAPTOP)
Location <- 1

if (Location == 1) {
  
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else if (Location == 2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  
} else if (Location == 4) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE) 
  
} else if (Location == 5) {
  
  input_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else if (Location == 6) {
  
  input_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research/Hedge_Fund_Misreporting/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp4/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4/", winslash = "\\", mustWork = TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S. Brad Daughdrill/Documents/My Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else {
  
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
# FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("compare","cwhmisc","data.table","fastmatch","foreign","formatR","gdata","gtools",
                       "Hmisc","koRpus","mitools","pbapply","plyr","R.oo","reshape2","rJava","RWeka","RWekajars",
                       "sqldf","stringr","tcltk","tm")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(external_packages,installed_packages,repo)


###############################################################################
# PREALLOCATE DATA;
cat("SECTION: PREALLOCATE DATA", "\n")
###############################################################################

#Create base column table
#temp_data_cols <- as.data.frame(matrix(NA, ncol=7, nrow=200),stringsAsFactors=FALSE)
#colnames(temp_data_cols) <- c("order","isnum","ischar","isdate","isfactor","colnames","desc")

temp_data_cols <- data.frame(matrix(NA, ncol=7, nrow=200, 
                                    dimnames=list(c(), c("order","isnum","ischar","isdate","isfactor","colnames","desc"))), 
                             stringsAsFactors=FALSE)

temp_data_cols[,1] <- as.numeric(temp_data_cols[,1])
temp_data_cols[,2] <- as.numeric(temp_data_cols[,2])
temp_data_cols[,3] <- as.numeric(temp_data_cols[,3])
temp_data_cols[,4] <- as.numeric(temp_data_cols[,4])
temp_data_cols[,5] <- as.numeric(temp_data_cols[,5])
temp_data_cols[,6] <- as.character(temp_data_cols[,6])
temp_data_cols[,7] <- as.character(temp_data_cols[,7])

#Files table
file_list <- c("EurekahedgeHF_Excel_aca.csv","EurekahedgeHF_Excel_aca_NAV_AUM.csv","EurekahedgeHF_Excel_aca_Instruments_Traded.csv")
files_cols_count <- 2
files_cols <- temp_data_cols[1:files_cols_count,]
files_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filename",stringsAsFactors=FALSE)
files_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filepath",stringsAsFactors=FALSE)
files <- as.data.frame(matrix(NA, ncol=files_cols_count, nrow=length(file_list)),stringsAsFactors=FALSE)
colnames(files) <- files_cols[,6]
files <- format_function(files,files_cols)


#Populate Percentiles table

#Note: If Confidence_Level is 0.900, 
#          word_grand  -  this means that if the word is the 10% most common occuring or higher overall word, it is removed (90% removed/10% kept)
#          word_unique -  this means that if the word is the 10% most common occuring or higher unique word, it is removed (90% removed/10% kept)
#          id_unique -  this means that if a word is in 10% of the ids or higher, it is removed  (90% removed/10% kept)
#Note: If Confidence_Level is 0.050, 
#          word_grand  -  this means that if the word is the 95% most common occuring or higher overall word, it is removed (5% removed/95% kept)
#          word_unique -  this means that if the word is the 95% most common occuring or higher unique word, it is removed (5% removed/95% kept)
#          id_unique -  this means that if a word is in 95% of the ids or higher, it is removed  (5% removed/95% kept)
# As confidence level increases, the words in the dictionary should decrease.
# A smaller dicitonary means that that smilarity will be less because there are fewer possible words that could be common between two texts 
# Thus, as confidence level increases, the smilarity scores will decrease

#Percentiles table
percentiles_cols_count <- 8
percentiles_cols <- temp_data_cols[1:percentiles_cols_count,]
percentiles_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Confidence_Level",stringsAsFactors=FALSE)
percentiles_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Confidence_Pct",stringsAsFactors=FALSE)
percentiles_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Confidence_lbl",stringsAsFactors=FALSE)
percentiles_cols[4,] <- data.frame(order=4,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Significance_Level",stringsAsFactors=FALSE)
percentiles_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Significance_Pct",stringsAsFactors=FALSE)
percentiles_cols[6,] <- data.frame(order=6,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Significance_lbl",stringsAsFactors=FALSE)
percentiles_cols[7,] <- data.frame(order=7,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Column_lbl",stringsAsFactors=FALSE)
percentiles_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Column_DV",stringsAsFactors=FALSE)

#percentile_vals <- c(0.990,0.950,0.900)
#percentile_vals <- c(0.900,0.750,0.500,0.250,0.100,0.050)
percentile_vals <- c(0.900)

percentiles <- as.data.frame(matrix(NA, ncol=percentiles_cols_count, nrow=length(percentile_vals)),stringsAsFactors=FALSE)
colnames(percentiles) <- percentiles_cols[,6]

percentiles[,"Confidence_Level"] <- format(as.double(percentile_vals), digits=3)
percentiles[,"Confidence_Pct"] <- as.double(percentiles[,"Confidence_Level"])*100
percentiles[,"Confidence_lbl"] <- formatC(percentiles[,"Confidence_Pct"],format="f", digits=1,width=4,  flag="0")
percentiles[,"Significance_Level"] <- 1-as.double(percentiles[,"Confidence_Level"])
percentiles[,"Significance_Pct"] <- as.double(percentiles[,"Significance_Level"])*100
percentiles[,"Significance_lbl"] <- formatC(percentiles[,"Significance_Pct"], format="f",digits=1, width=5,  flag="0")
percentiles[,"Confidence_lbl"] <-  paste(gsub(pattern="\\.", replacement="", x=percentiles[,"Confidence_lbl"]),"pct",sep="")
percentiles[,"Significance_lbl"] <- paste(gsub(pattern="\\.", replacement="", x=percentiles[,"Significance_lbl"]),"pct",sep="")
percentiles[,"Column_lbl"] <- paste("Word_Cutoff_",percentiles[,"Confidence_lbl"],sep="")
percentiles[,"Column_DV"] <- paste("Word_DV_",percentiles[,"Confidence_lbl"],sep="")
percentiles <- format_function(percentiles,percentiles_cols)
percentiles <- percentiles[order(percentiles[,"Confidence_Level"]),]


#Readability columns table
readbl_vars_cols_count <- 4
readbl_vars_cols <- temp_data_cols[1:readbl_vars_cols_count,]
readbl_vars_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="column",stringsAsFactors=FALSE)
readbl_vars_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="suffix",stringsAsFactors=FALSE)
readbl_vars_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="readabilitystats_table",stringsAsFactors=FALSE)
readbl_vars_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token_table",stringsAsFactors=FALSE)
readbl_vars <- as.data.frame(matrix(NA, ncol=readbl_vars_cols_count, nrow=1),stringsAsFactors=FALSE)
colnames(readbl_vars) <- readbl_vars_cols[,6]

#Readability statistics table
readbl_all_df_cols_count <- 5
readbl_all_df_cols <- temp_data_cols[1:readbl_all_df_cols_count,]
readbl_all_df_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="index",stringsAsFactors=FALSE)
readbl_all_df_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="flavour",stringsAsFactors=FALSE)
readbl_all_df_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="raw",stringsAsFactors=FALSE)
readbl_all_df_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="grade",stringsAsFactors=FALSE)
readbl_all_df_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="age",stringsAsFactors=FALSE)
readbl_all_df <- as.data.frame(matrix(NA, ncol=readbl_all_df_cols_count, nrow=44),stringsAsFactors=FALSE)
colnames(readbl_all_df) <- readbl_all_df_cols[,6]
readbl_all_df <- format_function(readbl_all_df,readbl_all_df_cols)

#Sample data input columns
#sample_data_input_cols_count <- 8
#sample_data_input_cols <- temp_data_cols[1:sample_data_input_cols_count,]
#sample_data_input_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ID",desc="input",stringsAsFactors=FALSE)
#sample_data_input_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",desc="input",stringsAsFactors=FALSE)
#sample_data_input_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="crsp_fundno",desc="input",stringsAsFactors=FALSE)
#sample_data_input_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_f",desc="input",stringsAsFactors=FALSE)
#sample_data_input_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_strategy_f",desc="input",stringsAsFactors=FALSE)
#sample_data_input_cols[6,] <- data.frame(order=6,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_strategy_f",desc="input",stringsAsFactors=FALSE)
#sample_data_input_cols[7,] <- data.frame(order=7,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="principal_risks_f",desc="input",stringsAsFactors=FALSE)
#sample_data_input_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="File",desc="input",stringsAsFactors=FALSE)

#Tokens table
tokens_all_cols_count <- 5
tokens_all_cols <- temp_data_cols[1:tokens_all_cols_count,]
tokens_all_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ID",stringsAsFactors=FALSE)
tokens_all_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",stringsAsFactors=FALSE)
tokens_all_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token",stringsAsFactors=FALSE)
tokens_all_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="desc",stringsAsFactors=FALSE)
tokens_all_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Remove",stringsAsFactors=FALSE)


#==============================================================================;
cat("SECTION: POPULATE DATA", "\n")
#==============================================================================;

#List variables to compute readability statistics and the suffixes used for each in the sample_data_statistics_cols table
readbl_vars[,1] <- c("Strategy")
readbl_vars[,2] <- c("_ios")
readbl_vars[,3] <- c("read_stats_ios_f")
readbl_vars[,4] <- c("tokens_all_ios_f")

#measures <- c("word_grand","word_unique","id_unique")
measures <- c("id_unique")

# remove stopwords
myStopwords <- c(stopwords('english'),stopwords('SMART'),"available", "via")
myStopwords_no_punct <- gsub(pattern="[^[[:alnum:][:space:]]", replacement="", x=myStopwords)
myStopwords_all <- c(myStopwords,myStopwords_no_punct)
myStopwords_all <- sort(myStopwords_all)
myStopwords_all <- unique(myStopwords_all, incomparables=FALSE)
myStopwords_all <- toupper(myStopwords_all)

#idx <- which(myStopwords_all %in% c("R",keep_one_letter_tokens,keep_two_letter_tokens))
idx <- which(myStopwords_all %in% c("R"))
myStopwords_all <- myStopwords_all[-idx]

keep_one_letter_words <- c("I")
keep_one_letter_ratings <- c("A","B","C","D","P")
keep_one_letter_tokens <- sort(c(keep_one_letter_words,keep_one_letter_ratings))

keep_two_letter_words <- c("AM","AN","AS","AT","BE","BY","DO","EG","EX","HA","ID","IE","IF","IN","IS",
                           "IT","MY","NO","OF","ON","OR","QA","RD","SO","SP","TM","TO","TV","UM","UN",
                           "UP","US","VP","WE")
keep_state_abbreviations <- c("AK","AL","AR","AZ","CA","CF","CL","CO","CT","DC","DE","DL","FL","GA","HA",
                              "HI","IA","ID","IL","IN","KA","KS","KY","LA","MA","MC","MD","ME","MI","MN",
                              "MO","MS","MT","NB","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR",
                              "PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WN","WS","WV","WY")
keep_two_letter_ratings <- c("AA","BA","BB","CA","CC")
keep_two_letter_tokens <- sort(c(keep_two_letter_words,keep_state_abbreviations,keep_two_letter_ratings))


###############################################################################
cat("SECTION: IMPORT DATA", "\n")
###############################################################################

files[,1] <-  file_list
files[,2] <-  unlist(mapply(merge_cols_function,col_one=input_directory,col_two=files[,1],separator="", SIMPLIFY=FALSE,USE.NAMES=FALSE))

for (j in 1:nrow(files))
{
  #j <- 1
  #j <- 2
  
  sample_data <- read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
  for(i in which(sapply(sample_data,class)=="character"))
  {
    sample_data[[i]] = trim(sample_data[[i]])
  }
  for (i in 1:ncol(sample_data))
  {
    sample_data[,i] <- unknownToNA(sample_data[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                              NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                              NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
    sample_data[,i] <- ifelse(is.na(sample_data[,i]),NA, sample_data[,i])
  } 
  
  sample_data <- sample_data[rowSums(is.na(sample_data[,1:ncol(sample_data)]))<ncol(sample_data),]
  
  row.names(sample_data) <- seq(nrow(sample_data))
  
  
  ###############################################################################
  # OUTPUT DATA;
  ###############################################################################
  
  write.csv(sample_data, file=paste(output_directory,file=files[j,1],sep=""),row.names=FALSE)
  
  ###############################################################################
  # CREATE PROGRESS OUTPUTS;
  ###############################################################################
  
  progress_function(outer_loop_count=j, outer_loop_start_val=1, outer_loop_end_val=nrow(files), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
}


###############################################################################
cat("SECTION: CLEAN EurekahedgeHF_Excel_aca_NAV_AUM", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_NAV_AUM <- read.csv(file=paste(output_directory,files[2,1],sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(EurekahedgeHF_Excel_aca_NAV_AUM,class)=="character"))
{
  EurekahedgeHF_Excel_aca_NAV_AUM[[i]] = trim(EurekahedgeHF_Excel_aca_NAV_AUM[[i]])
}
for (i in 1:ncol(EurekahedgeHF_Excel_aca_NAV_AUM))
{
  EurekahedgeHF_Excel_aca_NAV_AUM[,i] <- unknownToNA(EurekahedgeHF_Excel_aca_NAV_AUM[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                                    NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                                    NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  EurekahedgeHF_Excel_aca_NAV_AUM[,i] <- ifelse(is.na(EurekahedgeHF_Excel_aca_NAV_AUM[,i]),NA, EurekahedgeHF_Excel_aca_NAV_AUM[,i])
} 

EurekahedgeHF_Excel_aca_AUM <- EurekahedgeHF_Excel_aca_NAV_AUM[EurekahedgeHF_Excel_aca_NAV_AUM[,"Type"]=="AUM",]
EurekahedgeHF_Excel_aca_AUM <- EurekahedgeHF_Excel_aca_AUM[order(EurekahedgeHF_Excel_aca_AUM[,"Type"],
                                                                 EurekahedgeHF_Excel_aca_AUM[,"Fund.ID"]),]
row.names(EurekahedgeHF_Excel_aca_AUM) <- seq(nrow(EurekahedgeHF_Excel_aca_AUM))

EurekahedgeHF_Excel_aca_AUM_melt <- melt(EurekahedgeHF_Excel_aca_AUM, id=c("Type","Fund.ID","Fund.Name"), na.rm=FALSE)
EurekahedgeHF_Excel_aca_AUM_melt[,"variable"] <- gsub(pattern="X", replacement="", x=EurekahedgeHF_Excel_aca_AUM_melt[,"variable"])
EurekahedgeHF_Excel_aca_AUM_melt[,"variable"] <- gsub(pattern="\\.", replacement="-", x=EurekahedgeHF_Excel_aca_AUM_melt[,"variable"])
EurekahedgeHF_Excel_aca_AUM_melt[,"variable"] <- as.Date(EurekahedgeHF_Excel_aca_AUM_melt[,"variable"],format="%m-%d-%Y")

colnames(EurekahedgeHF_Excel_aca_AUM_melt)[match("variable",names(EurekahedgeHF_Excel_aca_AUM_melt))] <- "date"
colnames(EurekahedgeHF_Excel_aca_AUM_melt)[match("value",names(EurekahedgeHF_Excel_aca_AUM_melt))] <- "AUM"

EurekahedgeHF_Excel_aca_AUM_melt[,"AUM"] <- 
  ifelse(is.na(EurekahedgeHF_Excel_aca_AUM_melt[,"AUM"]), NA, EurekahedgeHF_Excel_aca_AUM_melt[,"AUM"])

EurekahedgeHF_Excel_aca_AUM_melt[,"AUM"] <- as.integer(EurekahedgeHF_Excel_aca_AUM_melt[,"AUM"])

EurekahedgeHF_Excel_aca_AUM_melt <- data.frame(EurekahedgeHF_Excel_aca_AUM_melt[,!names(EurekahedgeHF_Excel_aca_AUM_melt) %in% c("Type","Fund.Name")],
                                               yr=year(EurekahedgeHF_Excel_aca_AUM_melt[,"date"]),
                                               month=month(EurekahedgeHF_Excel_aca_AUM_melt[,"date"]),
                                               stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_NAV <- EurekahedgeHF_Excel_aca_NAV_AUM[EurekahedgeHF_Excel_aca_NAV_AUM[,"Type"]=="Return",]
EurekahedgeHF_Excel_aca_NAV <- EurekahedgeHF_Excel_aca_NAV[order(EurekahedgeHF_Excel_aca_NAV[,"Type"],
                                                                 EurekahedgeHF_Excel_aca_NAV[,"Fund.ID"]),]
row.names(EurekahedgeHF_Excel_aca_NAV) <- seq(nrow(EurekahedgeHF_Excel_aca_NAV))

EurekahedgeHF_Excel_aca_NAV_melt <- melt(EurekahedgeHF_Excel_aca_NAV, id=c("Type","Fund.ID","Fund.Name"), na.rm=FALSE)
EurekahedgeHF_Excel_aca_NAV_melt[,"variable"] <- gsub(pattern="X", replacement="", x=EurekahedgeHF_Excel_aca_NAV_melt[,"variable"])
EurekahedgeHF_Excel_aca_NAV_melt[,"variable"] <- gsub(pattern="\\.", replacement="-", x=EurekahedgeHF_Excel_aca_NAV_melt[,"variable"])
EurekahedgeHF_Excel_aca_NAV_melt[,"variable"] <- as.Date(EurekahedgeHF_Excel_aca_NAV_melt[,"variable"],format="%m-%d-%Y")

colnames(EurekahedgeHF_Excel_aca_NAV_melt)[match("variable",names(EurekahedgeHF_Excel_aca_NAV_melt))] <- "date"
colnames(EurekahedgeHF_Excel_aca_NAV_melt)[match("value",names(EurekahedgeHF_Excel_aca_NAV_melt))] <- "Monthly_Ret"

EurekahedgeHF_Excel_aca_NAV_melt[,"Monthly_Ret"] <- 
  ifelse(is.na(EurekahedgeHF_Excel_aca_NAV_melt[,"Monthly_Ret"]), NA, EurekahedgeHF_Excel_aca_NAV_melt[,"Monthly_Ret"])

EurekahedgeHF_Excel_aca_NAV_melt[,"Monthly_Ret"] <- as.numeric(EurekahedgeHF_Excel_aca_NAV_melt[,"Monthly_Ret"])

EurekahedgeHF_Excel_aca_NAV_melt <- data.frame(EurekahedgeHF_Excel_aca_NAV_melt[,!names(EurekahedgeHF_Excel_aca_NAV_melt) %in% c("Type","Fund.Name")],
                                               yr=year(EurekahedgeHF_Excel_aca_NAV_melt[,"date"]),
                                               month=month(EurekahedgeHF_Excel_aca_NAV_melt[,"date"]),
                                               stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_NAV_AUM_melt <- merge(EurekahedgeHF_Excel_aca_NAV_melt, EurekahedgeHF_Excel_aca_AUM_melt, 
                                              by.x=c("Fund.ID","date","yr","month"), by.y=c("Fund.ID","date","yr","month"), 
                                              all.x=TRUE, all.y=TRUE, sort=FALSE,suffixes=c(".x",".y"))

EurekahedgeHF_Excel_aca_NAV_AUM_melt <- EurekahedgeHF_Excel_aca_NAV_AUM_melt[rowSums(is.na(EurekahedgeHF_Excel_aca_NAV_AUM_melt[,1:ncol(EurekahedgeHF_Excel_aca_NAV_AUM_melt)]))<ncol(EurekahedgeHF_Excel_aca_NAV_AUM_melt),]

EurekahedgeHF_Excel_aca_NAV_AUM_melt <- EurekahedgeHF_Excel_aca_NAV_AUM_melt[order(EurekahedgeHF_Excel_aca_NAV_AUM_melt[,"Fund.ID"],
                                                                                   EurekahedgeHF_Excel_aca_NAV_AUM_melt[,"date"],
                                                                                   EurekahedgeHF_Excel_aca_NAV_AUM_melt[,"yr"],
                                                                                   EurekahedgeHF_Excel_aca_NAV_AUM_melt[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_NAV_AUM_melt) <- seq(nrow(EurekahedgeHF_Excel_aca_NAV_AUM_melt))



write.csv(EurekahedgeHF_Excel_aca_NAV_AUM_melt, file=paste(output_directory,file="EurekahedgeHF_Excel_aca_NAV_AUM_melt",".csv",sep=""),row.names=FALSE)

rm2(EurekahedgeHF_Excel_aca_NAV_AUM)
rm2(EurekahedgeHF_Excel_aca_AUM,EurekahedgeHF_Excel_aca_AUM_melt)
rm2(EurekahedgeHF_Excel_aca_NAV,EurekahedgeHF_Excel_aca_NAV_melt)


###############################################################################
cat("SECTION: CLEAN EurekahedgeHF_Excel_aca_Instruments_Traded", "\n")
###############################################################################

EurekahedgeHF_Excel_aca_Instruments_Traded <- read.csv(file=paste(output_directory,files[3,1],sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(EurekahedgeHF_Excel_aca_Instruments_Traded,class)=="character"))
{
  EurekahedgeHF_Excel_aca_Instruments_Traded[[i]] = trim(EurekahedgeHF_Excel_aca_Instruments_Traded[[i]])
}
for (i in 1:ncol(EurekahedgeHF_Excel_aca_Instruments_Traded))
{
  EurekahedgeHF_Excel_aca_Instruments_Traded[,i] <- unknownToNA(EurekahedgeHF_Excel_aca_Instruments_Traded[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                                                          NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                                                          NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  EurekahedgeHF_Excel_aca_Instruments_Traded[,i] <- ifelse(is.na(EurekahedgeHF_Excel_aca_Instruments_Traded[,i]),NA, EurekahedgeHF_Excel_aca_Instruments_Traded[,i])
} 

EurekahedgeHF_Excel_aca_Instruments_Traded  <- EurekahedgeHF_Excel_aca_Instruments_Traded[order(EurekahedgeHF_Excel_aca_Instruments_Traded[,"Fund.ID"],
                                                                                                EurekahedgeHF_Excel_aca_Instruments_Traded[,"Fund.Name"],
                                                                                                EurekahedgeHF_Excel_aca_Instruments_Traded[,"Instrument.Traded"]),]

row.names(EurekahedgeHF_Excel_aca_Instruments_Traded) <- seq(nrow(EurekahedgeHF_Excel_aca_Instruments_Traded))

EurekahedgeHF_Excel_aca_Instruments_Traded[,"Instrument.Traded"] <-  gsub(pattern=" ", replacement=".", x=EurekahedgeHF_Excel_aca_Instruments_Traded[,"Instrument.Traded"])
EurekahedgeHF_Excel_aca_Instruments_Traded[,"Instrument.Traded"] <-  gsub(pattern="\\.{2,}", replacement="\\.", x=EurekahedgeHF_Excel_aca_Instruments_Traded[,"Instrument.Traded"])
EurekahedgeHF_Excel_aca_Instruments_Traded[,"Instrument.Traded"] <-  gsub(pattern="-", replacement="_", x=EurekahedgeHF_Excel_aca_Instruments_Traded[,"Instrument.Traded"])

unique_instruments <-  unique(EurekahedgeHF_Excel_aca_Instruments_Traded[,"Instrument.Traded"])

#Instruments Traded
Instruments_Traded <- data.frame(EurekahedgeHF_Excel_aca_Instruments_Traded, 
                                 matrix(0, ncol=length(unique_instruments), nrow=nrow(EurekahedgeHF_Excel_aca_Instruments_Traded), dimnames=list(c(), paste("Instrument.Traded",unique_instruments,sep="_"))), 
                                 stringsAsFactors=FALSE)

for (i in 1:length(unique_instruments))
{
  
  Instruments_Traded[,paste("Instrument.Traded",unique_instruments[i],sep="_")] <- 
    ifelse(Instruments_Traded[,"Instrument.Traded"]==unique_instruments[i], 1, Instruments_Traded[,paste("Instrument.Traded",unique_instruments[i],sep="_")])
  
} 

Instruments_Traded_comb <- aggregate(Instruments_Traded[,(ncol(Instruments_Traded)-length(unique_instruments)+1):ncol(Instruments_Traded)], by=list(Instruments_Traded[,"Fund.ID"]), FUN=sum, na.rm=TRUE)
colnames(Instruments_Traded_comb)[1] <- "Fund.ID"

Instruments_Traded_comb <- Instruments_Traded_comb[,sort(colnames(Instruments_Traded_comb))]
Instruments_Traded_comb <- Instruments_Traded_comb[,c("Fund.ID",colnames(Instruments_Traded_comb)[-which(colnames(Instruments_Traded_comb) %in% "Fund.ID")])]

#Exposure
Exposure <- data.frame(EurekahedgeHF_Excel_aca_Instruments_Traded, 
                       matrix(NA, ncol=length(unique_instruments), nrow=nrow(EurekahedgeHF_Excel_aca_Instruments_Traded), dimnames=list(c(), paste("Exposure",unique_instruments,sep="_"))), 
                       stringsAsFactors=FALSE)

for (i in 1:length(unique_instruments))
{
  
  Exposure[,paste("Exposure",unique_instruments[i],sep="_")] <- 
    ifelse(Exposure[,"Instrument.Traded"]==unique_instruments[i], Exposure[,"Exposure"], Exposure[,paste("Exposure",unique_instruments[i],sep="_")])
  
} 

Exposure_comb <- dcast(Exposure, Fund.ID ~ Instrument.Traded, value.var = 'Exposure')
colnames(Exposure_comb)[2:ncol(Exposure_comb)] <- paste("Exposure",colnames(Exposure_comb)[2:ncol(Exposure_comb)],sep="_")
colnames(Exposure_comb)[1] <- "Fund.ID"

Exposure_comb <- Exposure_comb[,sort(colnames(Exposure_comb))]
Exposure_comb <- Exposure_comb[,c("Fund.ID",colnames(Exposure_comb)[-which(colnames(Exposure_comb) %in% "Fund.ID")])]


EurekahedgeHF_Excel_aca_Instruments_Traded_merge <- merge(Instruments_Traded_comb, Exposure_comb, by.x=c("Fund.ID"), by.y=c("Fund.ID"), 
                                                          all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)

EurekahedgeHF_Excel_aca_Instruments_Traded_merge <- EurekahedgeHF_Excel_aca_Instruments_Traded_merge[rowSums(is.na(EurekahedgeHF_Excel_aca_Instruments_Traded_merge[,1:ncol(EurekahedgeHF_Excel_aca_Instruments_Traded_merge)]))<ncol(EurekahedgeHF_Excel_aca_Instruments_Traded_merge),]

EurekahedgeHF_Excel_aca_Instruments_Traded_merge <- EurekahedgeHF_Excel_aca_Instruments_Traded_merge[order(EurekahedgeHF_Excel_aca_Instruments_Traded_merge[,"Fund.ID"]),]

row.names(EurekahedgeHF_Excel_aca_Instruments_Traded_merge) <- seq(nrow(EurekahedgeHF_Excel_aca_Instruments_Traded_merge))

rm(EurekahedgeHF_Excel_aca_Instruments_Traded)
rm(Instruments_Traded,Instruments_Traded_comb)
rm(Exposure,Exposure_comb)


###############################################################################
cat("SECTION: CLEAN EurekahedgeHF_Excel_aca", "\n")
###############################################################################

EurekahedgeHF_Excel_aca <- read.csv(file=paste(output_directory,files[1,1],sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(EurekahedgeHF_Excel_aca,class)=="character"))
{
  EurekahedgeHF_Excel_aca[[i]] = trim(EurekahedgeHF_Excel_aca[[i]])
}
for (i in 1:ncol(EurekahedgeHF_Excel_aca))
{
  EurekahedgeHF_Excel_aca[,i] <- unknownToNA(EurekahedgeHF_Excel_aca[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                    NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                    NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  EurekahedgeHF_Excel_aca[,i] <- ifelse(is.na(EurekahedgeHF_Excel_aca[,i]),NA, EurekahedgeHF_Excel_aca[,i])
} 

EurekahedgeHF_Excel_aca_cols <- colnames(EurekahedgeHF_Excel_aca)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="\\.{2,}", replacement="\\.", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="\\.{2,}", replacement="\\.", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="\\.{2,}", replacement="\\.", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="\\.{2,}", replacement="\\.", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="[[:punct:]]?$", replacement="", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="X2011.Returns", replacement="Returns.2011", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="X2012.Returns", replacement="Returns.2012", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="VaR.90", replacement="VaR.90pct", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="VaR.95", replacement="VaR.95pct", x=EurekahedgeHF_Excel_aca_cols)
EurekahedgeHF_Excel_aca_cols <- gsub(pattern="VaR.99", replacement="VaR.99pct", x=EurekahedgeHF_Excel_aca_cols)
colnames(EurekahedgeHF_Excel_aca) <- EurekahedgeHF_Excel_aca_cols

EurekahedgeHF_Excel_aca[,"Date.Added"] <- gsub(pattern="/", replacement="-", x=EurekahedgeHF_Excel_aca[,"Date.Added"])
EurekahedgeHF_Excel_aca[,"Date.Added"] <- as.Date(EurekahedgeHF_Excel_aca[,"Date.Added"],format="%m-%d-%Y")

EurekahedgeHF_Excel_aca[,"Dead.Date"] <- gsub(pattern="/", replacement="-", x=EurekahedgeHF_Excel_aca[,"Dead.Date"])
EurekahedgeHF_Excel_aca[,"Dead.Date"] <- as.Date(EurekahedgeHF_Excel_aca[,"Dead.Date"],format="%m-%d-%Y")

EurekahedgeHF_Excel_aca[,"Inception.Date"] <- gsub(pattern="/", replacement="-", x=EurekahedgeHF_Excel_aca[,"Inception.Date"])
EurekahedgeHF_Excel_aca[,"Inception.Date"] <- as.Date(EurekahedgeHF_Excel_aca[,"Inception.Date"],format="%m-%d-%Y")

EurekahedgeHF_Excel_aca <- EurekahedgeHF_Excel_aca[rowSums(is.na(EurekahedgeHF_Excel_aca[,1:ncol(EurekahedgeHF_Excel_aca)]))<ncol(EurekahedgeHF_Excel_aca),]

EurekahedgeHF_Excel_aca <- EurekahedgeHF_Excel_aca[order(EurekahedgeHF_Excel_aca[,"Fund.ID"]),]

row.names(EurekahedgeHF_Excel_aca) <- seq(nrow(EurekahedgeHF_Excel_aca))

write.csv(EurekahedgeHF_Excel_aca, file=paste(output_directory,file="EurekahedgeHF_Excel_aca",".csv",sep=""),row.names=FALSE)

rm2(EurekahedgeHF_Excel_aca_cols)


###############################################################################
cat("SECTION: REMOVE MONTHLY RETURNS FROM EurekahedgeHF_Excel_aca", "\n")
###############################################################################

monthly_ret_cols <- c("Jun.12.Returns","May.12.Returns","Apr.12.Returns")

EurekahedgeHF_Excel_aca_monthly_ret_temp <- EurekahedgeHF_Excel_aca[,c("Fund.ID",monthly_ret_cols)]

EurekahedgeHF_Excel_aca_monthly_ret_temp  <- EurekahedgeHF_Excel_aca_monthly_ret_temp[order(EurekahedgeHF_Excel_aca_monthly_ret_temp[,"Fund.ID"]),]

row.names(EurekahedgeHF_Excel_aca_monthly_ret_temp) <- seq(nrow(EurekahedgeHF_Excel_aca_monthly_ret_temp))

for(i in 1:length(monthly_ret_cols))
{
  #i <- 1
  #i <- 2
  #i <- 3
  
  temp <-  melt(EurekahedgeHF_Excel_aca_monthly_ret_temp[,c("Fund.ID",monthly_ret_cols[i])], id=c("Fund.ID"), na.rm=FALSE)
  
  for(j in 1:ncol(temp))
  {
    #j <- 1
    
    temp[,j] = trim(temp[,j])
    
  }
  
  temp[,"Fund.ID"] <- as.integer(temp[,"Fund.ID"])
  
  colnames(temp)[match("variable",names(temp))] <- "date"
  colnames(temp)[match("value",names(temp))] <- "Monthly_Ret2"
  
  temp[,"date"] <- gsub(pattern="X", replacement="", x=temp[,"date"])
  temp[,"date"] <- gsub(pattern="\\.", replacement="-", x=temp[,"date"])
  temp[,"date"] <- gsub(pattern=" ", replacement="", x=temp[,"date"])
  temp[,"date"] <- gsub(pattern=" ", replacement="", x=temp[,"date"])
  temp[,"date"] <- gsub(pattern="-Returns", replacement="", x=temp[,"date"])
  
  for(j in 1:ncol(temp))
  {
    #j <- 1
    
    temp[,j] = trim(temp[,j])
    
  }
  
  temp_month <- temp[,"date"]
  temp_month <- substr(temp_month, 1, 3)
  temp_month <- match(tolower(temp_month), tolower(month.abb))
  
  temp_yr <- temp[,"date"]
  temp_yr <- substr(temp_yr, 5, 6)
  temp_yr <- as.integer(temp_yr)
  
  temp_dt <- format(as.Date(paste(temp_yr,temp_month,"01",sep="-"),format="%y-%m-%d"),"%Y-%m-%d")
  temp_dt <- as.Date(temp_dt,format="%Y-%m-%d")
  
  temp[,"date"] <- temp_dt
  
  temp[,"Monthly_Ret2"] <- as.numeric(temp[,"Monthly_Ret2"])
  
  cat("LOOP: ",i, "\n")
  
  if(i==1)
  {
    EurekahedgeHF_Excel_aca_monthly_ret <- temp
    
  } else
  {
    EurekahedgeHF_Excel_aca_monthly_ret <- rbind(EurekahedgeHF_Excel_aca_monthly_ret,temp)
  }
  
}

EurekahedgeHF_Excel_aca_monthly_ret <- data.frame(EurekahedgeHF_Excel_aca_monthly_ret,
                                                  yr=year(EurekahedgeHF_Excel_aca_monthly_ret[,"date"]),
                                                  month=month(EurekahedgeHF_Excel_aca_monthly_ret[,"date"]),
                                                  stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_monthly_ret <- EurekahedgeHF_Excel_aca_monthly_ret[rowSums(is.na(EurekahedgeHF_Excel_aca_monthly_ret[,1:ncol(EurekahedgeHF_Excel_aca_monthly_ret)]))<ncol(EurekahedgeHF_Excel_aca_monthly_ret),]

EurekahedgeHF_Excel_aca_monthly_ret <- EurekahedgeHF_Excel_aca_monthly_ret[order(EurekahedgeHF_Excel_aca_monthly_ret[,"Fund.ID"],
                                                                                 EurekahedgeHF_Excel_aca_monthly_ret[,"date"],
                                                                                 EurekahedgeHF_Excel_aca_monthly_ret[,"yr"],
                                                                                 EurekahedgeHF_Excel_aca_monthly_ret[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_monthly_ret) <- seq(nrow(EurekahedgeHF_Excel_aca_monthly_ret))

rm(EurekahedgeHF_Excel_aca_monthly_ret_temp)
rm(temp,temp_yr,temp_month,temp_dt)


###############################################################################
cat("SECTION: REMOVE YEARLY RETURNS FROM EurekahedgeHF_Excel_aca", "\n")
###############################################################################

yearly_ret_cols <- c("Returns.2011","Returns.2012")

EurekahedgeHF_Excel_aca_yearly_ret_temp <- EurekahedgeHF_Excel_aca[,c("Fund.ID",yearly_ret_cols)]

EurekahedgeHF_Excel_aca_yearly_ret_temp  <- EurekahedgeHF_Excel_aca_yearly_ret_temp[order(EurekahedgeHF_Excel_aca_yearly_ret_temp[,"Fund.ID"]),]

row.names(EurekahedgeHF_Excel_aca_yearly_ret_temp) <- seq(nrow(EurekahedgeHF_Excel_aca_yearly_ret_temp))

for(i in 1:length(yearly_ret_cols))
{
  #i <- 1
  #i <- 2
  #i <- 3
  
  temp <-  melt(EurekahedgeHF_Excel_aca_yearly_ret_temp[,c("Fund.ID",yearly_ret_cols[i])], id=c("Fund.ID"), na.rm=FALSE)
  
  for(j in 1:ncol(temp))
  {
    #j <- 1
    
    temp[,j] = trim(temp[,j])
    
  }
  
  temp[,"Fund.ID"] <- as.integer(temp[,"Fund.ID"])
  
  colnames(temp)[match("variable",names(temp))] <- "yr"
  colnames(temp)[match("value",names(temp))] <- "Yearly_Ret"
  
  temp[,"yr"] <- gsub(pattern="X", replacement="", x=temp[,"yr"])
  temp[,"yr"] <- gsub(pattern="\\.", replacement="-", x=temp[,"yr"])
  temp[,"yr"] <- gsub(pattern=" ", replacement="", x=temp[,"yr"])
  temp[,"yr"] <- gsub(pattern=" ", replacement="", x=temp[,"yr"])
  temp[,"yr"] <- gsub(pattern="Returns-", replacement="", x=temp[,"yr"])
  
  for(j in 1:ncol(temp))
  {
    #j <- 1
    
    temp[,j] = trim(temp[,j])
    
  }
  
  temp[,"yr"] <- as.integer(temp[,"yr"])
  temp[,"Yearly_Ret"] <- as.numeric(temp[,"Yearly_Ret"])
  
  cat("LOOP: ",i, "\n")
  
  if(i==1)
  {
    EurekahedgeHF_Excel_aca_yearly_ret <- temp
    
  } else
  {
    EurekahedgeHF_Excel_aca_yearly_ret <- rbind(EurekahedgeHF_Excel_aca_yearly_ret,temp)
  }
  
}

EurekahedgeHF_Excel_aca_yearly_ret <- EurekahedgeHF_Excel_aca_yearly_ret[rowSums(is.na(EurekahedgeHF_Excel_aca_yearly_ret[,1:ncol(EurekahedgeHF_Excel_aca_yearly_ret)]))<ncol(EurekahedgeHF_Excel_aca_yearly_ret),]

EurekahedgeHF_Excel_aca_yearly_ret <- EurekahedgeHF_Excel_aca_yearly_ret[order(EurekahedgeHF_Excel_aca_yearly_ret[,"Fund.ID"],
                                                                               EurekahedgeHF_Excel_aca_yearly_ret[,"yr"]),]

row.names(EurekahedgeHF_Excel_aca_yearly_ret) <- seq(nrow(EurekahedgeHF_Excel_aca_yearly_ret))

rm(EurekahedgeHF_Excel_aca_yearly_ret_temp)
rm(temp)


###############################################################################
cat("SECTION: MERGE DATA", "\n")
###############################################################################
#EurekahedgeHF_Excel_aca_NAV_AUM_melt[,!names(EurekahedgeHF_Excel_aca_NAV_AUM_melt) %in% c("date")]
EurekahedgeHF_Excel_aca_full0 <- merge(EurekahedgeHF_Excel_aca_NAV_AUM_melt,
                                       EurekahedgeHF_Excel_aca_monthly_ret[,!names(EurekahedgeHF_Excel_aca_monthly_ret) %in% c("date")], 
                                       by.x=c("Fund.ID","yr","month"), by.y=c("Fund.ID","yr","month"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE,suffixes=c(".x",".y"),incomparables=NA)

EurekahedgeHF_Excel_aca_full1 <- merge(EurekahedgeHF_Excel_aca_full0, 
                                       EurekahedgeHF_Excel_aca_yearly_ret, 
                                       by.x=c("Fund.ID","yr"), by.y=c("Fund.ID","yr"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE,suffixes=c(".x",".y"),incomparables=NA)

EurekahedgeHF_Excel_aca_full2 <- merge(EurekahedgeHF_Excel_aca[,!names(EurekahedgeHF_Excel_aca) %in% c(monthly_ret_cols,yearly_ret_cols)],
                                       EurekahedgeHF_Excel_aca_full1, 
                                       by.x=c("Fund.ID"), by.y=c("Fund.ID"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE,suffixes=c(".x",".y"),incomparables=NA)

EurekahedgeHF_Excel_aca_full3 <- merge(EurekahedgeHF_Excel_aca_full2, EurekahedgeHF_Excel_aca_Instruments_Traded_merge, 
                                       by.x=c("Fund.ID"), by.y=c("Fund.ID"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE,suffixes=c(".x",".y"),incomparables=NA)

EurekahedgeHF_Excel_aca_merge <- EurekahedgeHF_Excel_aca_full3[!is.na(EurekahedgeHF_Excel_aca_full3[,"Strategy"]),]
EurekahedgeHF_Excel_aca_merge <- EurekahedgeHF_Excel_aca_merge[EurekahedgeHF_Excel_aca_merge[,"Base.Currency"]=="USD",]
EurekahedgeHF_Excel_aca_merge <- EurekahedgeHF_Excel_aca_merge[EurekahedgeHF_Excel_aca_merge[,"Minimum.Investment.Currency"]=="USD",]

#EurekahedgeHF_Excel_aca_full0 <- merge(EurekahedgeHF_Excel_aca, EurekahedgeHF_Excel_aca_NAV_AUM_melt, 
#                                      by.x=c("Fund.ID","Fund.Name"), by.y=c("Fund.ID","Fund.Name"), 
#                                      all.x=FALSE, all.y=FALSE, sort=FALSE,suffixes=c(".x",".y"),incomparables=NA)

#EurekahedgeHF_Excel_aca_full <- merge(EurekahedgeHF_Excel_aca_full0, EurekahedgeHF_Excel_aca_Instruments_Traded_merge, 
#                                      by.x=c("Fund.ID"), by.y=c("Fund.ID"), 
#                                      all.x=FALSE, all.y=FALSE, sort=FALSE,suffixes=c(".x",".y"),incomparables=NA)


#EurekahedgeHF_Excel_aca_full <- data.frame(EurekahedgeHF_Excel_aca_full,
#                                           yr=year(EurekahedgeHF_Excel_aca_full[,"date"]),
#                                           month=month(EurekahedgeHF_Excel_aca_full[,"date"]),
#                                           stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_merge <- EurekahedgeHF_Excel_aca_merge[rowSums(is.na(EurekahedgeHF_Excel_aca_merge[,1:ncol(EurekahedgeHF_Excel_aca_merge)]))<ncol(EurekahedgeHF_Excel_aca_merge),]

EurekahedgeHF_Excel_aca_merge <- EurekahedgeHF_Excel_aca_merge[order(EurekahedgeHF_Excel_aca_merge[,"Fund.ID"],
                                                                     EurekahedgeHF_Excel_aca_merge[,"Fund.Name"],
                                                                     EurekahedgeHF_Excel_aca_merge[,"date"],
                                                                     EurekahedgeHF_Excel_aca_merge[,"yr"],                                                                  
                                                                     EurekahedgeHF_Excel_aca_merge[,"month"]),]

row.names(EurekahedgeHF_Excel_aca_merge) <- seq(nrow(EurekahedgeHF_Excel_aca_merge))

rm(EurekahedgeHF_Excel_aca_full0,EurekahedgeHF_Excel_aca_full1,EurekahedgeHF_Excel_aca_full2,EurekahedgeHF_Excel_aca_full3)
#rm(EurekahedgeHF_Excel_aca_NAV_AUM_melt,EurekahedgeHF_Excel_aca,EurekahedgeHF_Excel_aca_Instruments_Traded_merge)
#rm(EurekahedgeHF_Excel_aca_monthly_ret,EurekahedgeHF_Excel_aca_yearly_ret)

###############################################################################
cat("REORDER COLUMNS", "\n")
###############################################################################

#EurekahedgeHF_Excel_aca_merge <- EurekahedgeHF_Excel_aca_merge[!is.na(EurekahedgeHF_Excel_aca_merge[,"Strategy"]),]
#EurekahedgeHF_Excel_aca_merge <- EurekahedgeHF_Excel_aca_merge[EurekahedgeHF_Excel_aca_merge[,"Base.Currency"]=="USD",]

starting_cols <- c("Fund.ID","Fund.Name","Date.Added","Flagship","Closed","Limited","Dead","Dead.Date","Dead.Reason",
                   "Eurekahedge.ID","ISIN","SEDOL","Valoren","CUSIP","Bloomberg","Reuters",
                   "date","yr","month","AUM","Yearly_Ret","Monthly_Ret","Monthly_Ret2")

all_cols <- colnames(EurekahedgeHF_Excel_aca_merge)

other_cols <- all_cols[-which(all_cols %in% starting_cols)]

EurekahedgeHF_Excel_aca_merge <- EurekahedgeHF_Excel_aca_merge[,c(starting_cols,other_cols)]

write.csv(EurekahedgeHF_Excel_aca_merge, file=paste(output_directory,file="EurekahedgeHF_Excel_aca_merge",".csv",sep=""),row.names=FALSE)

EurekahedgeHF_Excel_aca_merge_trim <- data.frame(EurekahedgeHF_Excel_aca_merge[,c("Fund.ID","Fund.Name","date","yr","month","Strategy")],
                                                 stringsAsFactors=FALSE)

EurekahedgeHF_Excel_aca_merge_trim <- unique(EurekahedgeHF_Excel_aca_merge_trim[,c("Fund.ID","Fund.Name","yr","Strategy")])

write.csv(EurekahedgeHF_Excel_aca_merge_trim, file=paste(output_directory,file="EurekahedgeHF_Excel_aca_merge_trim",".csv",sep=""),row.names=FALSE)


###############################################################################
cat("SECTION: FORMAT TEXT", "\n")
###############################################################################

sample_data_all <- read.csv(file=paste(output_directory,"EurekahedgeHF_Excel_aca_merge_trim",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
colnames(sample_data_all)[match("Fund.ID",names(sample_data_all))] <- "Fund_ID"
colnames(sample_data_all)[match("Fund.Name",names(sample_data_all))] <- "Fund_Name"
sample_data_all  <- sample_data_all[order(sample_data_all[,"Fund_ID"],sample_data_all[,"Fund_Name"],sample_data_all[,"yr"]),]

for(i in which(sapply(sample_data_all,class)=="character"))
{
  sample_data_all[[i]] = trim(sample_data_all[[i]])
}
for (i in 1:ncol(sample_data_all))
{
  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                    NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                    NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
} 

#Trim strings
sample_data_all[,"Strategy"] <- trim(sample_data_all[,"Strategy"])

#Remove multiple spaces (run a couple times)
for (a in 1:5)
{
  #a <- 1
  sample_data_all[,"Strategy"] <- gsub(pattern=" {2,}", replacement=" ", x=sample_data_all[,"Strategy"])
  
}

#Remove double hyphens
for (a in 1:5)
{
  #a <- 1
  sample_data_all[,"Strategy"] <- gsub(pattern="--", replacement="-", x=sample_data_all[,"Strategy"])
  
}

#Remove sections that have less than 100 words
sample_data_all_ios_len  <- pbsapply(sample_data_all[,"Strategy"],
                                     function(x) {return(length(strsplit(x,' ')[[1]]))}, 
                                     simplify=FALSE, USE.NAMES=FALSE)
sample_data_all_trim  <- data.frame(sample_data_all,ios_word_count=unlist(sample_data_all_ios_len),stringsAsFactors=FALSE)

sample_data_all_trim2 <- sample_data_all_trim[sample_data_all_trim[,"ios_word_count"]>=100,]

sample_data_all_trim3 <- subset(sample_data_all_trim2,select=-c(ios_word_count))

row.names(sample_data_all_trim3)  <- seq(nrow(sample_data_all_trim3))

write.csv(sample_data_all_trim3,file=paste(output_directory,"sample_data_all.csv",sep=""),na="",quote=TRUE,row.names=FALSE)

rm2(sample_data_all,sample_data_all_ios_len,sample_data_all_trim,sample_data_all_trim2,sample_data_all_trim3)


###############################################################################
cat("SECTION: COMPUTE READABILITY STATISTICS", "\n")
###############################################################################

sample_data_all <- read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(sample_data_all,class)=="character"))
{
  sample_data_all[[i]] = trim(sample_data_all[[i]])
}
for (i in 1:ncol(sample_data_all))
{
  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                    NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                    NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
} 


###TEMP - ASSUME TEXT IS SAME EVERY YEAR###
sample_data_all <- sample_data_all[,c("Fund_ID","Strategy")]
sample_data_all <- unique(sample_data_all)

sample_data_all <- data.frame(ID=seq(1,nrow(sample_data_all)),sample_data_all,stringsAsFactors=FALSE)
sample_data_all[,"ID"] <- paste("", formatC(sample_data_all[,"ID"] , width=6, format="d", flag="0"), sep="")


Dale.Chall_word_list <- read.csv(file=paste(input_directory,"DaleChall_word_list.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(Dale.Chall_word_list,class)=="character"))
{
  Dale.Chall_word_list[[i]] = trim(Dale.Chall_word_list[[i]])
}
for (i in 1:ncol(Dale.Chall_word_list))
{
  Dale.Chall_word_list[,i] <- unknownToNA(Dale.Chall_word_list[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  Dale.Chall_word_list[,i] <- ifelse(is.na(Dale.Chall_word_list[,i]),NA, Dale.Chall_word_list[,i])
} 

tagged_text_desc_stats <- c("lines","chars.no.space","letters.only","digits","normalized.space")
hyph_text_en_desc_stats <- c("num.syll")
readability_stats <- c("ARI", "Coleman.Liau","Flesch.Kincaid", "FOG","SMOG")
readability_desc_stats <- c("sentences","words","all.chars","punct","conjunctions","prepositions","pronouns",
                            "foreign","FOG.hard.words","TTR","sntc.per.word","avg.sentc.length","avg.word.length",
                            "avg.syll.word","sntc.per100","syll.per100","lett.per100")
readability_all_stats <- c(tagged_text_desc_stats,hyph_text_en_desc_stats,readability_stats,readability_desc_stats)

#token_stats <- c("token","desc","stop","stem")
token_stats <- c("token","desc")

#sample_data_all_backup <- sample_data_all
#sample_data_all <- sample_data_all_backup
#sample_data_all <- sample_data_all[1:50,]


for (l in 1:nrow(readbl_vars))
{
  #l <- 1
  
  readability_all_stats_temp <- paste(readability_all_stats, readbl_vars[l,2], sep="")
  readability_all_stats_temp <- gsub(pattern="\\.", replacement="_", x=readability_all_stats_temp)
  
  sample_results <- pbsapply(sample_data_all[,readbl_vars[l,1]],compute_readability_stats,
                             tagged_text_desc_measures=tagged_text_desc_stats,
                             hyph_text_en_desc_measures=hyph_text_en_desc_stats,
                             readability_measures=readability_stats,
                             readability_desc_measures=readability_desc_stats,
                             token_measures=token_stats,
                             dc_word_list=Dale.Chall_word_list,
                             stop_words=myStopwords_all,
                             #treetag_dir="C:/TreeTagger",
                             #treetag_dir="\\\\tsclient\\C\\TreeTagger",
                             #treetag_dir="\\tsclient\C\TreeTagger",
                             treetag_dir=treetag_directory,
                             debug=FALSE,
                             simplify=FALSE, USE.NAMES=FALSE)
  
  sample_read_stats <- pblapply(sample_results, "[[","readstats")
  sample_read_stats <- pblapply(seq_along(sample_read_stats), function(x) data.frame(ID=x,sample_read_stats[x],stringsAsFactors=FALSE))
  sample_read_stats_df <- as.data.frame(do.call(rbind, sample_read_stats),stringsAsFactors=FALSE)
  
  rm2(sample_read_stats)
  
  sample_read_stats_df[,1] <- paste("", formatC(sample_read_stats_df[,1], width=6, format="d", flag="0"), sep="")
  colnames(sample_read_stats_df) <- c("ID",readability_all_stats_temp)
  
  #sample_data_all_temp <- cbind(subset(sample_data_all,select=c("ID","Fund_ID","yr")),subset(sample_data_all,select=c(readbl_vars[l,1])))
  #sample_data_all_temp <- subset(sample_data_all,select=c("ID","yr","crsp_fundno"))
  
  sample_read_stats_df_merge <- merge(sample_data_all[,c("ID","Fund_ID")], sample_read_stats_df, by.x=c("ID") , by.y=c("ID"), all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)
  
  rm2(sample_read_stats_df)
  
  sample_read_stats_df_merge <- subset(sample_read_stats_df_merge,select=-c(ID))
  
  write.csv(sample_read_stats_df_merge, file=paste(output_directory,readbl_vars[l,3],".csv",sep=""),row.names=FALSE)
  
  rm2(sample_read_stats_df_merge)
  
  sample_tokens <- pblapply(sample_results, "[[","tokens") 
  sample_tokens <- pblapply(seq_along(sample_tokens), function(x) data.frame(ID=x,sample_tokens[x],stringsAsFactors=FALSE))
  sample_tokens_df <- as.data.frame(do.call(rbind, sample_tokens),stringsAsFactors=FALSE)
  
  rm2(sample_tokens)
  
  sample_tokens_df[,1] <- paste("", formatC(sample_tokens_df[,1], width=6, format="d", flag="0"), sep="")
  colnames(sample_tokens_df) <- c("ID",token_stats)
  #colnames(sample_tokens_df) <- c(token_stats)
  
  sample_tokens_df_merge <- merge(sample_data_all[,c("ID","Fund_ID")], sample_tokens_df, by.x=c("ID") , by.y=c("ID"), all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)
  
  rm2(sample_tokens_df)
  
  sample_tokens_df_merge <- subset(sample_tokens_df_merge,select=-c(ID))
  
  write.csv(sample_tokens_df_merge, file=paste(output_directory,readbl_vars[l,4],".csv",sep=""),row.names=FALSE)
  
  rm2(sample_tokens_df_merge,sample_results)
  
}

rm2(sample_data_all)
rm2(tagged_text_desc_stats,hyph_text_en_desc_stats,readability_stats,readability_desc_stats,readability_all_stats,token_stats)


###############################################################################
cat("SECTION: TEMP - EXPAND YEARS", "\n")
###############################################################################

l <- 1

sample_data_all <- read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(sample_data_all,class)=="character"))
{
  sample_data_all[[i]] = trim(sample_data_all[[i]])
}
for (i in 1:ncol(sample_data_all))
{
  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                    NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                    NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
} 

sample_read_stats_df_merge <- read.csv(file=paste(output_directory,readbl_vars[l,3],".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(sample_read_stats_df_merge,class)=="character"))
{
  sample_read_stats_df_merge[[i]] = trim(sample_read_stats_df_merge[[i]])
}
for (i in 1:ncol(sample_read_stats_df_merge))
{
  sample_read_stats_df_merge[,i] <- unknownToNA(sample_read_stats_df_merge[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                          NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                          NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  sample_read_stats_df_merge[,i] <- ifelse(is.na(sample_read_stats_df_merge[,i]),NA, sample_read_stats_df_merge[,i])
} 

sample_tokens_df_merge <- read.csv(file=paste(output_directory,readbl_vars[l,4],".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(sample_tokens_df_merge,class)=="character"))
{
  sample_tokens_df_merge[[i]] = trim(sample_tokens_df_merge[[i]])
}
for (i in 1:ncol(sample_tokens_df_merge))
{
  sample_tokens_df_merge[,i] <- unknownToNA(sample_tokens_df_merge[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                  NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                  NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  sample_tokens_df_merge[,i] <- ifelse(is.na(sample_tokens_df_merge[,i]),NA, sample_tokens_df_merge[,i])
} 



sample_read_stats_df_merge_full <- merge(subset(sample_data_all,select=-c(Fund_Name,Strategy)), sample_read_stats_df_merge, 
                                         by.x=c("Fund_ID") , by.y=c("Fund_ID"), 
                                         all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)

write.csv(sample_read_stats_df_merge_full, file=paste(output_directory,readbl_vars[l,3],"_full",".csv",sep=""),row.names=FALSE)


sample_tokens_df_merge_full <- merge(subset(sample_data_all,select=-c(Fund_Name,Strategy)), sample_tokens_df_merge, 
                                     by.x=c("Fund_ID") , by.y=c("Fund_ID"), 
                                     all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)

write.csv(sample_tokens_df_merge_full, file=paste(output_directory,readbl_vars[l,4],"_full",".csv",sep=""),row.names=FALSE)


###############################################################################
cat("SECTION: COMPUTE SIMILARITY STATISTICS", "\n")
###############################################################################

sample_data_all <- read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

for(i in which(sapply(sample_data_all,class)=="character"))
{
  sample_data_all[[i]] = trim(sample_data_all[[i]])
}
for (i in 1:ncol(sample_data_all))
{
  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                    NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                    NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
} 


for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  #m <- 2
  
  cat("Token table: ",readbl_vars[m,4], "\n")
  if (m==1)
  {
    
    #input_row_count <- nrow(read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE))
    #tokens_all_temp <- as.data.frame(matrix(NA, ncol=tokens_all_cols_count, nrow=input_row_count))
    #colnames(tokens_all_temp) <- tokens_all_cols[,6]
    
    #tokens_all_temp[,"ID"] <- read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[c("ID")]
    #tokens_all_temp[,"token"] <- read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[c("token")]
    #tokens_all_temp[,"desc"] <- read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[c("desc")]
    
    tokens_all_temp <- read.csv(file=paste(output_directory,readbl_vars[m,4],"_full",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
    
  } else if (m==2)
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
    
  } else
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
    
  }
  
  for(i in which(sapply(tokens_all_temp,class)=="character"))
  {
    tokens_all_temp[[i]] = trim(tokens_all_temp[[i]])
  }
  for (i in 1:ncol(tokens_all_temp))
  {
    tokens_all_temp[,i] <- unknownToNA(tokens_all_temp[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                      NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
    tokens_all_temp[,i] <- ifelse(is.na(tokens_all_temp[,i]),NA, tokens_all_temp[,i])
  } 
  
  tokens_all_temp <- tokens_all_temp[!(rowSums(is.na(tokens_all_temp[,1:ncol(tokens_all_temp)]))==ncol(tokens_all_temp)),]
  
  
  tokens_all_temp <- tokens_all_temp[order(tokens_all_temp[,"Fund_ID"],tokens_all_temp[,"yr"]),] 
  
  #Trim strings
  tokens_all_temp[,"token"] <- trim(tokens_all_temp[,"token"])
  
  #Upcase strings
  tokens_all_temp[,"token"] <- toupper(tokens_all_temp[,"token"])
  
  #Remove everything except letters, space, apostrophe, hyphen, and ampersand
  tokens_all_temp[,"token"] <- as.data.frame(gsub(pattern="[^[[:alnum:][:space:]'&-]", replacement=" ", x=tokens_all_temp[,"token"]))
  
  #Remove multiple spaces (run a couple times)
  for (a in 1:5)
  {
    #a <- 1
    tokens_all_temp[,"token"] <- gsub(pattern=" {2,}", replacement=" ", x=tokens_all_temp[,"token"])
  }
  
  #Remove numbers
  tokens_all_temp[,"token"]  <- gsub(pattern="\\d", replacement="", x=tokens_all_temp[,"token"])
  
  saved_symbols <- c("&","-","'")
  
  #Remove double apostrophe, hyphen, and ampersand
  for (a in 1:5)
  {
    #a <- 1
    tokens_all_temp[,"token"]  <- remove_duplicate_symbols(tokens_all_temp[,"token"], saved_symbols)
    
  }
  
  #Remove single, leading, and trailing symbols
  tokens_all_temp[,"token"]  <-  remove_single_symbols(tokens_all_temp[,"token"], saved_symbols)
  tokens_all_temp[,"token"]  <-  remove_leading_symbols(tokens_all_temp[,"token"], saved_symbols)
  tokens_all_temp[,"token"]  <-  remove_trailing_symbols(tokens_all_temp[,"token"], saved_symbols)
  
  #Remove single spaces (run a couple times)
  for (a in 1:5)
  {
    #a <- 1
    tokens_all_temp[,"token"]  <- gsub(pattern=" ", replacement="", x=tokens_all_temp[,"token"])
  }
  
  
  #Default Remove to NA
  tokens_all_temp[,"Remove"] <- as.numeric(rep(NA, nrow(tokens_all_temp)))
  
  #Find which rows to remove
  remove_descriptions <- c("Cardinal number","Comma","Sentence ending punctuation","Symbol",
                           "Opening bracket","Closing bracket","Quote","End quote")
  remove_punct <- c("%","&","\t","-","--","---","'",""," ")
  remove_phone <- c("1-800-XXX-XXXX","XXX-XXX-XXXX","XXX-XXXX","-XXX-XXX-XXXX")
  remove_tokens <- c(remove_punct,remove_phone)
  
  #keep_three_letter_ratings <- c("AAA","BAA","BBB","CAA","CCC","DDD")
  
  tokens_all_temp[,"Remove"] <- ifelse(!(tokens_all_temp$desc %in% remove_descriptions) & !(tokens_all_temp$token %in% remove_tokens), 0, 1)
  tokens_all_temp[,"Remove"] <- ifelse(((nchar(tokens_all_temp$token)==1) & !(tokens_all_temp$token %in% keep_one_letter_tokens)), 1, tokens_all_temp$Remove)
  tokens_all_temp[,"Remove"] <- ifelse(((nchar(tokens_all_temp$token)==2) & !(tokens_all_temp$token %in% keep_two_letter_tokens)), 1, tokens_all_temp$Remove)
  
  #Remove stop words
  #tokens_all_temp[,"Remove"] <- ifelse(tokens_all_temp$token %in% myStopwords_all, 1, tokens_all_temp$Remove)
  
  #==============================================================================;
  #Stem Words;
  #==============================================================================;
  
  tokens_all_temp <- tokens_all_temp[order(tokens_all_temp[,"Fund_ID"],tokens_all_temp[,"yr"]),] 
  
  tokens_all_temp_dt <- data.table(tokens_all_temp[(tokens_all_temp[,"Remove"]==0),], key = c("Fund_ID","yr"))
  tokens_all_temp1 <- tokens_all_temp_dt[,list(word=stem_words(token,myStopwords_all)),by="Fund_ID,yr"]
  tokens_all_temp1 <- as.data.frame(tokens_all_temp1,stringsAsFactors=FALSE)
  tokens_all_temp  <- data.frame(Fund_ID=tokens_all_temp1[,"Fund_ID"],
                                 yr=tokens_all_temp1[,"yr"],
                                 token=tokens_all_temp1[,"word"],
                                 Remove=0,stringsAsFactors=FALSE)
  
  rm2(tokens_all_temp_dt,tokens_all_temp1)
  
  #==============================================================================;
  #Find Unique Words for Each Fund_ID;
  #==============================================================================;
  
  colnames(tokens_all_temp)[match("Fund.ID",names(tokens_all_temp))] <- "Fund_ID"
  
  #Sort tokens_all_temp
  tokens_all_temp  <- tokens_all_temp[order(tokens_all_temp[,"Fund_ID"],tokens_all_temp[,"yr"],tokens_all_temp[,"token"]),]
  
  query_tokens_all_temp2 <- ""
  query_tokens_all_temp2 <- paste(query_tokens_all_temp2,"select distinct  Fund_ID, yr, Upper(token) token, Count(token) Count, Remove  ", sep=" ")
  query_tokens_all_temp2 <- paste(query_tokens_all_temp2,"from             tokens_all_temp                                              ", sep=" ")
  query_tokens_all_temp2 <- paste(query_tokens_all_temp2,"where            Remove=0                                                     ", sep=" ")
  query_tokens_all_temp2 <- paste(query_tokens_all_temp2,"group by         Fund_ID, yr, Upper(token)                                    ", sep=" ")
  query_tokens_all_temp2 <- trim(gsub(" {2,}", " ", query_tokens_all_temp2))
  
  #tokens_all_temp2 <- cbind(sqldf(query_tokens_all_temp2), NA, NA, NA)
  #colnames(tokens_all_temp2) <- c("ID","yr","token","Count","Remove","uTotal","gTotal","Total_Percentage")
  
  
  tokens_all_temp2 <- data.frame(sqldf(query_tokens_all_temp2),uTotal=NA,gTotal=NA,Total_Percentage=NA,stringsAsFactors=FALSE)
  
  tokens_all_temp2[,c("uTotal","gTotal","Total_Percentage")] <- subset(data.table(tokens_all_temp2)[, list(uTotal=length(token), gTotal=sum(Count), Total_Percentage=(Count)/sum(Count)),by="yr,Fund_ID"],
                                                                       select=c("uTotal","gTotal","Total_Percentage"))
  
  #==============================================================================;
  cat("SECTION: GLOBAL DICTIONARY (AGGREGATE)", "\n")
  #
  # global_agg_word_grand_temp: total words across ids 
  # global_agg_word_unique_temp: total unique words across ids 
  #
  #==============================================================================;
  
  identifier <- "Fund_ID"
  
  #==============================================================================;
  #Create global aggregate grand word table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_agg_word_grand_temp
  global_agg_word_grand_temp <- tokens_all_temp[tokens_all_temp[,"Remove"]==0,c("yr",identifier,"token")]
  global_agg_word_grand_temp <- global_agg_word_grand_temp[!(rowSums(is.na(global_agg_word_grand_temp[,1:ncol(global_agg_word_grand_temp)]))==ncol(global_agg_word_grand_temp)),]
  global_agg_word_grand_temp[,"yr"] <- 9999
  global_agg_word_grand_temp <- global_agg_word_grand_temp[order(global_agg_word_grand_temp[,"yr"], global_agg_word_grand_temp[,identifier],global_agg_word_grand_temp[,"token"]),] 
  
  global_agg_word_grand_temp3 <- create_global_dictionary_word(global_agg_word_grand_temp,"word_grand",percentiles)
  
  rm2(global_agg_word_grand_temp)
  
  
  #==============================================================================;
  #Create global aggregate unique word table;
  #==============================================================================;
  
  #Copy tokens_all_temp2 to global_agg_word_unique_temp
  global_agg_word_unique_temp <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_agg_word_unique_temp <- global_agg_word_unique_temp[!(rowSums(is.na(global_agg_word_unique_temp[,1:ncol(global_agg_word_unique_temp)]))==ncol(global_agg_word_unique_temp)),]
  global_agg_word_unique_temp[,"yr"] <- 9999
  global_agg_word_unique_temp <- unique(global_agg_word_unique_temp[,c("yr",identifier,"token")], incomparables=FALSE)
  global_agg_word_unique_temp <- global_agg_word_unique_temp[order(global_agg_word_unique_temp[,"yr"], global_agg_word_unique_temp[,identifier],global_agg_word_unique_temp[,"token"]),] 
  
  global_agg_word_unique_temp3 <- create_global_dictionary_word(global_agg_word_unique_temp,"word_unique",percentiles)
  
  rm2(global_agg_word_unique_temp)
  
  #==============================================================================;
  #Create global aggregate unique id table;
  #==============================================================================;
  
  #Copy tokens_all_temp2 to global_agg_id_unique_temp
  global_agg_id_unique_temp <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_agg_id_unique_temp <- global_agg_id_unique_temp[!(rowSums(is.na(global_agg_id_unique_temp[,1:ncol(global_agg_id_unique_temp)]))==ncol(global_agg_id_unique_temp)),]
  global_agg_id_unique_temp[,"yr"] <- 9999
  global_agg_id_unique_temp <- unique(global_agg_id_unique_temp[,c("yr",identifier,"token")], incomparables=FALSE)
  global_agg_id_unique_temp <- global_agg_id_unique_temp[order(global_agg_id_unique_temp[,"yr"], global_agg_id_unique_temp[,identifier],global_agg_id_unique_temp[,"token"]),] 
  
  #Copy tokens_all_temp to unique_ids_agg
  unique_ids_agg <- tokens_all_temp[tokens_all_temp[,"Remove"]==0,c("yr",identifier,"token")]
  unique_ids_agg <- unique_ids_agg[!(rowSums(is.na(unique_ids_agg[,1:ncol(unique_ids_agg)]))==ncol(unique_ids_agg)),]
  unique_ids_agg[,"yr"] <- 9999
  unique_ids_agg <- unique_ids_agg[order(unique_ids_agg[,"yr"], unique_ids_agg[,identifier]),] 
  
  #Get list of all unique words
  unique_ids_agg <- ddply(unique_ids_agg, "yr", function(x) as.data.frame(unique(x[,c("yr",identifier)], incomparables=FALSE)))
  unique_ids_agg <- ddply(unique_ids_agg, "yr", function(x) data.frame(x,nrow=as.numeric(nrow(x))))
  unique_ids_agg <- unique_ids_agg[order(unique_ids_agg[,"yr"], unique_ids_agg[,identifier]),] 
  write.csv(unique_ids_agg, file=paste(output_directory,"unique_ids_agg",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  global_agg_id_unique_temp3 <- create_global_dictionary_id(global_agg_id_unique_temp,"id_unique",percentiles,unique_ids_agg)
  
  #write.csv(global_agg_id_unique_temp3, file=paste(output_directory,"global_agg_id_unique_temp3",".csv",sep=""))
  
  rm2(global_agg_id_unique_temp)
  
  
  #==============================================================================;
  #Merge the ID DVs with the Word DVs (aggregate);
  #==============================================================================;
  
  global_agg_comb1 <- merge(global_agg_word_grand_temp3, global_agg_word_unique_temp3, by.x=c("yr","token") , by.y=c("yr","token"), all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)
  global_agg_comb2 <- merge(global_agg_comb1, global_agg_id_unique_temp3, by.x=c("yr","token") , by.y=c("yr","token") , all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)
  
  global_agg_count_vector <- grep(pattern='*Count_*', colnames(global_agg_comb2), ignore.case=FALSE, perl=FALSE, value=TRUE)
  global_agg_dv_vector <- grep(pattern='*_DV_*', colnames(global_agg_comb2), ignore.case=FALSE, perl=FALSE, value=TRUE)
  global_agg_col_vector <- append(global_agg_count_vector,global_agg_dv_vector)
  global_agg_col_vector <- append("token",global_agg_col_vector)
  global_agg_col_vector <- append("yr",global_agg_col_vector)
  global_agg_comb  <- global_agg_comb2[,global_agg_col_vector]
  write.csv(global_agg_comb, file=paste(output_directory,"global_agg_comb",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  rm2(global_agg_comb1,global_agg_comb2,global_agg_word_grand_temp3,global_agg_word_unique_temp3,global_agg_id_unique_temp3)
  
  
  #==============================================================================;
  #Create global aggregate tables based on the percentiles;
  #==============================================================================;
  
  global_agg_dv_vector_used <- as.data.frame(cbind(global_agg_dv_vector,rep(NA,length(global_agg_dv_vector))),stringsAsFactors=FALSE)
  colnames(global_agg_dv_vector_used)[2] <- "Column_not_all_0"
  
  #Remove trimmed words and save individual dictionaries
  for (b in 1:length(measures))
  {
    #b <- 1
    
    global_agg_dv_vector_used <- trim_global_dictionary(global_agg_comb,measures[b],percentiles,global_agg_dv_vector_used,readbl_vars[m,2])
    
    progress_function(outer_loop_count=b, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
    
  }
  
  assign(paste("global_agg_dv_vector_used",readbl_vars[m,2],sep=""), global_agg_dv_vector_used, envir=.GlobalEnv)
  
  rm2(global_agg_dv_vector_used)
  
  
  #==============================================================================;
  cat("SECTION: GLOBAL DICTIONARY (YEARLY)", "\n")
  #==============================================================================;
  
  #==============================================================================;
  #Create global yearly grand word table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_year_word_grand_temp
  global_year_word_grand_temp <- tokens_all_temp[tokens_all_temp[,"Remove"]==0,c("yr",identifier,"token")]
  global_year_word_grand_temp <- global_year_word_grand_temp[!(rowSums(is.na(global_year_word_grand_temp[,1:ncol(global_year_word_grand_temp)]))==ncol(global_year_word_grand_temp)),]
  global_year_word_grand_temp <- global_year_word_grand_temp[order(global_year_word_grand_temp[,"yr"], global_year_word_grand_temp[,identifier],global_year_word_grand_temp[,"token"]),] 
  
  global_year_word_grand_temp3 <- create_global_dictionary_word(global_year_word_grand_temp,"word_grand",percentiles)
  
  rm2(global_year_word_grand_temp)
  
  
  #==============================================================================;
  #Create global year unique word table;
  #==============================================================================;
  
  #Copy tokens_all_temp2 to global_year_word_unique_temp
  global_year_word_unique_temp <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_year_word_unique_temp <- global_year_word_unique_temp[!(rowSums(is.na(global_year_word_unique_temp[,1:ncol(global_year_word_unique_temp)]))==ncol(global_year_word_unique_temp)),]
  global_year_word_unique_temp <- global_year_word_unique_temp[order(global_year_word_unique_temp[,"yr"], global_year_word_unique_temp[,identifier],global_year_word_unique_temp[,"token"]),] 
  global_year_word_unique_temp <- unique(global_year_word_unique_temp[,c("yr",identifier,"token")], incomparables=FALSE)
  global_year_word_unique_temp3 <- create_global_dictionary_word(global_year_word_unique_temp,"word_unique",percentiles)
  
  rm2(global_year_word_unique_temp)
  
  
  #==============================================================================;
  #Create global year unique id table;
  #==============================================================================;
  
  #Copy tokens_all_temp2 to global_year_id_unique_temp
  global_year_id_unique_temp <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_year_id_unique_temp <- global_year_id_unique_temp[!(rowSums(is.na(global_year_id_unique_temp[,1:ncol(global_year_id_unique_temp)]))==ncol(global_year_id_unique_temp)),]
  global_year_id_unique_temp <- global_year_id_unique_temp[order(global_year_id_unique_temp[,"yr"], global_year_id_unique_temp[,identifier],global_year_id_unique_temp[,"token"]),] 
  global_year_id_unique_temp <- unique(global_year_id_unique_temp[,c("yr",identifier,"token")], incomparables=FALSE)
  
  #Copy tokens_all_temp to unique_ids_year
  unique_ids_year <- tokens_all_temp[tokens_all_temp[,"Remove"]==0,c("yr",identifier,"token")]
  unique_ids_year <- unique_ids_year[!(rowSums(is.na(unique_ids_year[,1:ncol(unique_ids_year)]))==ncol(unique_ids_year)),]
  unique_ids_year <- unique_ids_year[order(unique_ids_year[,"yr"], unique_ids_year[,identifier]),] 
  
  #Get list of all unique words
  unique_ids_year <- ddply(unique_ids_year, "yr", function(x) as.data.frame(unique(x[,c("yr",identifier)], incomparables=FALSE)))
  unique_ids_year <- ddply(unique_ids_year, "yr", function(x) data.frame(x,nrow=as.numeric(nrow(x))))
  unique_ids_year <- unique_ids_year[order(unique_ids_year[,"yr"], unique_ids_year[,identifier]),] 
  write.csv(unique_ids_year, file=paste(output_directory,"unique_ids_year",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  global_year_id_unique_temp3 <- create_global_dictionary_id(global_year_id_unique_temp,"id_unique",percentiles,unique_ids_year)
  
  #write.csv(global_year_id_unique_temp3, file=paste(output_directory,"global_year_id_unique_temp3",".csv",sep=""))
  
  rm2(global_year_id_unique_temp)
  
  
  #==============================================================================;
  #Merge the ID DVs with the Word DVs (year);
  #==============================================================================;
  
  global_year_comb1 <- merge(global_year_word_grand_temp3, global_year_word_unique_temp3, by.x=c("yr","token") , by.y=c("yr","token"), all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)
  global_year_comb2 <- merge(global_year_comb1, global_year_id_unique_temp3, by.x=c("yr","token") , by.y=c("yr","token") , all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables=NA)
  
  global_year_count_vector <- grep(pattern='*Count_*', colnames(global_year_comb2), ignore.case=FALSE, perl=FALSE, value=TRUE)
  global_year_dv_vector <- grep(pattern='*_DV_*', colnames(global_year_comb2), ignore.case=FALSE, perl=FALSE, value=TRUE)
  global_year_col_vector <- append(global_year_count_vector,global_year_dv_vector)
  global_year_col_vector <- append("token",global_year_col_vector)
  global_year_col_vector <- append("yr",global_year_col_vector)
  global_year_comb  <- global_year_comb2[,global_year_col_vector]
  write.csv(global_year_comb, file=paste(output_directory,"global_year_comb",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  
  #global_year_comb_diff <- global_year_comb[global_year_comb[,11]!=global_year_comb[,14],]
  
  rm2(global_year_comb1,global_year_comb2,global_year_word_grand_temp3,global_year_word_unique_temp3,global_year_id_unique_temp3)
  
  
  #==============================================================================;
  #Create global year tables based on the percentiles;
  #==============================================================================;
  
  global_year_dv_vector_used <- as.data.frame(cbind(global_year_dv_vector,rep(NA,length(global_year_dv_vector))),stringsAsFactors=FALSE)
  colnames(global_year_dv_vector_used)[2] <- "Column_not_all_0"
  
  #Remove trimmed words and save individual dictionaries
  for (b in 1:length(measures))
  {
    #b <- 1
    
    global_year_dv_vector_used <- trim_global_dictionary(global_year_comb,measures[b],percentiles,global_year_dv_vector_used,readbl_vars[m,2])
    
    progress_function(outer_loop_count=b, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
    
  }
  
  assign(paste("global_year_dv_vector_used",readbl_vars[m,2],sep=""), global_year_dv_vector_used, envir=.GlobalEnv)
  
  rm2(global_year_dv_vector_used)
  rm2(tokens_all_temp)
  
  #OUPUT INDIVIDUAL TOKEN DICTIONARIES
  
  global_agg_tokens <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_agg_tokens <- global_agg_tokens[!(rowSums(is.na(global_agg_tokens[,1:ncol(global_agg_tokens)]))==ncol(global_agg_tokens)),]
  global_agg_tokens[,"yr"] <- 9999
  global_agg_tokens <- unique(global_agg_tokens[,c("yr",identifier,"token")], incomparables=FALSE)
  global_agg_tokens <- global_agg_tokens[order(global_agg_tokens[,"yr"], global_agg_tokens[,identifier],global_agg_tokens[,"token"]),] 
  write.csv(global_agg_tokens, file=paste(output_directory,"global_agg_tokens",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  global_year_tokens <- tokens_all_temp2[tokens_all_temp2[,"Remove"]==0,c("yr",identifier,"token")]
  global_year_tokens <- global_year_tokens[!(rowSums(is.na(global_year_tokens[,1:ncol(global_year_tokens)]))==ncol(global_year_tokens)),]
  global_year_tokens <- unique(global_year_tokens[,c("yr",identifier,"token")], incomparables=FALSE)
  global_year_tokens <- global_year_tokens[order(global_year_tokens[,"yr"], global_year_tokens[,identifier],global_year_tokens[,"token"]),] 
  write.csv(global_year_tokens, file=paste(output_directory,"global_year_tokens",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  rm2(tokens_all_temp2)
  
  
  #==============================================================================;
  #CREATE PROGRESS OUTPUTS;
  #==============================================================================;
  
  rm2(global_agg_comb,unique_ids_agg,global_agg_tokens)
  rm2(global_year_comb,unique_ids_year,global_year_tokens)
  
  progress_function(outer_loop_count=m, outer_loop_start_val=1, outer_loop_end_val=nrow(readbl_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
  #END OF M FOR LOOP
  
}

rm2(sample_data_all)
rm2(remove_descriptions,remove_punct,remove_phone,remove_tokens)
rm2(keep_one_letter_words, keep_one_letter_ratings, keep_one_letter_tokens)
rm2(keep_two_letter_words, keep_state_abbreviations, keep_two_letter_ratings, keep_two_letter_tokens)


# #==============================================================================;
# cat("SECTION: INDIVIDUAL DICTIONARY - AGGREGATE", "\n")
# #==============================================================================;
# 
# identifier <- "Fund_ID"
# 
# output_db <- paste(output_directory,"Text_Analysis.s3db",sep="")
# 
# for (m in 1:nrow(readbl_vars))
# {
#   
#   #m <- 1
#   #m <- 2
#   
#   if (m==1)
#   {
#     file_type_str <- readbl_vars[m,2]
#     
#   } else if (m==2)
#   {
#     #file_type_str <- readbl_vars[m,2]
#     
#   } else
#   {
#     cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
#     
#   }
#   
#   #AGGREGATE
#   combined_data <- as.data.frame(fread(paste(output_directory,"global_agg_comb",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
#   tokens_data <- as.data.frame(fread(paste(output_directory,"global_agg_tokens",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
#   id_data <- as.data.frame(fread(paste(output_directory,"unique_ids_agg",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
#   
#   for (a in 1:length(measures))
#   {
#     
#     for (b in 1:nrow(percentiles))
#     {
#       #a <- 1
#       #b <- 1
#       
#       create_individual_dictionary(combined_data,"agg",file_type_str,tokens_data,id_data,measures[a],percentiles[b,],output_db,output_directory,identifier)
#       
#       progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=b, inner_loop_start_val=1, inner_loop_end_val=nrow(percentiles))
#       
#     }
#   }
#   
#   rm2(combined_data,tokens_data,id_data)
#  
# }
# 
# rm2(output_db)

#SQLite Test

#Text_Analysis_tables <- ListTables("Text_Analysis.s3db")

#Text_Analysis_fields <- ListFields("Text_Analysis.s3db")
#write.csv(Text_Analysis_fields, file=paste(output_directory,"Text_Analysis_fields",".csv",sep=""), row.names=FALSE)

#sql_q <- paste("SELECT * FROM ",ta_db_tables[1],"",sep="")
#aaa <- runsql(sql_q,"Text_Analysis.s3db")
#rm2(aaa)


#agg_id_unique_sim_cosine_normalized_990pct_iois_9999 <- runsql("SELECT * FROM agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
#ExportTable(agg_id_unique_sim_cosine_normalized_990pct_iois_9999,paste(output_directory,"Similarity_Analysis.s3db",sep=""))
#runsql("DROP TABLE agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
#rm2(agg_id_unique_sim_cosine_normalized_990pct_iois_9999)


#Similarity_Analysis_tables <- ListTables("Similarity_Analysis.s3db")


#==============================================================================;
cat("SECTION: INDIVIDUAL DICTIONARY - YEARLY", "\n")
#==============================================================================;

identifier <- "Fund_ID"

output_db <- paste(output_directory,"Text_Analysis.s3db",sep="")

for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  #m <- 2
  
  if (m==1)
  {
    file_type_str <- readbl_vars[m,2]
    
  } else if (m==2)
  {
    #file_type_str <- readbl_vars[m,2]
    
  } else
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
    
  }
  
  #YEARLY
  combined_data <- as.data.frame(fread(paste(output_directory,"global_year_comb",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  tokens_data <- as.data.frame(fread(paste(output_directory,"global_year_tokens",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  id_data <- as.data.frame(fread(paste(output_directory,"unique_ids_year",file_type_str,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
  
  for (a in 1:length(measures))
  {
    
    for (b in 1:nrow(percentiles))
    {
      
      #a <- 3
      #b <- 1
      
      create_individual_dictionary(combined_data,"year",file_type_str,tokens_data,id_data,measures[a],percentiles[b,],output_db,output_directory,identifier)
      
      progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=b, inner_loop_start_val=1, inner_loop_end_val=nrow(percentiles))
      
    }
  }
  
  rm2(combined_data,tokens_data,id_data)
  
}

rm2(output_db)

#SQLite Test

#Text_Analysis_tables <- ListTables("Text_Analysis.s3db")

#Text_Analysis_fields <- ListFields("Text_Analysis.s3db")
#write.csv(Text_Analysis_fields, file=paste(output_directory,"Text_Analysis_fields",".csv",sep=""), row.names=FALSE)

#sql_q <- paste("SELECT * FROM ",ta_db_tables[1],"",sep="")
#aaa <- runsql(sql_q,"Text_Analysis.s3db")
#rm2(aaa)


#agg_id_unique_sim_cosine_normalized_990pct_iois_9999 <- runsql("SELECT * FROM agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
#ExportTable(agg_id_unique_sim_cosine_normalized_990pct_iois_9999,paste(output_directory,"Similarity_Analysis.s3db",sep=""))
#runsql("DROP TABLE agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
#rm2(agg_id_unique_sim_cosine_normalized_990pct_iois_9999)


#Similarity_Analysis_tables <- ListTables("Similarity_Analysis.s3db")


#==============================================================================;
cat("COMPUTE SIMILARITY MEASURES", "\n")
#==============================================================================;

identifier <- "Fund_ID"

sample_data_all <- read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
for(i in which(sapply(sample_data_all,class)=="character"))
{
  sample_data_all[[i]] = trim(sample_data_all[[i]])
}
for (i in 1:ncol(sample_data_all))
{
  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
} 

input_db <- paste(output_directory,"Text_Analysis.s3db",sep="")
output_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")

#input_db_tables <- ListTables(input_db)
#input_db_fields <- ListFields(input_db)

#temp_fields <- input_db_fields[input_db_fields[,1]=="year_word_grand_expand_norm_990pct_iois",]

for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  #m <- 2
  
  if (m==1)
  {
    file_type_str <- readbl_vars[m,2]
    
  } else if (m==2)
  {
    file_type_str <- readbl_vars[m,2]
    
  } else
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
    
  }
  
  #AGGREGATE
  #for (a in 1:length(measures))
  #{
  #  
  #  for (b in 1:nrow(percentiles))
  #  {
  #    calculate_cosine_similarity("agg",file_type_str,measures[a],percentiles[b,],input_db,output_db,output_directory,"cosine_normalized",sample_data_all,identifier)
  #    
  #    progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=b, inner_loop_start_val=1, inner_loop_end_val=nrow(percentiles))
  
  #  }
  #}
  
  capture.output(gc(),file='NUL')
  
  #YEARLY
  for (a in 1:length(measures))
  {
    
    for (b in 1:nrow(percentiles))
    {
      #a <- 1
      #b <- 3
      
      calculate_cosine_similarity("year",file_type_str,measures[a],percentiles[b,],input_db,output_db,output_directory,"cosine_normalized",sample_data_all,identifier)
      
      progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=b, inner_loop_start_val=1, inner_loop_end_val=nrow(percentiles))
      
    }
  }
  
  capture.output(gc(),file='NUL')
  
}

rm2(input_db,output_db)

#Similarity_Analysis_tables <- ListTables("Similarity_Analysis.s3db")

#Similarity_Analysis_fields <- ListFields("Similarity_Analysis.s3db")
#write.csv(Similarity_Analysis_fields, file=paste(output_directory,"Similarity_Analysis_fields",".csv",sep=""), row.names=FALSE)

#sql_q <- paste("SELECT * FROM ",ta_db_tables[1],"",sep="")
#aaa <- runsql(sql_q,"Text_Analysis.s3db")
#rm2(aaa)

#agg_id_unique_sim_cosine_normalized_990pct_iois_9999 <- runsql("SELECT * FROM agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
#ExportTable(agg_id_unique_sim_cosine_normalized_990pct_iois_9999,paste(output_directory,"Similarity_Analysis.s3db",sep=""))
#runsql("DROP TABLE agg_id_unique_sim_cosine_normalized_990pct_iois_9999","Text_Analysis.s3db")
#rm2(agg_id_unique_sim_cosine_normalized_990pct_iois_9999)


#==============================================================================;
#DONE;
cat("DONE", "\n")
#==============================================================================;
