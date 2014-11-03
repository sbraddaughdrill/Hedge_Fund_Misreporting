# TODO: Add comment
# 
# Author: Brad
# File: HF_Misreporting_Tone.R
# Version: 1.0
# Date: 10.30.2014
# Purpose: Compute tone statistics
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
external_packages <- c("compare","cwhmisc","data.table","descr","fastmatch","foreign","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","koRpus","leaps","lmtest","markdown","memisc","mitools",
                       "pander","pbapply","PerformanceAnalytics","plm","plyr","psych","quantreg","R.oo","R2wd",
                       "reporttools","reshape2","rms","RSQLite","sandwich","sqldf","stargazer","stringr",
                       "texreg","taRifx","tm","UsingR","xtable","zoo")
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

identifier <- "fund_id"

start_year <- 1994
end_year <- 2011

#descriptive_stats_tables <- ListTables(descriptive_stats_db)
#descriptive_stats_fields <- ListFields(descriptive_stats_db)

data_all0 <- read.csv(file=paste(output_directory,"data_all",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)


###############################################################################
cat("WINSORIZE", "\n")
###############################################################################

winsorize_vars <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                    "avg_grade_level_ios","avg_grade_level_acf_ios","avg_grade_level_ac_ios",
                    "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
                    "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
                    "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios")

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
cat("IMPORT WORD LISTS", "\n")
###############################################################################

words_litigious <- read.csv(file=paste(input_directory,"\\","Loughran_McDonald","\\","LoughranMcDonald_Litigious",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

words_modalstrong <- read.csv(file=paste(input_directory,"\\","Loughran_McDonald","\\","LoughranMcDonald_ModalStrong",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

words_modalweak <- read.csv(file=paste(input_directory,"\\","Loughran_McDonald","\\","LoughranMcDonald_ModalWeak",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

words_negative <- read.csv(file=paste(input_directory,"\\","Loughran_McDonald","\\","LoughranMcDonald_Negative",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

words_positive <- read.csv(file=paste(input_directory,"\\","Loughran_McDonald","\\","LoughranMcDonald_Positive",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

words_uncertainty <- read.csv(file=paste(input_directory,"\\","Loughran_McDonald","\\","LoughranMcDonald_Uncertainty",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)


###############################################################################
cat("STEM WORD LISTS", "\n")
###############################################################################

words_litigious_dt <- data.table(words_litigious, key = c("TEXT"))
words_litigious_full0 <- words_litigious_dt[,list(STEM=stem_words(TEXT,"")),by="TEXT"]
words_litigious_full0 <- as.data.frame(words_litigious_full0,stringsAsFactors=FALSE)
words_litigious_full <- data.frame(TEXT=unique(sort(toupper(c(words_litigious_full0[,"TEXT"],
                                                              words_litigious_full0[,"STEM"])))),stringsAsFactors=FALSE)

rm2(words_litigious,words_litigious_dt,words_litigious_full0)

words_modalstrong_dt <- data.table(words_modalstrong, key = c("TEXT"))
words_modalstrong_full0 <- words_modalstrong_dt[,list(STEM=stem_words(TEXT,"")),by="TEXT"]
words_modalstrong_full0 <- as.data.frame(words_modalstrong_full0,stringsAsFactors=FALSE)
words_modalstrong_full <- data.frame(TEXT=unique(sort(toupper(c(words_modalstrong_full0[,"TEXT"],
                                                                words_modalstrong_full0[,"STEM"])))),stringsAsFactors=FALSE)

rm2(words_modalstrong,words_modalstrong_dt,words_modalstrong_full0)

words_modalweak_dt <- data.table(words_modalweak, key = c("TEXT"))
words_modalweak_full0 <- words_modalweak_dt[,list(STEM=stem_words(TEXT,"")),by="TEXT"]
words_modalweak_full0 <- as.data.frame(words_modalweak_full0,stringsAsFactors=FALSE)
words_modalweak_full <- data.frame(TEXT=unique(sort(toupper(c(words_modalweak_full0[,"TEXT"],
                                                              words_modalweak_full0[,"STEM"])))),stringsAsFactors=FALSE)

rm2(words_modalweak,words_modalweak_dt,words_modalweak_full0)

words_negative_dt <- data.table(words_negative, key = c("TEXT"))
words_negative_full0 <- words_negative_dt[,list(STEM=stem_words(TEXT,"")),by="TEXT"]
words_negative_full0 <- as.data.frame(words_negative_full0,stringsAsFactors=FALSE)
words_negative_full <- data.frame(TEXT=unique(sort(toupper(c(words_negative_full0[,"TEXT"],
                                                             words_negative_full0[,"STEM"])))),stringsAsFactors=FALSE)

rm2(words_negative,words_negative_dt,words_negative_full0)

words_positive_dt <- data.table(words_positive, key = c("TEXT"))
words_positive_full0 <- words_positive_dt[,list(STEM=stem_words(TEXT,"")),by="TEXT"]
words_positive_full0 <- as.data.frame(words_positive_full0,stringsAsFactors=FALSE)
words_positive_full <- data.frame(TEXT=unique(sort(toupper(c(words_positive_full0[,"TEXT"],
                                                             words_positive_full0[,"STEM"])))),stringsAsFactors=FALSE)

rm2(words_positive,words_positive_dt,words_positive_full0)

words_uncertainty_dt <- data.table(words_uncertainty, key = c("TEXT"))
words_uncertainty_full0 <- words_uncertainty_dt[,list(STEM=stem_words(TEXT,"")),by="TEXT"]
words_uncertainty_full0 <- as.data.frame(words_uncertainty_full0,stringsAsFactors=FALSE)
words_uncertainty_full <- data.frame(TEXT=unique(sort(toupper(c(words_uncertainty_full0[,"TEXT"],
                                                                words_uncertainty_full0[,"STEM"])))),stringsAsFactors=FALSE)

rm2(words_uncertainty,words_uncertainty_dt,words_uncertainty_full0)


# ###############################################################################
# cat("IMPORT TEXT", "\n")
# ###############################################################################
# 
# data_text <- read.csv(file=paste(output_directory,"sample_data_all",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
# 
# colnames(data_text)[match("Fund_ID",names(data_text))] <- "fund_id"
# colnames(data_text)[match("Fund_Name",names(data_text))] <- "fund_name"
# 
# for(i in which(sapply(data_text,class)=="character"))
# {
#   data_text[[i]] = trim(data_text[[i]])
# }
# for (i in 1:ncol(data_text))
# {
#   data_text[,i] <- unknownToNA(data_text[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
#                                                                     NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
#                                                                     NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
#   data_text[,i] <- ifelse(is.na(data_text[,i]),NA, data_text[,i])
# } 


# ###############################################################################
# cat("IMPORT TOKENS", "\n")
# ###############################################################################
# 
# data_tokens <- read.csv(file=paste(output_directory,"tokens_all_ios_f_full",".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
# 
# colnames(data_tokens)[match("Fund_ID",names(data_tokens))] <- "fund_id"
# 


###############################################################################
cat("COMPUTE TONE", "\n")
###############################################################################

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

#Readability columns table
readbl_vars_cols_count <- 4
readbl_vars_cols <- temp_data_cols[1:readbl_vars_cols_count,]
readbl_vars_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="column",stringsAsFactors=FALSE)
readbl_vars_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="suffix",stringsAsFactors=FALSE)
readbl_vars_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="readabilitystats_table",stringsAsFactors=FALSE)
readbl_vars_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token_table",stringsAsFactors=FALSE)
readbl_vars <- as.data.frame(matrix(NA, ncol=readbl_vars_cols_count, nrow=1),stringsAsFactors=FALSE)
colnames(readbl_vars) <- readbl_vars_cols[,6]

#List variables to compute readability statistics and the suffixes used for each in the sample_data_statistics_cols table
readbl_vars[,1] <- c("Strategy")
readbl_vars[,2] <- c("_ios")
readbl_vars[,3] <- c("read_stats_ios_f")
readbl_vars[,4] <- c("tokens_all_ios_f")

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

for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  
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
  #Remove words;
  #==============================================================================;
  
  tokens_all_temp <- tokens_all_temp[order(tokens_all_temp[,"Fund_ID"],tokens_all_temp[,"yr"]),] 
  
  #tokens_all_temp_trim <- unique(tokens_all_temp[(tokens_all_temp[,"Remove"]==0),])
  tokens_all_temp_trim <- tokens_all_temp[(tokens_all_temp[,"Remove"]==0),]
  
  colnames(tokens_all_temp_trim)[match("token",names(tokens_all_temp_trim))] <- "token_org"
  colnames(tokens_all_temp_trim)[match("desc",names(tokens_all_temp_trim))] <- "token_stemmed"
  tokens_all_temp_trim[,"token_stemmed"] <- NA
  
  
  #   #==============================================================================;
  #   #Split hyphens;
  #   #==============================================================================;
  # 
  #   tokens_all_temp_expand <- ddply(.data=tokens_all_temp_trim, .variables=c("Fund_ID","yr"), .fun = function(x,split){
  #     
  #     # x <- tokens_all_temp_trim[(tokens_all_temp_trim[,"Fund_ID"]==5002 & tokens_all_temp_trim[,"yr"]==1991),]
  #     # split <- "-"
  #     
  #     id <-  unique(x[,"Fund_ID"])
  #     yr <-  unique(x[,"yr"])
  #   
  #     x_out  <- data.frame(Fund_ID=id,yr=yr,token_org=unlist(strsplit(x[,"token_org"], split)),token_stemmed=NA,Remove=0,stringsAsFactors=FALSE)
  #     return(x_out)
  #     
  #   }, split="-",.progress = "text")
  #   
  
  
  #==============================================================================;
  #Stem Words;
  #==============================================================================;
  
  tokens_all_temp_dt <- data.table(tokens_all_temp_trim, key = c("Fund_ID","yr"))
  
  #tokens_all_temp1 <- tokens_all_temp_dt[,list(word=stem_words(token,myStopwords_all)),by="Fund_ID,yr"]
  tokens_all_temp1 <- tokens_all_temp_dt[,list(word=stem_words(token_org,"")),by="Fund_ID,yr"]
  tokens_all_temp1 <- as.data.frame(tokens_all_temp1,stringsAsFactors=FALSE)
  
  #tokens_all_temp1 <- as.data.frame(tokens_all_temp_dt,stringsAsFactors=FALSE)
  #colnames(tokens_all_temp1)[match("token",names(tokens_all_temp1))] <- "word"
  
  tokens_all_temp_stemmed  <- data.frame(Fund_ID=tokens_all_temp1[,"Fund_ID"],
                                         yr=tokens_all_temp1[,"yr"],
                                         token_stemmed=tokens_all_temp1[,"word"],
                                         Remove=0,stringsAsFactors=FALSE)
  
  rm2(tokens_all_temp,tokens_all_temp_dt,tokens_all_temp1)
  
  #==============================================================================;
  #Combine stemmed and regular tokens;
  #==============================================================================;
  
  tokens_all_temp_comb <- tokens_all_temp_trim
  tokens_all_temp_comb[,"token_stemmed"] <- tokens_all_temp_stemmed[,"token_stemmed"]
  
  tokens_all_temp_comb[,"token_org"] <- toupper(tokens_all_temp_comb[,"token_org"])
  tokens_all_temp_comb[,"token_stemmed"] <- toupper(tokens_all_temp_comb[,"token_stemmed"])
  
  rm2(tokens_all_temp_trim,tokens_all_temp_stemmed)
  
  #==============================================================================;
  #Get tone totals;
  #==============================================================================;
  
  tokens_all_temp_totals <- data.frame(tokens_all_temp_comb[,c("Fund_ID","yr","token_org","token_stemmed")],
                                       matrix(NA, ncol=6, nrow=nrow(tokens_all_temp_comb), 
                                              dimnames=list(c(),c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty"))),
                                       stringsAsFactors=FALSE)
  
  rm2(tokens_all_temp_comb)
  
  tokens_all_temp_totals[,"per_litigious"] <- ifelse((tokens_all_temp_totals[,"token_org"] %in% words_litigious_full[,"TEXT"] | 
                                                        tokens_all_temp_totals[,"token_stemmed"] %in% words_litigious_full[,"TEXT"]),1,0)
  
  tokens_all_temp_totals[,"per_modalstrong"] <- ifelse((tokens_all_temp_totals[,"token_org"] %in% words_modalstrong_full[,"TEXT"] | 
                                                          tokens_all_temp_totals[,"token_stemmed"] %in% words_modalstrong_full[,"TEXT"]),1,0)
  
  tokens_all_temp_totals[,"per_modalweak"] <- ifelse((tokens_all_temp_totals[,"token_org"] %in% words_modalweak_full[,"TEXT"] | 
                                                        tokens_all_temp_totals[,"token_stemmed"] %in% words_modalweak_full[,"TEXT"]),1,0)
  
  tokens_all_temp_totals[,"per_negative"] <- ifelse((tokens_all_temp_totals[,"token_org"] %in% words_negative_full[,"TEXT"] | 
                                                       tokens_all_temp_totals[,"token_stemmed"] %in% words_negative_full[,"TEXT"]),1,0)
  
  tokens_all_temp_totals[,"per_positive"] <- ifelse((tokens_all_temp_totals[,"token_org"] %in% words_positive_full[,"TEXT"] | 
                                                       tokens_all_temp_totals[,"token_stemmed"] %in% words_positive_full[,"TEXT"]),1,0)
  
  tokens_all_temp_totals[,"per_uncertainty"] <- ifelse((tokens_all_temp_totals[,"token_org"] %in% words_uncertainty_full[,"TEXT"] | 
                                                          tokens_all_temp_totals[,"token_stemmed"] %in% words_uncertainty_full[,"TEXT"]),1,0)
  
  tokens_all_temp_totals_collapse <- ddply(.data=tokens_all_temp_totals, .variables=c("Fund_ID","yr"), .fun = function(x){
    
    # x <- tokens_all_temp_totals[(tokens_all_temp_totals[,"Fund_ID"]==5002 & tokens_all_temp_totals[,"yr"]==1991),]
    
    x[,"per_litigious"] <- sum(x[,"per_litigious"])/nrow(x)
    x[,"per_modalstrong"] <- sum(x[,"per_modalstrong"])/nrow(x)
    x[,"per_modalweak"] <- sum(x[,"per_modalweak"])/nrow(x)
    x[,"per_negative"] <- sum(x[,"per_negative"])/nrow(x)
    x[,"per_positive"] <- sum(x[,"per_positive"])/nrow(x)
    x[,"per_uncertainty"] <- sum(x[,"per_uncertainty"])/nrow(x)
    
    x_out <- unique(x[,c("Fund_ID","yr","per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")])
    
    return(x_out)
    
  },.progress = "text")
  
  rm2(tokens_all_temp_totals)
  
  #OUTPUT
  
  write.csv(tokens_all_temp_totals_collapse, file=paste(output_directory,"tone_stats",readbl_vars[m,2],".csv",sep=""),row.names=FALSE)
  
  rm2(tokens_all_temp_totals_collapse)
  
  
  #==============================================================================;
  #CREATE PROGRESS OUTPUTS;
  #==============================================================================;
  
  progress_function(outer_loop_count=m, outer_loop_start_val=1, outer_loop_end_val=nrow(readbl_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
  #END OF M FOR LOOP
  
}


#==============================================================================;
#Merge Tone Stats;
#==============================================================================;

m <- 1

tone_stats <- read.csv(file=paste(output_directory,"tone_stats",readbl_vars[m,2],".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

colnames(tone_stats)[match("Fund_ID",names(tone_stats))] <- "fund_id"


data_all_tone <- merge(data_all, tone_stats, 
                       by.x=c("fund_id","yr"), by.y=c("fund_id","yr"), 
                       all.x=TRUE, all.y=FALSE, sort=FALSE,suffixes=c(".x",".y"))


write.csv(data_all_tone, file=paste(output_directory,"data_all_tone",".csv",sep=""),row.names=FALSE)



