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


regression_expand <- function(x){
  
  #x <- regression_equations1
  
  require(plyr)
  
  list_index <- 0
  regression_equations_comb1_temp <-  ldply(x, function(x){
    
    #x <- x[[1]]
    #x <- x[[2]]
    
    list_index <<- list_index + 1
    
    regression_expand_temp0 <- expand.grid(as.vector(unlist(x["dep_var"])),as.vector(unlist(x["models"])),
                                           as.vector(unlist(x["model_type"])),as.vector(unlist(x["note"])))
    colnames(regression_expand_temp0) <- c("dep_var","indep_var","model_type","note")
    
    regression_expand_temp0 <- regression_expand_temp0[do.call(order,regression_expand_temp0[c("dep_var")]),]
    row.names(regression_expand_temp0) <- seq(nrow(regression_expand_temp0))
    
    dep_index <- 0
    regression_expand_temp1 <- ddply(.data=regression_expand_temp0, .variables="dep_var", 
                                     .fun = function(x){ dep_index <<- dep_index + 1
                                                         data.frame(dep_index=dep_index, x, model_index=seq(1,nrow(x)),stringsAsFactors=FALSE)}, 
                                     .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
    
    rm(dep_index,regression_expand_temp0)
    
    regression_expand_temp1[,c("dep_index")] <- as.integer(regression_expand_temp1[,c("dep_index")])
    regression_expand_temp1[,c("model_index")] <- as.integer(regression_expand_temp1[,c("model_index")])
    
    regression_expand_temp1 <- regression_expand_temp1[do.call(order,regression_expand_temp1[c("dep_var")]),]
    row.names(regression_expand_temp1) <- seq(nrow(regression_expand_temp1))
    
    regression_expand_temp2 <- data.frame(lapply(regression_expand_temp1, as.character), stringsAsFactors=FALSE)
    
    rm(regression_expand_temp1)
    
    time_frame_temp0 <-  x["time_frame"]
    time_frame_temp1 <- do.call(cbind,time_frame_temp0[[1]])
    
    rm(time_frame_temp0)
    
    time_frame_temp2 <- data.frame(time_frame_temp1, stringsAsFactors=FALSE)
    colnames(time_frame_temp2) <- c("beg_years","end_years")
    
    rm(time_frame_temp1)
    
    regression_expand_temp3 <- do.call(rbind, replicate(nrow(time_frame_temp2), coredata(regression_expand_temp2), simplify = FALSE))
    #time_frame_temp3 <- replicate(nrow(regression_expand_temp2), coredata(time_frame_temp2), simplify = FALSE)
    
    time_frame_temp3 <- adply(.data=time_frame_temp2, .margins=1, 
                              .fun = function(x,count){ 
                                #x <- time_frame_temp2[1,]
                                #count <- nrow(regression_expand_temp2)
                                do.call(rbind, replicate(count, coredata(x), simplify = FALSE))
                              }, count=nrow(regression_expand_temp2),
                              .expand = TRUE, .progress = "none", .inform = FALSE, .parallel = FALSE,.paropts = NULL)
    
    rm(regression_expand_temp2,time_frame_temp2)
    
    regression_comb <- data.frame(list_index=list_index,
                                  time_frame_temp3,
                                  regression_expand_temp3, 
                                  stringsAsFactors=FALSE)
    
    rm(time_frame_temp3,regression_expand_temp3)
    
    regression_comb[,"indep_var"] <- gsub(pattern="  ", replacement=" ", x=regression_comb[,"indep_var"])
    regression_comb[,"indep_var"] <- gsub(pattern="  ", replacement=" ", x=regression_comb[,"indep_var"])
    regression_comb[,"indep_var"] <- gsub(pattern="  ", replacement=" ", x=regression_comb[,"indep_var"])
    regression_comb[,"indep_var"] <- gsub(pattern="  ", replacement=" ", x=regression_comb[,"indep_var"])
    regression_comb[,"indep_var"] <- gsub("^\\s+|\\s+$", "", regression_comb[,"indep_var"])
    
    return(regression_comb)
  })
  rm(list_index)
  
  regression_equations_comb1_unique1 <- unique(regression_equations_comb1_temp[,c("beg_years","end_years")])
  
  regression_equations_comb1_unique2 <- data.frame(date_index=seq(1,nrow(regression_equations_comb1_unique1)),
                                                   regression_equations_comb1_unique1,
                                                   stringsAsFactors=FALSE)
  
  regression_equations_comb1 <- merge(regression_equations_comb1_temp, regression_equations_comb1_unique2, 
                                      by.x=c("beg_years","end_years"), by.y=c("beg_years","end_years"), 
                                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(regression_equations_comb1_unique1,regression_equations_comb1_unique2)
  
  #date_index <- 0
  #regression_equations_comb1 <- ddply(.data=regression_equations_comb1_temp, .variables=c("beg_years","end_years"), 
  #                                    .fun = function(x){ date_index <<- date_index + 1; data.frame(date_index=date_index,x,stringsAsFactors=FALSE)}, 
  #                                    .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
  #rm(date_index)
  
  beginning_cols1 <- c("date_index","list_index","dep_index","model_index")
  regression_equations_comb1 <- regression_equations_comb1[,c(beginning_cols1,
                                                              colnames(regression_equations_comb1[,!(colnames(regression_equations_comb1) %in% beginning_cols1)]))]
  
  rm(beginning_cols1)
  
  regression_equations_comb1[,c("date_index")] <- as.integer(regression_equations_comb1[,c("date_index")])
  regression_equations_comb1[,c("list_index")] <- as.integer(regression_equations_comb1[,c("list_index")])
  regression_equations_comb1[,c("dep_index")] <- as.integer(regression_equations_comb1[,c("dep_index")])
  regression_equations_comb1[,c("model_index")] <- as.integer(regression_equations_comb1[,c("model_index")])
  
  regression_equations_comb1 <- regression_equations_comb1[order(regression_equations_comb1[,"date_index"],
                                                                 regression_equations_comb1[,"list_index"],
                                                                 regression_equations_comb1[,"dep_index"],
                                                                 regression_equations_comb1[,"model_index"]),]
  row.names(regression_equations_comb1) <- seq(nrow(regression_equations_comb1))
  
  regression_equations_comb2 <- data.frame(regression_equations_comb1,
                                           outname_short=NA,
                                           stringsAsFactors=FALSE)
  
  rm(regression_equations_comb1)
  
  regression_equations_comb2[,c("outname_short")] <- paste(regression_equations_comb2[,c("dep_var")],
                                                           regression_equations_comb2[,c("beg_years")],
                                                           regression_equations_comb2[,c("end_years")],
                                                           regression_equations_comb2[,c("note")],
                                                           sep="_")
  
  beginning_cols2 <- c("date_index","list_index","dep_index","model_index","outname_short","note","beg_years","end_years","model_type")
  regression_equations_comb2 <- regression_equations_comb2[,c(beginning_cols2,
                                                              colnames(regression_equations_comb2[,!(colnames(regression_equations_comb2) %in% beginning_cols2)]))]
  
  rm(beginning_cols2)
  
  return(regression_equations_comb2)
}

regression_read_adjustment <- function(equations_df,equations_col,name_col,read_prefix,read_list,index_list){
  
  #equations_df <- regression_equations_expand2
  #equations_col <- "indep_var"
  #name_col <- "outname_short"
  #read_prefix <- "XXX"
  #read_list <- "ios"
  #index_list <- c("date_index","list_index","dep_index","model_index")
  
  require(plyr)
  
  read_index <- 0
  
  equations_adjusted <- ldply(.data=read_list, .fun = function(x,equations_df,equations_col,name_col,read_prefix){
    
    #x <- read_list[1]
    
    read_index <<- read_index + 1
    
    equations_df[,equations_col] <- gsub(read_prefix,x, equations_df[,equations_col])
    
    equations_df[,c(name_col)] <- paste(equations_df[,c(name_col)],x,sep="_")
    
    data_temp <- data.frame(read_index=read_index, equations_df,stringsAsFactors=FALSE)
    
  }, equations_df=equations_df, equations_col=equations_col,name_col=name_col,read_prefix=read_prefix,
  .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)
  
  rm(read_index)
  
  equations_adjusted2 <- equations_adjusted[,c(index_list,
                                               colnames(equations_adjusted[,!(colnames(equations_adjusted) %in% index_list)]))]
  
  return(equations_adjusted2)
}

regression_sim_adjustment <- function(equations_df,equations_col,name_col,sim_prefix,sim_list,index_list){
  
  #equations_df <- regression_equations_expand2
  #equations_col <- "indep_var"
  #name_col <- "outname_short"
  #sim_prefix <- "YYYpct"
  #sim_list <- sim_type2
  #index_list <- c("date_index","list_index","dep_index","model_index")
  
  require(plyr)
  
  sim_index <- 0
  
  equations_adjusted <- ldply(.data=sim_list, .fun = function(x,equations_df,equations_col,name_col,sim_prefix){
    
    #x <- sim_list[1]
    
    sim_index <<- sim_index + 1
    
    equations_df[,equations_col] <- gsub(sim_prefix,x, equations_df[,equations_col])
    
    equations_df[,c(name_col)] <- paste(equations_df[,c(name_col)],x,sep="_")
    
    data_temp <- data.frame(sim_index=sim_index, equations_df,stringsAsFactors=FALSE)
    
  }, equations_df=equations_df, equations_col=equations_col,name_col=name_col,sim_prefix=sim_prefix,
  .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)
  
  rm(sim_index)
  
  equations_adjusted2 <- equations_adjusted[,c(index_list,
                                               colnames(equations_adjusted[,!(colnames(equations_adjusted) %in% index_list)]))]
  
  return(equations_adjusted2)
}

regression_correlation <- function(x,data,decimals,read_abrev,sim_abrev,additional_vars){
  
  require(plyr)
  
  #x <- regression_equations_final1
  #x <- regression_equations_final2
  #data_all <- data_all
  #decimals <- corr_decimals
  #read_abrev <- c('ios')
  #sim_abrev <- c('050pct','900pct')
  #additional_vars <- c("pflow_lag1","mktadjret_lag1","exret_lag1")
  #additional_vars <- c("pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4","mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4","exret_lag1","exret_lag2","exret_lag3","exret_lag4")
  
  
  regression1_correlation1 <- c(na.omit(as.character(unlist(x[,"indep_var"], use.names=FALSE))))
  regression1_correlation2 <- paste(regression1_correlation1, sep="", collapse="+")
  rm(regression1_correlation1)
  
  regression1_correlation3 <- unique(strsplit(gsub("\\+", "\\1 ", regression1_correlation2), " ")[[1]])
  rm(regression1_correlation2)
  
  #regression1_correlation4 <- gsub("XXX","ios",regression1_correlation3,ignore.case = TRUE)
  regression1_correlation4a <- gsub_expand(myrepl=list(list(pattern=c('XXX'),replacement=read_abrev)), 
                                           regression1_correlation3)
  regression1_correlation4b <- gsub_expand(myrepl=list(list(pattern=c('YYYpct'),replacement=sim_abrev)), 
                                           regression1_correlation4a)
  regression1_correlation4 <- unique(regression1_correlation4b)
  rm(regression1_correlation3,regression1_correlation4a,regression1_correlation4b)
  
  regression1_correlation5 <- regression1_correlation4[!(regression1_correlation4=="")]
  rm(regression1_correlation4)
  
  dep_vars_list <- unique(c(x[,"dep_var"]))
  
  dep_additional_vars0 <- llply(.data=dep_vars_list,.fun = function(x,dep_additional_vars_temp){
    #x <- dep_vars_list[1]
    #dep_additional_vars <- sort(unique(c(dep_vars_list,additional_vars)))
    
    dep_additional_vars_temp[grep(x, dep_additional_vars_temp, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE)] 
    
  },dep_additional_vars_temp=sort(unique(c(dep_vars_list,additional_vars))), .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL)
  
  dep_additional_vars <-unlist(dep_additional_vars0)
  
  rm(dep_vars_list,dep_additional_vars0)
  
  control_vars_list <- c(read_abrev,sim_abrev)
  
  control_vars0 <- llply(.data=control_vars_list,.fun = function(x,dep_additional_vars_temp){
    #x <- dep_vars[1]
    #dep_additional_vars <- sort(unique(c(dep_vars,additional_vars)))
    
    dep_additional_vars_temp[grep(x, dep_additional_vars_temp, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE)] 
    
  },dep_additional_vars_temp=sort(unique(regression1_correlation5)), .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL)
  
  control_vars <- c(unlist(control_vars0),regression1_correlation5)
  control_vars <- unique(control_vars)
  
  rm(control_vars_list,control_vars0,regression1_correlation5)
  
  regression1_correlation6a <- c(dep_additional_vars,control_vars)
  regression1_correlation6b <- regression1_correlation6a[!(regression1_correlation6a %in% c("factor(yr)"))]
  
  rm(regression1_correlation6a)
  
  regression1_correlation6c <- unique(regression1_correlation6b)
  regression1_correlation6c <- regression1_correlation6c[!is.na(regression1_correlation6c)]
  
  rm(regression1_correlation6b)
  
  regression1_correlation7 <- corstarsl(data_all[,regression1_correlation6c],round=decimals)
  
  rm(regression1_correlation6c)
  
  regression1_correlation8 <- matrix("", ncol=nrow(regression1_correlation7), nrow=nrow(regression1_correlation7), 
                                     dimnames=list(rownames(regression1_correlation7), rownames(regression1_correlation7)))
  
  regression1_correlation7 <- data.frame(lapply(regression1_correlation7, as.character), stringsAsFactors=FALSE)
  
  for (i in 1:ncol(regression1_correlation7))
  {
    
    temp_col_name <- colnames(regression1_correlation7)[i]
    regression1_correlation8[,temp_col_name] <- regression1_correlation7[,temp_col_name]
    rm(temp_col_name)
  }
  rm(regression1_correlation7,i)
  
  diag(regression1_correlation8) <- paste(format(1.0, digits = decimals, nsmall=decimals),"***",sep="")
  
  regression1_correlation8 <- data.frame(var=row.names(regression1_correlation8),regression1_correlation8, stringsAsFactors=FALSE)
  row.names(regression1_correlation8) <- seq(nrow(regression1_correlation8))
  
  return(regression1_correlation8)
}


regression_execute <- function(equations_df,data_all,date_index_var,id,output_dir){
  
  #equations_df <- regression_equations_final2
  #data_all <- data_all
  #date_index_var <- "date_index"
  #id <- identifier
  #output_dir <- output_directory_reg_similarity
  
  
  require(plyr)
  
  invisible(d_ply(.data=equations_df, .variables=date_index_var,
                  .fun = function(x,data_all,id,output_dir){
                    
                    #x <- regression_equations_final2[regression_equations_final2[,"date_index"]==1,]
                    #x <- regression_equations_final2[regression_equations_final2[,"date_index"]==2,]
                    #data_all <- data_all
                    #id <- identifier
                    #output_dir <- output_directory_reg_similarity
                    
                    Start_yr_temp <- as.integer(unique(x["beg_years"]))
                    End_yr_temp <- as.integer(unique(x["end_years"]))
                    
                    cat("\n","START YEAR:", Start_yr_temp, "END YEAR:", End_yr_temp,"\n")
                    
                    data_temp <- data_all
                    data_temp.pd <- pdata.frame(data_all, index=c(id, "yr_month"), drop.index=TRUE, row.names=TRUE)
                    
                    data_temp <- data_temp[(data_temp[,"yr"]>=Start_yr_temp & data_temp[,"yr"]<=End_yr_temp),]
                    row.names(data_temp) <- seq(nrow(data_temp))
                    
                    data_temp.pd <- pdata.frame(data_temp, index=c(id, "yr_month"), drop.index=TRUE, row.names=TRUE)
                    
                    d_ply(.data=x, .variables=c("list_index","dep_index","read_index","sim_index"), 
                          .fun = function(y,data_temp.pd,data_temp,id,output_dir){
                            
                            #y <- x[(x[,"list_index"]==1 & x[,"dep_index"]==1 & x[,"read_index"]==1 & x[,"sim_index"]==1),]
                            
                            dep_var_temp <- unique(y[,c("dep_var")])
                            note_temp <- unique(y[,c("note")])
                            
                            model_type_temp <- unique(y[,c("model_type")])
                            
                            #out_file_name <- paste("reg_compare_plm",dep_var_temp,name_short,note_temp,sep="_")
                            out_file_name <- paste("reg_compare_plm",unique(y[,c("outname_short")]),sep="_")
                            
                            note_temp_clean1 <- gsub("_", " ", note_temp, perl=TRUE)
                            note_temp_clean2 <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", note_temp_clean1, perl=TRUE)
                            
                            regressions_temp <- dlply(.data=y, .variables="model_index", 
                                                      .fun = function(z,data_temp.pd,data_temp,model_type_temp,id){ 
                                                        
                                                        #l <- 1
                                                        #l <- 2
                                                        #z <- y[l,]
                                                        
                                                        model_count <- as.integer(unique(z[,c("model_index")]))
                                                        
                                                        ind_vars_reg0 <- z[,"indep_var"]
                                                        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
                                                        reg0 <- plm(as.formula(paste(z[,"dep_var"],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type_temp)
                                                        #reg0 <- lm(as.formula(paste(z[,"dep_var"],ind_vars_reg0,sep="~")), data_temp)
                                                        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
                                                        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,id])
                                                        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
                                                        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,id], data_temp[,"month"])
                                                        #reg0_rse <- mcl(data_temp,reg0, data_temp[,id], data_temp[,"month"])
                                                        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
                                                        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
                                                        
                                                        rm(model_count,ind_vars_reg0)
                                                        
                                                        return(list(reg0,reg0_rse))
                                                        
                                                      }, 
                                                      data_temp.pd=data_temp.pd, data_temp=data_temp, model_type_temp=model_type_temp, id=id,
                                                      .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                            
                            
                            reg <- sapply(regressions_temp, "[",1)
                            
                            rse <- sapply(regressions_temp, "[[",2)
                            
                            se <- llply(.data=rse, .fun = function(w){w[,4]}, 
                                        .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
                            
                            pval <- llply(.data=rse, .fun = function(w){w[,4]}, 
                                          .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
                            
                            cat("\n")
                            
                            htmlreg(l=reg, 
                                    model.names=paste("(",seq(1,nrow(y)),")",sep=""),
                                    override.se=se,
                                    override.pval=pval,
                                    stars=c(0.01, 0.05, 0.1), digits=3, 
                                    caption=paste("Effect Of",note_temp_clean2,"On Hedge Fund Flows – Multivariate",sep=" "),
                                    file=paste(output_dir,out_file_name,".doc",sep=""))
                            
                            rm(dep_var_temp,note_temp,model_type_temp,out_file_name,note_temp_clean1,note_temp_clean2)
                            rm(regressions_temp,reg,rse,se,pval)
                            
                          }, 
                          data_temp.pd=data_temp.pd, data_temp=data_temp,id=id,output_dir=output_dir,
                          .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                    
                    rm(Start_yr_temp,End_yr_temp,data_temp,data_temp.pd)
                    
                  },
                  data_all=data_all,id=id,output_dir=output_dir,
                  .progress = "text", .inform = FALSE,.print = FALSE, .parallel = FALSE, .paropts = NULL))
  
}


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
#descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")
#data_fulll_db <- paste(output_directory,"Data_full.s3db",sep="")

###############################################################################
cat("IMPORT DATA", "\n")
###############################################################################

identifier <- "fund_id"

beg_year <- 1994
end_year <- 2011

#descriptive_stats_tables <- ListTables(descriptive_stats_db)
#descriptive_stats_fields <- ListFields(descriptive_stats_db)

corr_decimals <- 3

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
cat("PANEL REGRESSION - VARIABLES", "\n")
###############################################################################

pattern_str <- "kink_percent_90  + indexrsq_percent_90 + ar_1_percent_90 + num_zero_percent_90 + uniform_percent_90 + string_percent_90 + num_pairs_percent_90 + per_negative_percent_90"
quality_str <- "num_zero_percent_90 + uniform_percent_90 + string_percent_90 + num_pairs_percent_90 + per_negative_percent_90"
nonquality_str <- "kink_percent_90  + indexrsq_percent_90 + ar_1_percent_90"
pb_str <- "kink_percent_90  + indexrsq_percent_90 + ar_1_percent_90 + num_zero_percent_90 + uniform_percent_90 + string_percent_90"

###############################################################################
cat("PANEL REGRESSION - READBILITY", "\n")
###############################################################################

output_directory_reg_readability <- paste(output_directory,"reg_readability","\\",sep="")
create_directory(output_directory_reg_readability,remove=1)

#Regression equations
regression_equations1_1 <- list(#time_frame=list(c(beg_year,2000,2000,2006),
                                #                c(end_year,2011,2005,2011)),
                                time_frame=list(c(beg_year),c(end_year)),
                                dep_var=c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios","avg_grade_level_ios"),
                                models=c("quality_score_trim2_90",
                                         pattern_str,
                                         pb_str,
                                         "quality_score_trim2_90 + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         paste(pattern_str, "    + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",sep=""),
                                         paste(pb_str, "         + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",sep=""),
                                         "quality_score_trim2_90 + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",
                                         paste(pattern_str, "    + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",sep=""),
                                         paste(pb_str, "         + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",sep="")),
                                model_type=c("pooling"),
                                note=c("readability"))

# regression_equations1_2 <- list(time_frame=list(c(end_year),c(end_year)),
#                                 #time_frame=list(c(beg_year),c(end_year)),
#                                 dep_var=c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios","avg_grade_level_ios"),
#                                 models=c("quality_score_trim2_90",
#                                          pattern_str,
#                                          quality_str,
#                                          nonquality_str,
#                                          "quality_score_trim2_90 + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
#                                          paste(pattern_str, "    + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",sep=""),
#                                          paste(pattern_str, "    + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",sep=""),
#                                          paste(nonquality_str, " + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",sep="")),
#                                 model_type=c("pooling"),
#                                 note=c("readability"))

#regression_equations1 <- list(regression_equations1_1,regression_equations1_2)
#rm(regression_equations1_1,regression_equations1_2)

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


regression_execute(equations_df=regression_equations_final1,data_all=data_all,date_index_var="date_index",
                   id=identifier,output_dir=output_directory_reg_readability)

rm2(output_directory_reg_readability,regression_equations_final1)


###############################################################################
cat("PANEL REGRESSION - SIMILARITY", "\n")
###############################################################################

output_directory_reg_similarity <- paste(output_directory,"reg_similarity","\\",sep="")
create_directory(output_directory_reg_similarity,remove=1)

#sim_type2 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type2 <- c("050pct","750pct","900pct")

#Regression equations
regression_equations2_1 <- list(#time_frame=list(c(beg_year,2000,2000,2006),
                                #                c(end_year,2011,2005,2011)),
                                time_frame=list(c(beg_year),c(end_year)),
                                #time_frame=list(c(2006),c(2011)),
                                dep_var=c("all_similarity_900pct_ios","main_investment_strategy_similarity_900pct_ios"),
                                models=c("quality_score_trim2_90",
                                         pattern_str,
                                         pb_str,
                                         "quality_score_trim2_90 + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         paste(pattern_str, "    + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",sep=""),
                                         paste(pb_str, "         + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",sep=""),
                                         "quality_score_trim2_90 + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",
                                         paste(pattern_str, "    + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",sep=""),
                                         paste(pb_str, "         + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",sep="")),
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
                                                                  #name_col="outname_short", sim_prefix="YYYpct", sim_list=sim_type2,
                                                                  name_col="outname_short", sim_prefix="YYYpct", sim_list="NOSIM",
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

regression_execute(equations_df=regression_equations_final2,data_all=data_all,date_index_var="date_index",
                   id=identifier,output_dir=output_directory_reg_similarity)

rm2(output_directory_reg_similarity,regression_equations_final2)


###############################################################################
cat("PANEL REGRESSION - READABILITY & SIMILARITY", "\n")
###############################################################################

output_directory_reg_readability_similarity <- paste(output_directory,"reg_readability_similarity","\\",sep="")
create_directory(output_directory_reg_readability_similarity,remove=1)

#sim_type3 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type3 <- c("050pct","750pct","900pct")

#Regression equations
regression_equations3_1 <- list(time_frame=list(c(beg_year,2000,2000,2006),
                                                c(end_year,2011,2005,2011)),
                                #time_frame=list(c(beg_year),c(end_year)),
                                #time_frame=list(c(2006),c(2011)),
                                dep_var=c("pflow","mktadjret","exret"),
                                models=c("                            all_similarity_YYYpct_XXX                                                  + avg_grade_level_XXX",
                                         "                            all_similarity_YYYpct_XXX + ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                                         "       main_investment_strategy_similarity_YYYpct_XXX                                                  + avg_grade_level_XXX",
                                         "       main_investment_strategy_similarity_YYYpct_XXX + ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX", 
                                         "                            all_similarity_YYYpct_XXX                                                  + avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",
                                         "                            all_similarity_YYYpct_XXX + ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",
                                         "       main_investment_strategy_similarity_YYYpct_XXX                                                  + avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",
                                         "       main_investment_strategy_similarity_YYYpct_XXX + ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)",
                                         "                            all_similarity_YYYpct_XXX                                                     + coleman_liau_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin + factor(yr)"),
                                model_type=c("pooling"),
                                note=c("readbility_similarity"))

regression_equations3_2 <- list(time_frame=list(c(beg_year,2000,2000,2006),
                                                c(end_year,2011,2005,2011)),
                                #time_frame=list(c(beg_year),c(end_year)),
                                dep_var=c("int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24","int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24"),
                                models=c("                            all_similarity_YYYpct_XXX                                                  + avg_grade_level_XXX",
                                         "                            all_similarity_YYYpct_XXX + ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                                         "       main_investment_strategy_similarity_YYYpct_XXX                                                  + avg_grade_level_XXX",
                                         "       main_investment_strategy_similarity_YYYpct_XXX + ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX", 
                                         "                            all_similarity_YYYpct_XXX                                                  + avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "                            all_similarity_YYYpct_XXX + ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "       main_investment_strategy_similarity_YYYpct_XXX                                                  + avg_grade_level_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "       main_investment_strategy_similarity_YYYpct_XXX + ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                                         "                            all_similarity_YYYpct_XXX                                                     + coleman_liau_XXX + mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin"),
                                model_type=c("pooling"),
                                note=c("readbility_similarity"))

regression_equations3 <- list(regression_equations3_1,regression_equations3_2)

rm(regression_equations3_1,regression_equations3_2)

regression_equations_expand3 <- regression_expand(regression_equations3)

regression_equations_expand_adj_read3 <- regression_read_adjustment(equations_df=regression_equations_expand3,equations_col="indep_var",
                                                                    name_col="outname_short", read_prefix="XXX", read_list="ios",
                                                                    index_list=c("date_index","list_index","dep_index","model_index"))

regression_equations_expand_adj_sim3 <- regression_sim_adjustment(equations_df=regression_equations_expand_adj_read3,equations_col="indep_var",
                                                                  name_col="outname_short", sim_prefix="YYYpct", sim_list=sim_type3,
                                                                  index_list=c("date_index","list_index","dep_index","model_index","read_index"))

regression_equations_final3 <- regression_equations_expand_adj_sim3
regression_equations_final3 <- regression_equations_final3[order(regression_equations_final3[,"date_index"],regression_equations_final3[,"list_index"],
                                                                 regression_equations_final3[,"dep_index"],regression_equations_final3[,"read_index"],
                                                                 regression_equations_final3[,"sim_index"],regression_equations_final3[,"model_index"]),]
row.names(regression_equations_final3) <- seq(nrow(regression_equations_final3))

rm2(regression_equations_expand3,regression_equations_expand_adj_read3,regression_equations_expand_adj_sim3)


regression_correlation3 <- regression_correlation(x=regression_equations_final3,data=data_all,decimals=corr_decimals,
                                                  read_abrev <- c('ios'), sim_abrev <- sim_type3,
                                                  additional_vars=c("pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
                                                                    "mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
                                                                    "exret_lag1","exret_lag2","exret_lag3","exret_lag4"))

rm2(regression_correlation3,sim_type3)

regression_execute(equations_df=regression_equations_final3,data_all=data_all,date_index_var="date_index",
                   id=identifier,output_dir=output_directory_reg_readability_similarity)

rm2(output_directory_reg_readability_similarity,regression_equations_final3)


###############################################################################
cat("PANEL REGRESSION - READABILITY & SIMILARITY QUARTILES", "\n")
###############################################################################

output_directory_reg_readability_similarity_quartiles <- paste(output_directory,"reg_readability_similarity_quartiles","\\",sep="")
create_directory(output_directory_reg_readability_similarity_quartiles,remove=1)

data_year_groups4 <- data.frame(matrix(NA, ncol=2, nrow=1, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                stringsAsFactors=FALSE)

data_year_groups4[1,] <- c(beg_year,end_year)
#data_year_groups4[2,] <- c(2000,2011)
#data_year_groups4[3,] <- c(2006,2011)
#data_year_groups4[4,] <- c(1994,1999)
#data_year_groups4[5,] <- c(2000,2005)


#dep_var4 <- c("pflow","nflow")
#dep_var4 <- c("pflow")
#dep_var4 <- c("pflow","mktadjret")
dep_var4 <- c("pflow","mktadjret","exret",
              "int_nonloading_ff_24","int_loading_ff_24","int_nonloading_ffm_24","int_loading_ffm_24","int_nonloading_ffml_24","int_loading_ffml_24",
              "int_nonloading_hf7_24","int_loading_hf7_24","int_nonloading_hf8_24","int_loading_hf8_24")

model_type4 <- "pooling"

note4 <- "readbility_similarity_year"

#sim_type4 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
sim_type4 <- c("050pct","900pct")


#Regression equations
regression_equations4 <- data.frame(grade=NA,
                                    similarity=NA,
                                    controls=NA,
                                    quantile=NA,
                                    fixed_effects=NA,
                                    full_independent_vars=NA,
                                    stringsAsFactors=FALSE)
regression_equations4[1,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA,NA,NA)
regression_equations4[2,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA,NA,NA)
regression_equations4[3,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                               NA,NA,NA)
regression_equations4[4,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee",
                               NA,NA,NA)
regression_equations4[5,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations4[6,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,NA,NA)
regression_equations4[7,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)
regression_equations4[8,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)
regression_equations4[9,] <- c("avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + log_aum_lag1 + management_fee + other_fee + listed_on_exchange_bin + domicile_onshore_bin + flagship_bin + dead_bin",
                               NA,
                               NA,#"factor(yr)",
                               NA)

regression_equations4 <- unknown_to_NA(regression_equations4,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations4))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)], use.names=FALSE))))
  regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}

for (k in 1:nrow(data_year_groups4))
{
  #k <- 1
  
  cat("START YEAR:", data_year_groups4[k,1], "END YEAR:", data_year_groups4[k,2],"\n")
  
  data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
  
  for (i in 1:length(dep_var4))  
    #for (i in 4:length(dep_var4))
  {
    #i <- 1
    
    for (j in 1:length(sim_type4))
    {
      #j <- 1
      #j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sim_type4[j],sep="_")
      
      #models <- rep( list(list()), nrow(regression_equations4) )
      se <- rep( list(list()), nrow(regression_equations4) )
      pval <- rep( list(list()), nrow(regression_equations4) )
      
      for (l in 1:nrow(regression_equations4))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
        ind_vars_reg0 <- gsub("YYYpct",sim_type4[j],ind_vars_reg0,ignore.case = TRUE)
        reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
        #reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")), data_temp)
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
        #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
        
        rm2(ind_vars_reg0,reg0,reg0_rse)
        
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),
              override.se=se,
              override.pval=pval,
              stars=c(0.01, 0.05, 0.1), digits=3, 
              caption="Effect of Readability & Similarity on Hedge Fund Flows – Multivariate",
              file=paste(output_directory_reg_readability_similarity_quartiles,out_file_name,".doc",sep=""))
      
      #custom.names
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var4), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type4))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
      
    }
    rm2(j)
    
  } 
  
  rm2(data_temp,data_temp.pd,i)
  
}

rm2(data_year_groups4,dep_var4,model_type4,note4,sim_type4,temp_char_vec,regression_equations4,k)





























###############################################################################
cat("YOUNG FUNDS", "\n")
###############################################################################

data_year_groups4 <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                stringsAsFactors=FALSE)

data_year_groups4[1,] <- c(beg_year,end_year)
data_year_groups4[2,] <- c(2000,2011)
data_year_groups4[3,] <- c(1994,1999)
data_year_groups4[4,] <- c(2000,2005)
data_year_groups4[5,] <- c(2006,2011)

#dep_var4 <- c("pflow","nflow")
dep_var4 <- c("pflow")

model_type4 <- "pooling"

note4 <- "young_readbility_similarity"

sim_type4 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#sim_type4 <- c("050pct","900pct")


#Regression equations
regression_equations4 <- data.frame(grade=NA,
                                    similarity=NA,
                                    controls=NA,
                                    quantile=NA,
                                    fixed_effects=NA,
                                    full_independent_vars=NA,
                                    stringsAsFactors=FALSE)
regression_equations4[1,] <- c("avg_grade_level_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations4[2,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations4[3,] <- c("avg_grade_level_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               NA,NA,NA)
regression_equations4[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               NA,NA,NA)
regression_equations4[5,] <- c(NA,NA,
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               NA,NA)
regression_equations4[6,] <- c(NA,NA,
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA)
regression_equations4[7,] <- c(NA,NA,
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3 + all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA)
regression_equations4[8,] <- c(NA,NA,
                               "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
                               "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3 + all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "factor(yr)",
                               NA)
regression_equations4 <- unknown_to_NA(regression_equations4,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations4))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations4[i,1:(ncol(regression_equations4)-1)], use.names=FALSE))))
  regression_equations4[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}

for (k in 1:nrow(data_year_groups4))
{
  #k <- 1
  
  cat("START YEAR:", data_year_groups4[k,1], "END YEAR:", data_year_groups4[k,2],"\n")
  
  data_temp <- data_all[(data_all[,"yr"]>=data_year_groups4[k,1] & data_all[,"yr"]<=data_year_groups4[k,2]),]
  data_temp <- data_temp[data_temp[,"age_m"]<=36,]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr_month"), drop.index=TRUE, row.names=TRUE)
  
  for (i in 1:length(dep_var4))
  {
    #i <- 1
    
    for (j in 1:length(sim_type4))
    {
      #j <- 1
      #j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var4[i],data_year_groups4[k,1],data_year_groups4[k,2],note4,sim_type4[j],sep="_")
      
      #models <- rep( list(list()), nrow(regression_equations4) )
      se <- rep( list(list()), nrow(regression_equations4) )
      pval <- rep( list(list()), nrow(regression_equations4) )
      
      for (l in 1:nrow(regression_equations4))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations4[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
        ind_vars_reg0 <- gsub("YYYpct",sim_type4[j],ind_vars_reg0,ignore.case = TRUE)
        reg0 <- plm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type4)
        #reg0 <- lm(as.formula(paste(dep_var4[i],ind_vars_reg0,sep="~")), data_temp)
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"month"])
        #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"month"])
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
        
        rm2(ind_vars_reg0,reg0,reg0_rse)
        
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations4)),")",sep=""),
              override.se=se,
              override.pval=pval,
              stars=c(0.01, 0.05, 0.1), digits=3, 
              caption="Effect of Readability & Similarity on Young Hedge Fund Flows – Multivariate",
              file=paste(output_directory,out_file_name,".doc",sep=""))
      
      #custom.names
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var4), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type4))
      
      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations4)),sep="",collapse=","),")",sep="")))
      
    }
    rm2(j)
    
  } 
  
  rm2(data_temp,data_temp.pd,i)
  
}

rm2(data_year_groups4,dep_var4,model_type4,note4,sim_type4,temp_char_vec,regression_equations4,k)


###############################################################################
cat("FLOW VOLATILITY DATA", "\n")
###############################################################################

data_vol_other0 <- data_all[,c(identifier,"yr","sdpct_flow",
                               "age_y","total_fee","performance_fee","management_fee","other_fee",
                               "ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                               "avg_grade_level_ios","avg_grade_level_acf_ios","avg_grade_level_ac_ios",
                               "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
                               "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
                               "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios",
                               "ari_ios_below_quartile1","ari_ios_above_quartile3",
                               "coleman_liau_ios_below_quartile1","coleman_liau_ios_above_quartile3",
                               "flesch_kincaid_ios_below_quartile1","flesch_kincaid_ios_above_quartile3",
                               "fog_ios_below_quartile1","fog_ios_above_quartile3",
                               "smog_ios_below_quartile1","smog_ios_above_quartile3",
                               "avg_grade_level_ios_below_quartile1","avg_grade_level_ios_above_quartile3",
                               "avg_grade_level_acf_ios_below_quartile1","avg_grade_level_acf_ios_above_quartile3",
                               "avg_grade_level_ac_ios_below_quartile1","avg_grade_level_ac_ios_above_quartile3",
                               "all_similarity_050pct_ios_below_quartile1","all_similarity_100pct_ios_below_quartile1","all_similarity_250pct_ios_below_quartile1",
                               "all_similarity_500pct_ios_below_quartile1","all_similarity_750pct_ios_below_quartile1","all_similarity_900pct_ios_below_quartile1",
                               "all_similarity_050pct_ios_above_quartile3","all_similarity_100pct_ios_above_quartile3","all_similarity_250pct_ios_above_quartile3",
                               "all_similarity_500pct_ios_above_quartile3","all_similarity_750pct_ios_above_quartile3","all_similarity_900pct_ios_above_quartile3",
                               "main_investment_strategy_similarity_050pct_ios_below_quartile1","main_investment_strategy_similarity_100pct_ios_below_quartile1",
                               "main_investment_strategy_similarity_250pct_ios_below_quartile1","main_investment_strategy_similarity_500pct_ios_below_quartile1",
                               "main_investment_strategy_similarity_750pct_ios_below_quartile1","main_investment_strategy_similarity_900pct_ios_below_quartile1",
                               "main_investment_strategy_similarity_050pct_ios_above_quartile3","main_investment_strategy_similarity_100pct_ios_above_quartile3",
                               "main_investment_strategy_similarity_250pct_ios_above_quartile3","main_investment_strategy_similarity_500pct_ios_above_quartile3",
                               "main_investment_strategy_similarity_750pct_ios_above_quartile3","main_investment_strategy_similarity_900pct_ios_above_quartile3")]

data_vol_other_dt0 <- data.table(data_vol_other0, key = "fund_id,yr")
#data_vol_other_dt <- data_vol_other_dt0[, tail(.SD, 1), by = key(data_vol_other_dt0)]
data_vol_other_dt <- data_vol_other_dt0[unique(data_vol_other_dt0[,key(data_vol_other_dt0), with = FALSE]), mult = 'last']
data_vol_other <- data.frame(data_vol_other_dt,stringsAsFactors=FALSE)
data_vol_other <- unique(data_vol_other)

rm2(data_vol_other0,data_vol_other_dt0,data_vol_other_dt)

data_vol_dt <- data.table(data_all[c(identifier,"yr","aum","monthly_ret")])
setkeyv(data_vol_dt,c(identifier,"yr"))

ret_annualized1 <- data_vol_dt[, list(aret1=monthly_ret+1),by="fund_id,yr"]
ret_annualized2 <- ret_annualized1[, list(aret2=prod(aret1, na.rm=FALSE)),by="fund_id,yr"]
ret_annualized3 <- ret_annualized2[, list(aret=aret2-1),by="fund_id,yr"]
ret_annualized3 <- as.data.frame(ret_annualized3,stringsAsFactors=FALSE)
ret_annualized <- data.frame(ret_annualized3,
                             aret_lag1=NA,
                             stringsAsFactors=FALSE)
ret_annualized[,"aret_lag1"] <- create_lags2(ret_annualized,"aret",identifier,1)

rm2(ret_annualized1,ret_annualized2,ret_annualized3)

data_averaged0 <- data_vol_dt[, list(a_aum=mean(aum,na.rm=TRUE)),by="fund_id,yr"]
data_averaged0 <- as.data.frame(data_averaged0,stringsAsFactors=FALSE)
data_averaged <- data.frame(data_averaged0,
                            a_aum_log=suppressWarnings(log(data_averaged0[,"a_aum"])),
                            stringsAsFactors=FALSE)

rm2(data_averaged0)

data_vol0 <- merge(data_averaged, ret_annualized, 
                   by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                   all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

rm2(data_averaged,ret_annualized)

data_vol <- merge(data_vol0, data_vol_other, 
                  by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                  all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))

data_vol <- unknown_to_NA(data_vol,unknowns_strings)

rm2(data_vol0,data_vol_other,data_vol_dt)


###############################################################################
cat("FLOW VOLATILITY REGRESSION", "\n")
###############################################################################


data_year_groups5 <- data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("Start_yr","End_yr"))), 
                                stringsAsFactors=FALSE)

data_year_groups5[1,] <- c(beg_year,end_year)
data_year_groups5[2,] <- c(2000,2011)
data_year_groups5[3,] <- c(1994,1999)
data_year_groups5[4,] <- c(2000,2005)
data_year_groups5[5,] <- c(2006,2011)


dep_var5 <- c("sdpct_flow")

model_type5 <- "pooling"

note5 <- "vol_readability_similarity"

sim_type5 <- c("050pct","100pct","250pct","500pct","750pct","900pct")

#Regression equations
regression_equations5 <- data.frame(grade=NA,
                                    similarity=NA,
                                    controls=NA,
                                    quantile=NA,
                                    fixed_effects=NA,
                                    full_independent_vars=NA,
                                    stringsAsFactors=FALSE)
regression_equations5[1,] <- c("avg_grade_level_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations5[2,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               NA,NA,NA,NA)
regression_equations5[3,] <- c("avg_grade_level_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               NA,NA,NA)
regression_equations5[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
                               "all_similarity_YYYpct_XXX + main_investment_strategy_similarity_YYYpct_XXX",
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               NA,NA,NA)
regression_equations5[5,] <- c(NA,NA,
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               "ari_ios_below_quartile1 + ari_ios_above_quartile3 + coleman_liau_ios_below_quartile1 + coleman_liau_ios_above_quartile3 + flesch_kincaid_ios_below_quartile1 + flesch_kincaid_ios_above_quartile3 + fog_ios_below_quartile1 + fog_ios_above_quartile3 + smog_ios_below_quartile1 + smog_ios_above_quartile3",
                               NA,NA)
regression_equations5[6,] <- c(NA,NA,
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               "avg_grade_level_ios_below_quartile1 + avg_grade_level_ios_above_quartile3",
                               NA,NA)
regression_equations5[7,] <- c(NA,NA,
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               NA,NA)
regression_equations5[7,] <- c(NA,NA,
                               "aret_lag1 + a_aum_log + age_y + total_fee",
                               "all_similarity_YYYpct_XXX_below_quartile1 + all_similarity_YYYpct_XXX_above_quartile3 + main_investment_strategy_similarity_YYYpct_XXX_below_quartile1 + main_investment_strategy_similarity_YYYpct_XXX_above_quartile3",
                               "factor(yr)",
                               NA)
regression_equations5 <- unknown_to_NA(regression_equations5,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations5))
{
  
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations5[i,1:(ncol(regression_equations5)-1)], use.names=FALSE))))
  regression_equations5[i,"full_independent_vars"] <- paste(temp_char_vec, sep="", collapse=" + ") 
  
}


for (k in 1:nrow(data_year_groups5))
{
  #k <- 1
  
  cat("START YEAR:", data_year_groups5[k,1], "END YEAR:", data_year_groups5[k,2],"\n")
  
  data_temp <- data_vol[(data_vol[,"yr"]>=data_year_groups5[k,1] & data_vol[,"yr"]<=data_year_groups5[k,2]),]
  data_temp.pd <- pdata.frame(data_temp, index=c(identifier, "yr"), drop.index=TRUE, row.names=TRUE)
  
  for (i in 1:length(dep_var5))
  {
    #i <- 1
    
    for (j in 1:length(sim_type5))
    {
      #j <- 1
      #j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var5[i],data_year_groups5[k,1],data_year_groups5[k,2],note5,sim_type5[j],sep="_")
      
      #models <- rep( list(list()), nrow(regression_equations5) )
      se <- rep( list(list()), nrow(regression_equations5) )
      pval <- rep( list(list()), nrow(regression_equations5) )
      
      for (l in 1:nrow(regression_equations5))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations5[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case = TRUE)
        ind_vars_reg0 <- gsub("YYYpct",sim_type5[j],ind_vars_reg0,ignore.case = TRUE)
        reg0 <- plm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type5)
        #reg0 <- lm(as.formula(paste(dep_var5[i],ind_vars_reg0,sep="~")), data_temp)
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovHC(x, cluster="group", type="HC1"))
        #reg0_rse <- cl.plm(data_temp, reg0, data_temp[,identifier])
        #reg0_rse <- coeftest(reg0, vcov=function(x) vcovDC(x, type="HC1"))
        reg0_rse <- mcl.plm(data_temp, reg0, data_temp[,identifier], data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0, data_temp[,identifier], data_temp[,"yr"])
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""), reg0, envir = .GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""), reg0_rse, envir = .GlobalEnv)
        
        rm2(ind_vars_reg0,reg0,reg0_rse)
        
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations5)),sep="",collapse=","),")",sep=""))), 
              model.names=paste("(",seq(1,nrow(regression_equations5)),")",sep=""),
              override.se=se,
              override.pval=pval,
              stars=c(0.01, 0.05, 0.1), digits=3, 
              caption="Effect of Readability Similarity on Hedge Fund Flow Volatility – Multivariate",
              file=paste(output_directory,out_file_name,".doc",sep=""))
      
      progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(dep_var5), 
                        inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(sim_type5))
      
      rm2(se,pval,out_file_name,l)
      
    }
    rm2(j)
    
  } 
  
  rm2(data_temp,data_temp.pd,i)
  
}

rm2(data_year_groups5,dep_var5,model_type5,note5,sim_type5,temp_char_vec,regression_equations5,k)







###############################################################################
cat("PRINCIPAL COMPONENT - READABILITY", "\n")
###############################################################################

pc_scores_loadings <- function(data_in,variables,suffix,note,tol){
  
  #data_in <- data_all
  #variables <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios")
  #suffix <- "ios"
  #note <- "Readability"
  #tol=NULL
  #tol=0.3
  
  title_full <- ifelse(is.null(tol), paste(note," (Tol = Null)",sep=""), paste(note," (Tol = ",tol,")",sep=""))
  
  pc_temp <- prcomp(~ ., data=data_in[,variables], na.action=na.omit, scale=TRUE)
  
  #Plotting the variances
  plot(pc_temp, main = title_full)
  barplot(pc_temp$sdev/pc_temp$sdev[1], main = title_full)
  
  pc_sum <- summary(pc_temp) 
  
  #Importance (NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign)
  pc_importance <- pc_sum$importance
  pc_importance <- as.matrix(pc_importance)
  pc_importance <- data.frame(type="Importance",
                              var=row.names(pc_importance),
                              pc_importance,
                              PC_avg=rowMeans(pc_importance),
                              stringsAsFactors=FALSE)
  row.names(pc_importance)  <- seq(nrow(pc_importance))
  
  #Loadings (NOT SURE IF AVERAGE IS MAKES SINCE... just using it for sign)
  pc_loadings <- pc_sum$rotation
  pc_loadings <- as.matrix(pc_loadings)
  pc_loadings <- data.frame(type="Loadings",
                            var=row.names(pc_loadings),
                            pc_loadings,
                            PC_avg=rowMeans(pc_loadings),
                            stringsAsFactors=FALSE)
  row.names(pc_loadings)  <- seq(nrow(pc_loadings))
  
  #Importance and Loadings
  pc_importance_loadings <- rbind(pc_importance,pc_loadings)
  row.names(pc_importance_loadings)  <- seq(nrow(pc_importance_loadings))
  rm(pc_importance,pc_loadings)
  
  #Center
  pc_center <- pc_sum$center
  pc_center <- as.matrix(pc_center)
  pc_center <- data.frame(var=row.names(pc_center),
                          pc_center,
                          stringsAsFactors=FALSE)
  row.names(pc_center)  <- seq(nrow(pc_center))
  
  #Scale
  pc_scale <- pc_sum$scale
  pc_scale <- as.matrix(pc_scale)
  pc_scale <- data.frame(var=row.names(pc_scale),
                         pc_scale,
                         stringsAsFactors=FALSE)
  row.names(pc_scale)  <- seq(nrow(pc_scale))
  
  #Center and Scale
  pc_center_scale <- merge(pc_center, pc_scale, 
                           by.x="var", by.y="var", 
                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables = NA)
  row.names(pc_center_scale) <- seq(nrow(pc_center_scale))
  rm(pc_center,pc_scale)
  
  #Scores
  pc_scores <- pc_sum$x
  pc_scores <- as.matrix(pc_scores)
  colnames(pc_scores) <- paste(colnames(pc_scores),suffix,sep="_")
  pc_scores <- data.frame(pc_scores,
                          stringsAsFactors=FALSE)
  row.names(pc_scores)  <- seq(nrow(pc_scores))
  
  #Combine data
  comb_list <- list("Importance_and_Loadings" = pc_importance_loadings, "Center_and_Scale" = pc_center_scale, "Scores" = pc_scores)
  
  rm(pc_importance_loadings,pc_center_scale,pc_scores)
  
  return(comb_list)
  
}

pca_text_read_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios")

#pc_read_ios <- pc_scores_loadings(data_all,pca_text_read_vars_ios,"ios","Readability", NULL)
pc_read_ios <- pc_scores_loadings(data_all,pca_text_read_vars_ios,"ios","Readability", 0.2)

pc_read_ios_loadings <- pc_read_ios$Importance_and_Loadings
pc_read_ios_center <- pc_read_ios$Center_and_Scale
pc_read_ios_scores <- pc_read_ios$Scores

rm2(pca_text_read_vars_ios,pc_read_ios,pc_read_ios_loadings,pc_read_ios_center)


###############################################################################
cat("PRINCIPAL COMPONENT - SIMILARITY", "\n")
###############################################################################

#pca_text_sim_vars_ios <- c("all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
#                           "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
#                           "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios")
pca_text_sim_vars_ios <- c("all_similarity_050pct_ios","main_investment_strategy_similarity_050pct_ios")

#pc_sim_ios <- pc_scores_loadings(data_all,pca_text_sim_vars_ios,"ios","Similarity", NULL)
pc_sim_ios <- pc_scores_loadings(data_all,pca_text_sim_vars_ios,"ios","Similarity", 0.4 )

pc_sim_ios_loadings <- pc_sim_ios$Importance_and_Loadings
pc_sim_ios_center <- pc_sim_ios$Center_and_Scale
pc_sim_ios_scores <- pc_sim_ios$Scores

rm2(pca_text_sim_vars_ios,pc_sim_ios,pc_sim_ios_loadings,pc_sim_ios_center)


###############################################################################
cat("PRINCIPAL COMPONENT - READABILITY & SIMILARITY", "\n")
###############################################################################

# pca_text_both_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
#                             "all_similarity_050pct_ios","all_similarity_100pct_ios","all_similarity_250pct_ios","all_similarity_500pct_ios","all_similarity_750pct_ios","all_similarity_900pct_ios",
#                             "main_investment_strategy_similarity_050pct_ios","main_investment_strategy_similarity_100pct_ios","main_investment_strategy_similarity_250pct_ios",
#                             "main_investment_strategy_similarity_500pct_ios","main_investment_strategy_similarity_750pct_ios","main_investment_strategy_similarity_900pct_ios")
pca_text_both_vars_ios <- c("ari_ios","coleman_liau_ios","flesch_kincaid_ios","fog_ios","smog_ios",
                            "all_similarity_050pct_ios","main_investment_strategy_similarity_050pct_ios")

#pc_both_ios <- pc_scores_loadings(data_all,pca_text_both_vars_ios,"ios","Readability and Similarity", NULL)
pc_both_ios <- pc_scores_loadings(data_all,pca_text_both_vars_ios,"ios","Readability and Similarity", 0.2)

pc_both_ios_loadings <- pc_both_ios$Importance_and_Loadings
pc_both_ios_center <- pc_both_ios$Center_and_Scale
pc_both_ios_scores <- pc_both_ios$Scores

rm2(pca_text_both_vars_ios,pc_both_ios,pc_both_ios_loadings,pc_both_ios_center)


###############################################################################
cat("PRINCIPAL COMPONENT DATA - MERGE", "\n")
###############################################################################

data_all_pc_rn <- data.frame(rn=as.integer(row.names(data_all)),
                             data_all,
                             stringsAsFactors=FALSE)

ios_scores_rn <- data.frame(rn=as.integer(row.names(pc_both_ios_scores)),
                            pc_both_ios_scores,
                            stringsAsFactors=FALSE)

data_all_pc <- merge(data_all_pc_rn, ios_scores_rn, 
                     by.x="rn", by.y="rn", 
                     all.x=TRUE, all.y=FALSE, sort=TRUE, suffixes=c(".x",".y"),incomparables = NA)

data_all_pc <- data_all_pc[order(data_all_pc[,"rn"]),]
data_all_pc <- data_all_pc[,!(colnames(data_all_pc) %in% "rn")]
data_all_pc <- data_all_pc[order(data_all_pc[,identifier],
                                 data_all_pc[,"yr"],
                                 data_all_pc[,"month"]),]
row.names(data_all_pc) <- seq(nrow(data_all_pc))

rm2(data_all_pc_rn,ios_scores_rn)
rm2(pc_read_ios_scores,pc_sim_ios_scores,pc_both_ios_scores)


###############################################################################
cat("PRINCIPAL COMPONENT REGRESSION", "\n")
###############################################################################


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
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations4[4,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + total_fee",
#                                NA,NA,NA)
# regression_equations4[5,] <- c("avg_grade_level_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + management_fee + performance_fee + other_fee",
#                                NA,NA,NA)
# regression_equations4[6,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + management_fee + performance_fee + other_fee",
#                                NA,NA,NA)
# regression_equations4[7,] <- c("ari_XXX + coleman_liau_XXX + flesch_kincaid_XXX + fog_XXX + smog_XXX",
#                                NA,
#                                "mktadjret_lag1 + mktadjret_lag2 + mktadjret_lag3 + mktadjret_lag1_sq + age_y + management_fee + performance_fee + other_fee + flagship_bin + closed_bin + dead_bin",
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
