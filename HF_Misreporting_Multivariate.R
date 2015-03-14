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

source(file=paste(function_directory,"functions_db.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=F)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=F)


regression_expand <- function(x){
  
  #x <- regression_equations1
  
  require(plyr)
  
  list_index <- 0
  regression_equations_comb1_temp <-  ldply(x,function(x){
    
    require(plyr)
    require(zoo)
    
    #x <- x[[1]]
    #x <- x[[2]]
    
    list_index <<- list_index + 1
    
    regression_expand_temp0 <- expand.grid(as.vector(unlist(x["dep_var"])),as.vector(unlist(x["models"])),
                                           as.vector(unlist(x["model_type"])),as.vector(unlist(x["note"])))
    colnames(regression_expand_temp0) <- c("dep_var","indep_var","model_type","note")
    
    regression_expand_temp0 <- regression_expand_temp0[do.call(order,regression_expand_temp0[c("dep_var")]),]
    row.names(regression_expand_temp0) <- seq(nrow(regression_expand_temp0))
    
    dep_index <- 0
    regression_expand_temp1 <- ddply(.data=regression_expand_temp0,.variables="dep_var",
                                     .fun=function(x){ dep_index <<- dep_index + 1
                                                         data.frame(dep_index=dep_index,x,model_index=seq(1,nrow(x)),stringsAsFactors=F)},
                                     .progress="none",.inform=F,.drop=T,.parallel=F,.paropts=NULL)
    
    rm(dep_index,regression_expand_temp0)
    
    regression_expand_temp1[,c("dep_index")] <- as.integer(regression_expand_temp1[,c("dep_index")])
    regression_expand_temp1[,c("model_index")] <- as.integer(regression_expand_temp1[,c("model_index")])
    
    regression_expand_temp1 <- regression_expand_temp1[do.call(order,regression_expand_temp1[c("dep_var")]),]
    row.names(regression_expand_temp1) <- seq(nrow(regression_expand_temp1))
    
    regression_expand_temp2 <- data.frame(lapply(regression_expand_temp1,as.character),stringsAsFactors=F)
    
    rm(regression_expand_temp1)
    
    time_frame_temp0 <-  x["time_frame"]
    time_frame_temp1 <- do.call(cbind,time_frame_temp0[[1]])
    
    rm(time_frame_temp0)
    
    time_frame_temp2 <- data.frame(time_frame_temp1,stringsAsFactors=F)
    colnames(time_frame_temp2) <- c("beg_years","end_years")
    
    rm(time_frame_temp1)
    
    regression_expand_temp3 <- do.call(rbind,replicate(nrow(time_frame_temp2),coredata(regression_expand_temp2),simplify=F))
    #time_frame_temp3 <- replicate(nrow(regression_expand_temp2),coredata(time_frame_temp2),simplify=F)
    
    time_frame_temp3 <- adply(.data=time_frame_temp2,.margins=1,
                              .fun=function(x,count){ 
                                #x <- time_frame_temp2[1,]
                                #count <- nrow(regression_expand_temp2)
                                do.call(rbind,replicate(count,coredata(x),simplify=F))
                              },count=nrow(regression_expand_temp2),
                              .expand=T,.progress="none",.inform=F,.parallel=F,.paropts=NULL)
    
    rm(regression_expand_temp2,time_frame_temp2)
    
    regression_comb <- data.frame(list_index=list_index,
                                  time_frame_temp3,
                                  regression_expand_temp3,
                                  stringsAsFactors=F)
    
    rm(time_frame_temp3,regression_expand_temp3)
    
    regression_comb[,"indep_var"] <- gsub(pattern="  ",replacement=" ",x=regression_comb[,"indep_var"])
    regression_comb[,"indep_var"] <- gsub(pattern="  ",replacement=" ",x=regression_comb[,"indep_var"])
    regression_comb[,"indep_var"] <- gsub(pattern="  ",replacement=" ",x=regression_comb[,"indep_var"])
    regression_comb[,"indep_var"] <- gsub(pattern="  ",replacement=" ",x=regression_comb[,"indep_var"])
    regression_comb[,"indep_var"] <- gsub("^\\s+|\\s+$","",regression_comb[,"indep_var"])
    
    return(regression_comb)
  })
  rm(list_index)
  
  regression_equations_comb1_unique1 <- unique(regression_equations_comb1_temp[,c("beg_years","end_years")])
  
  regression_equations_comb1_unique2 <- data.frame(date_index=seq(1,nrow(regression_equations_comb1_unique1)),
                                                   regression_equations_comb1_unique1,
                                                   stringsAsFactors=F)
  
  regression_equations_comb1 <- merge(regression_equations_comb1_temp,regression_equations_comb1_unique2,
                                      by.x=c("beg_years","end_years"),by.y=c("beg_years","end_years"),
                                      all.x=T,all.y=F,sort=F,suffixes=c(".x",".y"))
  
  rm(regression_equations_comb1_unique1,regression_equations_comb1_unique2)
  
  #date_index <- 0
  #regression_equations_comb1 <- ddply(.data=regression_equations_comb1_temp,.variables=c("beg_years","end_years"),
  #                                    .fun=function(x){ date_index <<- date_index + 1; data.frame(date_index=date_index,x,stringsAsFactors=F)},
  #                                    .progress="none",.inform=F,.drop=T,.parallel=F,.paropts=NULL)
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
                                           stringsAsFactors=F)
  
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
  
  equations_adjusted <- ldply(.data=read_list,.fun=function(x,equations_df,equations_col,name_col,read_prefix){
    
    #x <- read_list[1]
    
    read_index <<- read_index + 1
    
    equations_df[,equations_col] <- gsub(read_prefix,x,equations_df[,equations_col])
    
    equations_df[,c(name_col)] <- paste(equations_df[,c(name_col)],x,sep="_")
    
    data_temp <- data.frame(read_index=read_index,equations_df,stringsAsFactors=F)
    
  },equations_df=equations_df,equations_col=equations_col,name_col=name_col,read_prefix=read_prefix,
  .progress="none",.inform=F,.parallel=F,.paropts=NULL,.id=NA)
  
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
  
  equations_adjusted <- ldply(.data=sim_list,.fun=function(x,equations_df,equations_col,name_col,sim_prefix){
    
    #x <- sim_list[1]
    
    sim_index <<- sim_index + 1
    
    equations_df[,equations_col] <- gsub(sim_prefix,x,equations_df[,equations_col])
    
    equations_df[,c(name_col)] <- paste(equations_df[,c(name_col)],x,sep="_")
    
    data_temp <- data.frame(sim_index=sim_index,equations_df,stringsAsFactors=F)
    
  },equations_df=equations_df,equations_col=equations_col,name_col=name_col,sim_prefix=sim_prefix,
  .progress="none",.inform=F,.parallel=F,.paropts=NULL,.id=NA)
  
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
  
  
  regression1_correlation1 <- c(na.omit(as.character(unlist(x[,"indep_var"],use.names=F))))
  regression1_correlation2 <- paste(regression1_correlation1,sep="",collapse="+")
  rm(regression1_correlation1)
  
  regression1_correlation3 <- unique(strsplit(gsub("\\+","\\1 ",regression1_correlation2)," ")[[1]])
  rm(regression1_correlation2)
  
  #regression1_correlation4 <- gsub("XXX","ios",regression1_correlation3,ignore.case=T)
  regression1_correlation4a <- gsub_expand(myrepl=list(list(pattern=c('XXX'),replacement=read_abrev)),
                                           regression1_correlation3)
  regression1_correlation4b <- gsub_expand(myrepl=list(list(pattern=c('YYYpct'),replacement=sim_abrev)),
                                           regression1_correlation4a)
  regression1_correlation4 <- unique(regression1_correlation4b)
  rm(regression1_correlation3,regression1_correlation4a,regression1_correlation4b)
  
  regression1_correlation5 <- regression1_correlation4[!(regression1_correlation4=="")]
  rm(regression1_correlation4)
  
  dep_vars_list <- unique(c(x[,"dep_var"]))
  
  dep_additional_vars0 <- llply(.data=dep_vars_list,.fun=function(x,dep_additional_vars_temp){
    #x <- dep_vars_list[1]
    #dep_additional_vars <- sort(unique(c(dep_vars_list,additional_vars)))
    
    dep_additional_vars_temp[grep(x,dep_additional_vars_temp,ignore.case=F,perl=F,value=F,fixed=F,useBytes=F,invert=F)] 
    
  },dep_additional_vars_temp=sort(unique(c(dep_vars_list,additional_vars))),.progress="none",.inform=F,.parallel=F,.paropts=NULL)
  
  dep_additional_vars <-unlist(dep_additional_vars0)
  
  rm(dep_vars_list,dep_additional_vars0)
  
  control_vars_list <- c(read_abrev,sim_abrev)
  
  control_vars0 <- llply(.data=control_vars_list,.fun=function(x,dep_additional_vars_temp){
    #x <- dep_vars[1]
    #dep_additional_vars <- sort(unique(c(dep_vars,additional_vars)))
    
    dep_additional_vars_temp[grep(x,dep_additional_vars_temp,ignore.case=F,perl=F,value=F,fixed=F,useBytes=F,invert=F)] 
    
  },dep_additional_vars_temp=sort(unique(regression1_correlation5)),.progress="none",.inform=F,.parallel=F,.paropts=NULL)
  
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
  
  regression1_correlation8 <- matrix("",ncol=nrow(regression1_correlation7),nrow=nrow(regression1_correlation7),
                                     dimnames=list(rownames(regression1_correlation7),rownames(regression1_correlation7)))
  
  regression1_correlation7 <- data.frame(lapply(regression1_correlation7,as.character),stringsAsFactors=F)
  
  for (i in 1:ncol(regression1_correlation7))
  {
    
    temp_col_name <- colnames(regression1_correlation7)[i]
    regression1_correlation8[,temp_col_name] <- regression1_correlation7[,temp_col_name]
    rm(temp_col_name)
  }
  rm(regression1_correlation7,i)
  
  diag(regression1_correlation8) <- paste(format(1.0,digits=decimals,nsmall=decimals),"***",sep="")
  
  regression1_correlation8 <- data.frame(var=row.names(regression1_correlation8),regression1_correlation8,stringsAsFactors=F)
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
  
  invisible(d_ply(.data=equations_df,.variables=date_index_var,
                  .fun=function(x,data_all,id,output_dir){
                    
                    require(plm)
                    require(plyr)
                    
                    #x <- regression_equations_final2[regression_equations_final2[,"date_index"]==1,]
                    #x <- regression_equations_final2[regression_equations_final2[,"date_index"]==2,]
                    #data_all <- data_all
                    #id <- identifier
                    #output_dir <- output_directory_reg_similarity
                    
                    Start_yr_temp <- as.integer(unique(x["beg_years"]))
                    End_yr_temp <- as.integer(unique(x["end_years"]))
                    
                    cat("\n","START YEAR:",Start_yr_temp,"END YEAR:",End_yr_temp,"\n")
                    
                    data_temp <- data_all
                    data_temp.pd <- pdata.frame(data_all,index=c(id,"yr_month"),drop.index=T,row.names=T)
                    
                    data_temp <- data_temp[(data_temp[,"yr"]>=Start_yr_temp & data_temp[,"yr"]<=End_yr_temp),]
                    row.names(data_temp) <- seq(nrow(data_temp))
                    
                    data_temp.pd <- pdata.frame(data_temp,index=c(id,"yr_month"),drop.index=T,row.names=T)
                    
                    d_ply(.data=x,.variables=c("list_index","dep_index","read_index","sim_index"),
                          .fun=function(y,data_temp.pd,data_temp,id,output_dir){
                            
                            require(plyr)
                            require(texreg)
                            
                            #y <- x[(x[,"list_index"]==1 & x[,"dep_index"]==1 & x[,"read_index"]==1 & x[,"sim_index"]==1),]
                            
                            dep_var_temp <- unique(y[,c("dep_var")])
                            note_temp <- unique(y[,c("note")])
                            
                            model_type_temp <- unique(y[,c("model_type")])
                            
                            #out_file_name <- paste("reg_compare_plm",dep_var_temp,name_short,note_temp,sep="_")
                            out_file_name <- paste("reg_compare_plm",unique(y[,c("outname_short")]),sep="_")
                            
                            note_temp_clean1 <- gsub("_"," ",note_temp,perl=T)
                            note_temp_clean2 <- gsub("(^|[[:space:]])([[:alpha:]])","\\1\\U\\2",note_temp_clean1,perl=T)
                            
                            regressions_temp <- dlply(.data=y,.variables="model_index",
                                                      .fun=function(z,data_temp.pd,data_temp,model_type_temp,id){ 
                                                        
                                                        require(plm)
                                                        
                                                        #l <- 1
                                                        #l <- 2
                                                        #z <- y[l,]
                                                        
                                                        model_count <- as.integer(unique(z[,c("model_index")]))
                                                        
                                                        ind_vars_reg0 <- z[,"indep_var"]
                                                        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
                                                        reg0 <- plm(as.formula(paste(z[,"dep_var"],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type_temp)
                                                        #reg0 <- lm(as.formula(paste(z[,"dep_var"],ind_vars_reg0,sep="~")),data_temp)
                                                        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
                                                        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,id])
                                                        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
                                                        reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,id],data_temp[,"month"])
                                                        #reg0_rse <- mcl(data_temp,reg0,data_temp[,id],data_temp[,"month"])
                                                        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
                                                        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
                                                        
                                                        rm(model_count,ind_vars_reg0)
                                                        
                                                        return(list(reg0,reg0_rse))
                                                        
                                                      },
                                                      data_temp.pd=data_temp.pd,data_temp=data_temp,model_type_temp=model_type_temp,id=id,
                                                      .progress="none",.inform=F,.drop=T,.parallel=F,.paropts=NULL)
                            
                            
                            reg <- sapply(regressions_temp,"[",1)
                            
                            rse <- sapply(regressions_temp,"[[",2)
                            
                            se <- llply(.data=rse,.fun=function(w){w[,4]},
                                        .progress="none",.inform=F,.parallel=F,.paropts=NULL)
                            
                            pval <- llply(.data=rse,.fun=function(w){w[,4]},
                                          .progress="none",.inform=F,.parallel=F,.paropts=NULL)
                            
                            cat("\n")
                            
                            htmlreg(l=reg,
                                    model.names=paste("(",seq(1,nrow(y)),")",sep=""),
                                    override.se=se,
                                    override.pval=pval,
                                    stars=c(0.01,0.05,0.1),digits=3,
                                    caption=paste("Effect Of",note_temp_clean2,"On Hedge Fund Flows – Multivariate",sep=" "),
                                    file=paste(output_dir,out_file_name,".doc",sep=""))
                            
                            rm(dep_var_temp,note_temp,model_type_temp,out_file_name,note_temp_clean1,note_temp_clean2)
                            rm(regressions_temp,reg,rse,se,pval)
                            
                          },
                          data_temp.pd=data_temp.pd,data_temp=data_temp,id=id,output_dir=output_dir,
                          .progress="text",.inform=F,.drop=T,.parallel=F,.paropts=NULL)
                    
                    rm(Start_yr_temp,End_yr_temp,data_temp,data_temp.pd)
                    
                  },
                  data_all=data_all,id=id,output_dir=output_dir,
                  .progress="text",.inform=F,.print=F,.parallel=F,.paropts=NULL))
  
}


###############################################################################
cat("SECTION: LIBRARIES","\n")
###############################################################################

#Load External Packages
# c("compare","cwhmisc","data.table","descr","fastmatch","formatR",
#   "gtools","Hmisc","installr","knitr","leaps","lmtest","markdown","memisc","mitools",
#   "pander","pbapply","PerformanceAnalytics","plm","psych","quantreg","R.oo","R2wd",
#   "reporttools","reshape2","rms","sandwich","sqldf","stargazer","stringr",
#   "texreg","taRifx","UsingR","xtable","zoo")
external_packages <- c("plyr","gdata","MASS")
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

data_all0 <- read.csv(file=paste(output_directory,"data_all",".csv",sep=""),header=T,na.strings="NA",stringsAsFactors=F)


###############################################################################
cat("TRIM DATA","\n")
###############################################################################

#data_all0 <- data_all0[(data_all0[,"yr"]>=beg_year & data_all0[,"yr"]<=end_year),]
data_all0 <- data_all0[,!(colnames(data_all0) %in% c("Fund_Name","Secondary_Investment_Strategy","Strategy","Strat_ID"))]



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
cat("MULTIVARIATE ANALYSIS - VARIABLES","\n")
###############################################################################

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

pattern_cols <- c(pattern_cols_99_any,pattern_cols_99_avg,pattern_cols_95_any,pattern_cols_95_avg,pattern_cols_90_any,pattern_cols_90_avg)

### Revision cols
revision_cols <- c("Revision_DV","Revision_1BP_DV","Revision_10BP_DV","Revision_50BP_DV","Revision_100BP_DV")


### Dep Vars (Text Vars)

multivariate_vars_dep <- c(pattern_cols,revision_cols)

### Continuous Vars

multivariate_vars_continuous_fund <- c("pflow","sdpct_flow","sdpct_flow_lag1",
                                       "Yearly_Ret2","Yearly_Ret2_lag1","Yearly_Ret2_sq","Yearly_Ret2_sq_lag1",
                                       "mktadjret","mktadjret_lag1",
                                       "mktadjret_sq","mktadjret_sq_lag1",
                                       "AUM","AUM_lag1","AUM_log_lag1",
                                       "Fund_Size_USm","age_y","total_fee","Sharpe_Ratio","Sortino_Ratio")

#"pflow_lag1","pflow_lag2","pflow_lag3","pflow_lag4",
#"mktadjret_lag1","mktadjret_lag2","mktadjret_lag3","mktadjret_lag4",
#"mktadjret_sq_lag1","mktadjret_sq_lag2","mktadjret_sq_lag3","mktadjret_sq_lag4",
#"AUM_log_lag1","AUM_log_lag2","AUM_log_lag3","AUM_log_lag4",

multivariate_vars_continuous_text <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios",
                                     "avg_grade_level_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
                                     "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios",
                                     "all_similarity_900pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios")

multivariate_vars_continuous_tone <- c("per_litigious","per_modalstrong","per_modalweak","per_negative","per_positive","per_uncertainty")

multivariate_vars_continuous <- c(multivariate_vars_continuous_fund,multivariate_vars_continuous_text,multivariate_vars_continuous_tone)

multivariate_vars_binary <- c(multivariate_vars_binary_fund,multivariate_vars_binary_tone)

### Binary Vars

multivariate_vars_binary_fund <- c("Listed_on_Exchange_bin","Hurdle_Rate_bin","Domicile_onshore_bin","Leverage_bin","Lockup_bin",
                                 "Flagship_bin","Closed_bin","Dead_bin")

multivariate_vars_binary_tone <- c("litigious_dv","modalstrong_dv","modalweak_dv","negative_dv","positive_dv","uncertainty_dv")

multivariate_vars_binary <- c(multivariate_vars_binary_fund,multivariate_vars_binary_tone)

rm2(pattern_cols_99,pattern_cols_99_any,pattern_cols_99_avg)
rm2(pattern_cols_95,pattern_cols_95_any,pattern_cols_95_avg)
rm2(pattern_cols_90,pattern_cols_90_any,pattern_cols_90_avg)
rm2(pattern_cols_sub,pattern_cols_all)
#rm2(pattern_cols,revision_cols)


###############################################################################
cat("FIND FIRST YEAR FOR EACH FUND","\n")
###############################################################################

data_all_multivariate_lookup0 <- data_all[!is.na(data_all[,strat_col]),]

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


#data_all_multivariate_full_keep_trim <- unique(data_all[,c(identifier,"yr",multivariate_vars_dep,multivariate_vars_continuous)])
data_all_multivariate_full_keep_trim <- unique(data_all[,colnames(data_all)[colnames(data_all) %in% c(identifier,"yr",strat_col,multivariate_vars_dep,multivariate_vars_continuous,multivariate_vars_binary)]])

data_all_multivariate_full1 <- merge(data_all_multivariate_lookup,data_all_multivariate_full_keep_trim,
                                         by.x=c(identifier,"yr"),by.y=c(identifier,"yr"),
                                         all.x=T,all.y=F,sort=T,suffixes=c(".x",".y"))
data_all_multivariate_full1 <- data_all_multivariate_full1[order(data_all_multivariate_full1[,identifier],data_all_multivariate_full1[,"yr"]),]
row.names(data_all_multivariate_full1) <- seq(nrow(data_all_multivariate_full1))

data_all_multivariate_full2 <- as.data.table(data_all_multivariate_full1)
setkeyv(data_all_multivariate_full2,c(identifier,"yr"))
setorderv(data_all_multivariate_full2,c(identifier,"yr"),c(1,1))
data_all_multivariate_full2[,na_count :=rowSums(is.na(.SD)),.SDcols=colnames(data_all_multivariate_full2)]
setorderv(data_all_multivariate_full2,c(identifier,"yr","na_count"),c(1,1,1))
setkeyv(data_all_multivariate_full2,c(identifier,"yr"))
data_all_multivariate_full2 <- data_all_multivariate_full2[,.SD[c(1)],by=c(identifier,"yr")]
data_all_multivariate_full <- as.data.frame(data_all_multivariate_full2,stringsAsFactors=F)

rm2(data_all_multivariate_full_keep_trim,data_all_multivariate_full1,data_all_multivariate_full2)


###############################################################################
cat("PANEL REGRESSION - VARIABLES","\n")
###############################################################################

pattern_str <- "kink_percent_PCT_ANYAVG_CUTOFF + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF + num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF + num_pairs_percent_PCT_ANYAVG_CUTOFF + per_negative_percent_PCT_ANYAVG_CUTOFF"
quality_str <- "num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF + num_pairs_percent_PCT_ANYAVG_CUTOFF + per_negative_percent_PCT_ANYAVG_CUTOFF"
nonquality_str <- "kink_percent_PCT_ANYAVG_CUTOFF  + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF"
pb_str <- "kink_percent_PCT_ANYAVG_CUTOFF  + indexrsq_percent_PCT_ANYAVG_CUTOFF + ar_1_percent_PCT_ANYAVG_CUTOFF + num_zero_percent_PCT_ANYAVG_CUTOFF + uniform_percent_PCT_ANYAVG_CUTOFF + string_percent_PCT_ANYAVG_CUTOFF"
read_str <- "ARI_ios + Coleman_Liau_ios + Flesch_Kincaid_ios + FOG_ios + SMOG_ios + avg_grade_level_ios"

#controls <- "age_y + AUM_log_lag1 + total_fee + Domicile_onshore_bin + Flagship_bin + Revision_DV"
#controls <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Revision_DV + Sharpe_Ratio + Lockup_bin"
#controls <- "age_y + AUM + total_fee + Domicile_onshore_bin + Flagship_bin + Lockup_bin"
controls <- "age_y + AUM + Domicile_onshore_bin + Flagship_bin + Lockup_bin"


###############################################################################
cat("PANEL REGRESSION - READBILITY","\n")
###############################################################################

output_directory_reg_readability <- paste(output_directory,"reg_readability","\\",sep="")
create_directory(output_directory_reg_readability,remove=1)

data_year_groups1 <- data.frame(matrix(NA,ncol=2,nrow=1,dimnames=list(c(),c("Start_yr","End_yr"))),stringsAsFactors=F)
data_year_groups1[1,] <- c(beg_year,end_year)


#dep_var1 <- c("ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ios")
#dep_var1 <- c("quality_score_trim1_PCT_ANYAVG_CUTOFF")
dep_var1 <- c("quality_score_trim1_90_any_024","quality_score_trim1_90_avg_024","quality_score_trim1_90_any_036","quality_score_trim1_90_avg_036",
              "quality_score_trim1_90_any_048","quality_score_trim1_90_avg_048","quality_score_trim1_90_any_060","quality_score_trim1_90_avg_060")
model_type1 <- "pooling"
note1 <- "readability"

#sim_type1 <- c("050pct","100pct","250pct","500pct","750pct","900pct")
#sim_type1 <- c("050pct","900pct")
sim_type1 <- c("900pct")

#Regression equations
regression_equations1 <- data.frame(grade=NA,similarity=NA,controls=NA,quantile=NA,fixed_effects=NA,full_independent_vars=NA,stringsAsFactors=F)
regression_equations1[1,] <- c("avg_grade_level_XXX",NA,NA,NA,NA,NA)
regression_equations1[2,] <- c("Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",NA,NA,NA,NA,NA)
regression_equations1[3,] <- c("avg_grade_level_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",NA,NA,NA,NA,NA)
regression_equations1[4,] <- c("avg_grade_level_XXX",NA,controls,NA,NA,NA)
regression_equations1[5,] <- c("Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",NA,controls,NA,NA,NA)
regression_equations1[6,] <- c("avg_grade_level_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",NA,controls,NA,NA,NA)
regression_equations1[7,] <- c("avg_grade_level_XXX",NA,controls,NA,"factor(Primary_Investment_Strategy_combcol)",NA)
regression_equations1[8,] <- c("Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",NA,controls,NA,"factor(Primary_Investment_Strategy_combcol)",NA)
regression_equations1[9,] <- c("avg_grade_level_XXX + Primary_Investment_Strategy_combcol_similarity_YYYpct_XXX",NA,controls,NA,"factor(Primary_Investment_Strategy_combcol)",NA)

regression_equations1 <- unknown_to_NA(regression_equations1,unknowns_strings)

#Create Independent Variable Equation
for (i in 1:nrow(regression_equations1))
{
  temp_char_vec <- c(na.omit(as.character(unlist(regression_equations1[i,1:(ncol(regression_equations1)-1)],use.names=F))))
  regression_equations1[i,"full_independent_vars"] <- paste(temp_char_vec,sep="",collapse=" + ") 
}
# for (i in 1:ncol(regression_equations1))
# {
#   regression_equations1[i,] <-  gsub("PCT","90",regression_equations1[i,])
#   regression_equations1[i,] <-  gsub("ANYAVG","any",regression_equations1[i,])
#   regression_equations1[i,] <-  gsub("CUTOFF","024",regression_equations1[i,])
# }


for (k in 1:nrow(data_year_groups1))
{
  # k <- 1
  
  cat("START YEAR:",data_year_groups1[k,1],"END YEAR:",data_year_groups1[k,2],"\n")
  
  #data_temp <- data_all[(data_all[,"yr"]>=data_year_groups1[k,1] & data_all[,"yr"]<=data_year_groups1[k,2]),]
  data_temp <- data_all_multivariate_full[(data_all_multivariate_full[,"yr"]>=data_year_groups1[k,1] & data_all_multivariate_full[,"yr"]<=data_year_groups1[k,2]),]
  data_temp.pd <- pdata.frame(data_temp,index=c(identifier,"yr"),drop.index=T,row.names=T)
  
  for (i in 1:length(dep_var1))  
  {
    # i <- 1
    
    for (j in 1:length(sim_type1))
    {
      # j <- 1
      # j <- 2
      
      out_file_name <- paste("reg_compare_plm",dep_var1[i],data_year_groups1[k,1],data_year_groups1[k,2],note1,sim_type1[j],sep="_")
      
      #models <- rep( list(list()),nrow(regression_equations1) )
      se <- rep(list(list()),nrow(regression_equations1))
      pval <- rep(list(list()),nrow(regression_equations1))
      
      for (l in 1:nrow(regression_equations1))
      {
        #l <- 1
        
        ind_vars_reg0 <- regression_equations1[l,"full_independent_vars"]
        ind_vars_reg0 <- gsub("XXX","ios",ind_vars_reg0,ignore.case=T)
        ind_vars_reg0 <- gsub("YYYpct",sim_type1[j],ind_vars_reg0,ignore.case=T)
        
        reg0 <- lm(as.formula(paste(dep_var1[i],ind_vars_reg0,sep="~")),data_temp)
        reg0_rse <- coeftest(reg0,vcov=function(x) vcovHC(x,cluster="group",type="HC1"))
        #reg0_rse <- coeftest(reg0,vcov=function(x) vcovDC(x,type="HC1"))
        
        #reg0 <- plm(as.formula(paste(dep_var1[i],ind_vars_reg0,sep="~")),data=data_temp.pd,model=model_type1)
        #reg0_rse <- cl.plm(data_temp,reg0,data_temp[,identifier])
        #reg0_rse <- mcl.plm(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        #reg0_rse <- mcl(data_temp,reg0,data_temp[,identifier],data_temp[,"yr"])
        
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,2]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        #screenreg(list(reg0),digits=3,model.names=c("(1)"),override.se=list(reg0_rse[,4]),override.pval=list(reg0_rse[,4]),stars=c(0.01,0.05,0.1))
        
        #models[[l]] <- reg0
        se[[l]] <- reg0_rse[,4]
        pval[[l]] <- reg0_rse[,4]
        
        assign(paste("reg",l,sep=""),reg0,envir=.GlobalEnv)
        #assign(paste("reg",l,"_rse",sep=""),reg0_rse,envir=.GlobalEnv)
        rm2(ind_vars_reg0,reg0,reg0_rse)
      }
      
      htmlreg(l=eval(parse(text=paste("list(",paste("reg",seq(1,nrow(regression_equations1)),sep="",collapse=","),")",sep=""))),
              model.names=paste("(",seq(1,nrow(regression_equations1)),")",sep=""),
              override.se=se,override.pval=pval,
              stars=c(0.01,0.05,0.1),digits=3,
              caption="Effect of Readability & Similarity on Hedge Fund Scores – Multivariate",
              file=paste(output_directory_reg_readability,out_file_name,".doc",sep=""))
      unlink(paste(output_directory_reg_readability,out_file_name,".doc",sep=""))

      rm2(se,pval,out_file_name,l)
      eval(parse(text=paste("rm(",paste("reg",seq(1,nrow(regression_equations1)),sep="",collapse=","),")",sep="")))
    }
    rm2(j)
  } 
  rm2(data_temp,data_temp.pd,i)
}
rm2(data_year_groups1,dep_var1,model_type1,note1,sim_type1,temp_char_vec,regression_equations1,k)




###############################################################################
cat("MIXED SELECTION","\n")
###############################################################################

model_var_dep <- "quality_score_trim1_90_any_024"

#model_var_indep <- colnames(data_temp)[!(colnames(data_temp) %in% c(identifier,"yr",strat_col,"na_count"))]
model_var_indep <- colnames(data_temp)[colnames(data_temp) %in% c(multivariate_vars_continuous,multivariate_vars_binary,revision_cols)]

#model_var_indep_trim <- model_var_indep
drop_cols <- c("total_fee","Sharpe_Ratio","Sortino_Ratio",
               "ARI_ios","Coleman_Liau_ios","Flesch_Kincaid_ios","FOG_ios","SMOG_ios","avg_grade_level_ac_ios","avg_grade_level_acf_ios",
               "all_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_050pct_ios","Primary_Investment_Strategy_combcol_similarity_900pct_ios",
               "Revision_DV","Revision_1BP_DV","Revision_10BP_DV","Revision_50BP_DV","Revision_100BP_DV")
model_var_indep_trim <- model_var_indep[!(model_var_indep %in% drop_cols)]

model_2_data <- data_temp[,c(model_var_dep,model_var_indep_trim)]
model_2_data_trim <- model_2_data[complete.cases(model_2_data),]

model_2_forward <- lm(as.formula(paste(model_var_dep,"1",sep="~")), data=model_2_data_trim, na.action=na.omit)
model_2_step_forward <- stepAIC(model_2_forward, direction="forward", scope = paste("~",paste(model_var_indep_trim,sep="",collapse=" + "),sep=""),trace=F)
model_2_summary_forward <- summary(model_2_step_forward)

model_2_backward <- lm(as.formula(paste(model_var_dep,paste(model_var_indep_trim,sep="",collapse=" + "),sep="~")), data=model_2_data_trim, na.action=na.omit)
model_2_step_backward <- stepAIC(model_2_backward, direction="backward", scope = paste("~",paste(model_var_indep_trim,sep="",collapse=" + "),sep=""),trace=F)
model_2_summary_backward <- summary(model_2_step_backward)

model_2_mixed <- lm(as.formula(paste(model_var_dep,"1",sep="~")), data=model_2_data_trim, na.action=na.omit)
model_2_step_mixed <- stepAIC(model_2_mixed, direction="both", scope = paste("~",paste(model_var_indep_trim,sep="",collapse=" + "),sep=""),trace=F)
model_2_summary_mixed <- summary(model_2_step_mixed)





