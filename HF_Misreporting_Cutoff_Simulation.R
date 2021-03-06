# TODO: Add comment
# 
# Author:  Brad
# File:    HF_Misreporting_Cutoff_Simulation.R
# Version: 1.0
# Date:    09.07.2014
# Purpose: This Simulates the Data Quality (S5) cutoffs from Bollen_Pool (2012)
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP","\n")
###############################################################################

# Clear workspace
rm(list=ls(all=TRUE))
rm(list=ls(all.names=TRUE))

# Limit History to not exceed 500 lines
Sys.setenv(R_HISTSIZE=500)

repo <- c("http://cran.us.r-project.org")
options(repos=structure(repo))
options(install.packages.check.source=FALSE)

# String as factors is False -- used for read.csv
options(StringsAsFactors=FALSE)

# Default maxprint option
options(max.print=500)
# options(max.print=99999)

# Memory limit
#memory.limit(size=8183)

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
Location <- 1

if (Location==1) {
  
  #input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  input_directory <- normalizePath("F:/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp4",winslash="\\",mustWork=TRUE)
  #function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)    
  function_directory <- normalizePath("F:/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)  
  
} else if (Location==2) {
  
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)   
  
} else if (Location==3) {
  
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  
} else if (Location==4) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\",mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  
} else if (Location==5) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp4",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  
} else if (Location==6) {
  
  input_directory <- normalizePath("H:/Research/Hedge_Fund_Misreporting/Data",winslash="\\",mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp4",winslash="\\",mustWork=TRUE)
  #function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/F/Dropbox/Research_Methods/R",winslash="\\",mustWork=TRUE)
  
} else {
  
  cat("ERROR ASSIGNING DIRECTORIES","\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS","\n")
###############################################################################

#source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_parse.R",sep="\\"),echo=FALSE)

source(file=paste(function_directory,"functions_finance.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_misreporting_screens.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
cat("SECTION: LIBRARIES","\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","fGarch","LaplacesDemon","metRology","plyr","psych","sn")
invisible(unlist(sapply(external_packages,load_external_packages,repo_str=repo,simplify=FALSE,USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
cat("SECTION: SETUP SIMULATION","\n")
###############################################################################

#analysis_col <- "mktadjret"
#analysis_col <- "Monthly_Ret"
analysis_col <- "Monthly_Ret_org"

percentiles <- c(0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99)

simulation_type <- data.frame(sim_type_id=NA,include=NA,package=NA,command=NA,stringsAsFactors=FALSE)
simulation_type[1,] <- c(1,0,"sn","sn::rst(n=num_obs+1,xi=location,omega=scale,alpha=skewness,nu=df)")
simulation_type[2,] <- c(2,0,"metRology","metRology::rt.scaled(n=num_obs+1,mean=location,sd=scale,ncp=skewness,df=df)")
simulation_type[3,] <- c(3,0,"fGarch","fGarch::rsstd(n=num_obs+1,mean=location,sd=scale,nu=df)")
simulation_type[4,] <- c(4,1,"LaplacesDemon","LaplacesDemon::rst(n=num_obs+1,mu=location,sigma=scale,nu=df)")

# 2 Years - Brad
simulation_inputs1 <- data.frame(sim_id=1,rounding_digit=6,draws=1,simulations=10000,obs_per_sim=24,
                                 annual_mean=0.10,monthly_mean=NA,annual_vol=0.15,monthly_vol=NA,
                                 kurtosis=3,df=NA,skewness=0,stringsAsFactors=FALSE)

# 3 Years - Brad
simulation_inputs2 <- data.frame(sim_id=2,rounding_digit=6,draws=1,simulations=10000,obs_per_sim=36,
                                 annual_mean=0.10,monthly_mean=NA,annual_vol=0.15,monthly_vol=NA,
                                 kurtosis=3,df=NA,skewness=0,stringsAsFactors=FALSE)

# 4 Years - Brad
simulation_inputs3 <- data.frame(sim_id=3,rounding_digit=6,draws=1,simulations=10000,obs_per_sim=48,
                                 annual_mean=0.10,monthly_mean=NA,annual_vol=0.15,monthly_vol=NA,
                                 kurtosis=3,df=NA,skewness=0,stringsAsFactors=FALSE)

# 5 Years - Bollen & Poole (2012)
simulation_inputs4 <- data.frame(sim_id=4,rounding_digit=6,draws=1,simulations=10000,obs_per_sim=60,
                                 annual_mean=0.10,monthly_mean=NA,annual_vol=0.15,monthly_vol=NA,
                                 kurtosis=3,df=NA,skewness=0,stringsAsFactors=FALSE)

# 10 Years - Bollen & Poole (2012)
simulation_inputs5 <- data.frame(sim_id=4,rounding_digit=3,draws=1,simulations=10000,obs_per_sim=120,
                                 annual_mean=0.05,monthly_mean=NA,annual_vol=0.10,monthly_vol=NA,
                                 kurtosis=3,df=NA,skewness=0,stringsAsFactors=FALSE)

simulation_inputs <- rbindlist(l=list(simulation_inputs1,simulation_inputs2,simulation_inputs3,simulation_inputs4,simulation_inputs5))
simulation_inputs <- as.data.frame(simulation_inputs)

simulation_inputs[,"monthly_mean"] <- annualize_ret(simulation_inputs[,"annual_mean"],12,"log compound")
simulation_inputs[,"monthly_vol"] <- annualize_sddev(simulation_inputs[,"annual_vol"],12,"log compound")

simulation_inputs[,"df"] <- (6/simulation_inputs[,"kurtosis"])+4

rm(simulation_inputs1,simulation_inputs2,simulation_inputs3,simulation_inputs4,simulation_inputs5)


###############################################################################
cat("SECTION: RUN SIMULATION","\n")
###############################################################################

output_directory_sim <- paste(output_directory,"\\","cutoff_simulation","\\",sep="")
create_directory(output_directory_sim,remove=1)




#sim_out <- adply(.data=simulation_inputs,.margins=1,.fun=function(x,sim_type,analysis_col,percentiles,output_directory_sim){
a_ply(.data=simulation_inputs,.margins=1,.fun=function(x,sim_type,analysis_col,percentiles,output_directory_sim){
  
  # x <- simulation_inputs[1,]
  # x <- simulation_inputs[2,]
  # x <- simulation_inputs[3,]
  # x <- simulation_inputs[4,]
  # x <- simulation_inputs[5,]
  
  
  # sim_type <- simulation_type
  # analysis_col <- analysis_col
  # percentiles <- percentiles
  # output_directory_sim <- output_directory_sim
  
  cat("Simulation:",unique(x[,"sim_id"]),"\n")
  
  rounding_digit_temp <- x[,"rounding_digit"]
  
  draws <- x[,"draws"]
  simulations <- x[,"simulations"]
  obs_per_sim <- x[,"obs_per_sim"]
  
  output_name <- paste("Cutoff_Simulation","_",sprintf("%03d",obs_per_sim),".csv",sep="")
  
  ###############################################################################
  cat("S5 - GENERATE DATA","\n")
  ###############################################################################
  
  simulation_commands0 <- sim_type[sim_type[,"include"]==1,]
  
  #   simulation_commands <- sapply(data.frame(draw_id=NA,simulation_commands0,stringsAsFactors=FALSE),rep.int,
  #                                 times=ceiling(draws/nrow(simulation_commands0)))
  #   simulation_commands <- as.data.frame(simulation_commands,stringsAsFactors=FALSE)
  
  #   simulation_commands <- adply(.data=data.frame(draw_id=NA,simulation_commands0,stringsAsFactors=FALSE),.margins=1,.fun=function(x,times){
  # 
  #                                  return(rep.int(x,times=times))
  #                                  
  #                                },times=ceiling(draws/nrow(simulation_commands0)),.expand=TRUE)
  #   
  
  simulation_commands <- data.frame(draw_id=NA,
                                    simulation_commands0[rep(seq.int(1,nrow(simulation_commands0)),times=ceiling(draws/nrow(simulation_commands0))),],
                                    num_obs=NA,location=NA,scale=NA,skewness=NA,df=NA,stringsAsFactors=FALSE)
  row.names(simulation_commands) <- seq(nrow(simulation_commands))
  
  simulation_commands[,"draw_id"] <- seq(1,nrow(simulation_commands))
  simulation_commands <- simulation_commands[1:draws,]
  
  rm(simulation_commands0)
  
  
  ### Create Data
  
  #num_obs <- ceiling((obs_per_sim*simulations)/draws)
  #skewness <- x[,"skewness"]
  #df <- x[,"df"]
  
  simulation_commands[,"num_obs"] <- ceiling((obs_per_sim*simulations)/draws)
  simulation_commands[,"skewness"] <- x[,"skewness"]
  simulation_commands[,"df"] <- x[,"df"]
  
  
  #sim_input_return_freq <- "annual"
  sim_input_return_freq <- "monthly"
  
  if (sim_input_return_freq=="annual") {
    
    #cat("Annual","\n")
    
    #location <- x[,"annual_mean"]
    #scale <- x[,"annual_vol"]
    
    simulation_commands[,"location"] <- x[,"annual_mean"]
    simulation_commands[,"scale"] <- x[,"annual_vol"]
    
    data_s5_temp <- adply(.data=simulation_commands,.margins=1,.fun=generate_data,command_col="command",.expand=FALSE,.progress="text")
    
    data_s5_temp[,"other_ret"] <- annualize_ret(data_s5_temp[,"calc_ret"],12,"compound")
    data_s5_temp[,"other_ret_lag"] <- annualize_ret(data_s5_temp[,"calc_ret_lag"],12,"compound")
    
    calc_col <- "annual_ret"
    other_col <- analysis_col
    
  } else if (sim_input_return_freq=="monthly") {
    
    #cat("Monthly","\n")
    
    #location <- x[,"monthly_mean"]
    #scale <- x[,"monthly_vol"]
    
    simulation_commands[,"location"] <- x[,"monthly_mean"]
    simulation_commands[,"scale"] <- x[,"monthly_vol"]  
    
    data_s5_temp <- adply(.data=simulation_commands,.margins=1,.fun=generate_data,command_col="command",.expand=FALSE,.progress="text")
    
    data_s5_temp[,"other_ret"] <- annualize_ret(data_s5_temp[,"calc_ret"],1/12,"compound")
    data_s5_temp[,"other_ret_lag"] <- annualize_ret(data_s5_temp[,"calc_ret_lag"],1/12,"compound")
    
    calc_col <- analysis_col
    other_col <- "annual_ret"
    
  } else {
    
    cat("ERROR","\n")
    
  }
  
  colnames(data_s5_temp)[match("X1",names(data_s5_temp))] <- "draw_id"
  
  colnames(data_s5_temp)[match("calc_ret",names(data_s5_temp))] <- calc_col
  colnames(data_s5_temp)[match("calc_ret_lag",names(data_s5_temp))] <- paste(calc_col,"lag1",sep="_")
  colnames(data_s5_temp)[match("other_ret",names(data_s5_temp))] <- other_col
  colnames(data_s5_temp)[match("other_ret_lag",names(data_s5_temp))] <- paste(other_col,"lag1",sep="_")
  
  data_s5_temp[,"overall_id"] <- 0
  data_s5_temp[,"sim_id"] <- as.vector(sapply(seq(1,ceiling(nrow(data_s5_temp)/obs_per_sim),1),rep.int,times=obs_per_sim))[1:nrow(data_s5_temp)]
  data_s5_temp[,"sim_ob_id"] <- rep.int(seq(1,obs_per_sim,1),ceiling(nrow(data_s5_temp)/obs_per_sim))[1:nrow(data_s5_temp)]
  
  data_trim_id_full_cols <- c("overall_id","draw_id","sim_id","sim_ob_id")
  data_trim_nonid_cols <- sort(colnames(data_s5_temp)[!(colnames(data_s5_temp) %in% data_trim_id_full_cols)])
  data_s5_temp <- data_s5_temp[,c(data_trim_id_full_cols,data_trim_nonid_cols)]
  
  data_s5 <- data_s5_temp[1:(obs_per_sim*simulations),]
  row.names(data_s5) <- seq(nrow(data_s5))
  
  rm(data_s5_temp,sim_input_return_freq,calc_col,other_col)
  
  
  ###############################################################################
  cat("S5 - DATA SUMMARY","\n")
  ###############################################################################
  
  data_s5_stats <- describe2(data_s5[,c("annual_ret",analysis_col)])
  #data_s5_stats <- describeBy2(data_s5[,c("draw_id","annual_ret",analysis_col)],group="draw_id")
  #data_s5_stats <- describeBy2(data_s5[,c("sim_id","annual_ret",analysis_col)],group="sim_id")
  
  assign(paste("simulation_data",obs_per_sim,sep=""),data_s5_stats,envir=.GlobalEnv)
  
  
  ###############################################################################
  cat("S5 - SETUP TESTS","\n")
  ###############################################################################
  
  ### Round
  data_s5[,analysis_col] <- round(data_s5[,analysis_col],digits=rounding_digit_temp)
  data_s5[,paste(analysis_col,"lag1",sep="_")] <- round(data_s5[,paste(analysis_col,"lag1",sep="_")],digits=rounding_digit_temp)
  
  ### Group ID
  
  #identifier <- "overall_id"
  #identifier <- "draw_id"
  identifier <- "sim_id"
  
  
  ###############################################################################
  cat("S5 - (1) NUM_ZERO and (6) PER_NEGATIVE","\n")
  ###############################################################################
  
  data_s5_1_screen <- ddply(.data=data_s5[,c(data_trim_id_full_cols,analysis_col)],.variables=identifier,.fun=function(x,ret_col,prob_type){
    
    # x <- data_s5[data_s5[,identifier]==1,c(data_trim_id_full_cols,analysis_col)]
    # ret_col <- analysis_col
    
    zero_and_neg_screen <- zero_neg_screen_execute(data=x,ret_col=ret_col,prob_type="normal")
    
    return(zero_and_neg_screen)
    
  },ret_col=analysis_col,prob_type="normal",.progress="text",.inform=FALSE,.drop=TRUE)
  
  
  data_s5_1_cutoffs <- data.frame(sim_type_id=data_s5_1_screen[,identifier],
                                  num_zero=data_s5_1_screen[,"sum_zero"],
                                  per_neg1=data_s5_1_screen[,"prob_ind_neg"],
                                  per_neg2=data_s5_1_screen[,"sum_neg"]/data_s5_1_screen[,"sum_total"],
                                  per_neg3=data_s5_1_screen[,"prob_cum_neg"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_1_screen)
  
  
  ###############################################################################
  cat("S5 - (2) PER_REPEATS","\n")
  ###############################################################################
  
  data_s5_2_screen <- ddply(.data=data_s5[,c(data_trim_id_full_cols,analysis_col)],.variables=identifier,.fun=function(x,ret_col){
    
    # x <- data_s5[data_s5[,identifier]==1,c(data_trim_id_full_cols,analysis_col)]
    # ret_col <- analysis_col
    # id_col <- identifier
    
    per_repeat_screen <- per_repeat_screen_execute(data=x,ret_col=ret_col)
    
    return(per_repeat_screen)
    
  },ret_col=analysis_col,.progress="text",.inform=FALSE,.drop=TRUE)
  
  data_s5_2_cutoffs <- data.frame(sim_type_id=data_s5_2_screen[,identifier],
                                  per_repeat=data_s5_2_screen[,"Prop_u_one_minus"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_2_screen)
  
  ###############################################################################
  cat("S5 - (3) STRING","\n")
  ###############################################################################
  
  data_s5_3_screen <- ddply(.data=data_s5[,c(data_trim_id_full_cols,analysis_col)],.variables=identifier,.fun=function(x,ret_col){
    
    # x <- data_s5[data_s5[,identifier]==0,c(data_trim_id_full_cols,analysis_col)]
    # ret_col <- analysis_col
    
    string_screen <- string_screen_execute(data=x,ret_col=ret_col)
    
    return(string_screen)
    
  },ret_col=analysis_col,.progress="text",.inform=FALSE,.drop=TRUE)
  
  data_s5_3_cutoffs <- data.frame(sim_type_id=data_s5_3_screen[,identifier],
                                  string=data_s5_3_screen[,"Max_Length"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_3_screen)
  
  
  ###############################################################################
  cat("S5 - (4) NUM_PAIRS","\n")
  ###############################################################################
  
  data_s5_4_screen <- ddply(.data=data_s5,.variables=identifier,.fun=function(x,ret_col,lag_ret_col,both_ways){
    
    # x <- data_s5[data_s5[,identifier]==0,]
    # ret_col <- analysis_col
    # lag_ret_col <- paste(analysis_col,"lag1",sep="_")
    # both_ways <- TRUE
    
    num_pairs_screen <- num_pairs_screen_execute(data=x,ret_col=ret_col,lag_ret_col=lag_ret_col,both_ways=both_ways)
    
    return(num_pairs_screen)
    
  },ret_col=analysis_col,lag_ret_col=paste(analysis_col,"lag1",sep="_"),both_ways=TRUE,.progress="text",.inform=FALSE,.drop=TRUE)
  
  data_s5_4_cutoffs <- data.frame(sim_type_id=data_s5_4_screen[,identifier],
                                  num_pairs=data_s5_4_screen[,"Max_Pairs_Adj"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_4_screen)
  
  
  ###############################################################################
  cat("S5 - (5) UNIFORM","\n")
  ###############################################################################
  
  data_s5_5_screen <- ddply(.data=data_s5[,c(data_trim_id_full_cols,analysis_col)],.variables=identifier,.fun=function(x,ret_col,id_col,graph,rounding_digit){
    
    # x <- data_s5[data_s5[,identifier]==1,c(data_trim_id_full_cols,analysis_col)]
    # x <- data_s5[data_s5[,identifier]==2,c(data_trim_id_full_cols,analysis_col)]
    # x <- data_s5[data_s5[,identifier]==3,c(data_trim_id_full_cols,analysis_col)]
    # ret_col <- analysis_col
    # id_col <- identifier
    # graph <- FALSE
    # rounding_digit <- rounding_digit_temp
    
    #uniform_out <- uniform_screen_execute(data=x,ret_col=ret_col,graph=graph,rounding_digit=rounding_digit)
    
    #ok <- TRUE
    tryCatch(uniform_out <- uniform_screen_execute(data=x,ret_col=ret_col,graph=graph,rounding_digit=rounding_digit),
             warning=function(v) {cat("\n",unique(x[,id_col]),"\n") ; uniform_out <<- NA})
    #if (!ok) {cat(unique(x[,identifier]),"\n")}
    
    return(uniform_out)
  },ret_col=analysis_col,id_col=identifier,graph=FALSE,rounding_digit=rounding_digit_temp,.progress="text",.inform=FALSE,.drop=TRUE)
  
  data_s5_5_cutoffs <- data.frame(sim_type_id=data_s5_5_screen[,identifier],
                                  uniform=data_s5_5_screen[,"x_squared_uniform"],
                                  stringsAsFactors=FALSE)
  
  #rm(data_s5_5_screen)
  
  ###############################################################################
  cat("S5 - CREATE PERCENTILES","\n")
  ###############################################################################
  
  data_s5_cutoff_percentiles <- data.frame(Flag=NA,matrix(NA,ncol=length(percentiles),nrow=8,dimnames=list(c(),paste("Per",formatC(percentiles,digits=2,format="f",flag="0"),sep="_"))),
                                           stringsAsFactors=FALSE)
  
  data_s5_cutoff_percentiles[1,] <- c("Num_Zero",t(quantile(data_s5_1_cutoffs[,"num_zero"],percentiles)))
  data_s5_cutoff_percentiles[2,] <- c("Per_Repeat",t(quantile(data_s5_2_cutoffs[,"per_repeat"],percentiles)))
  data_s5_cutoff_percentiles[3,] <- c("Uniform",t(quantile(data_s5_5_cutoffs[,"uniform"],percentiles)))
  data_s5_cutoff_percentiles[4,] <- c("String",t(quantile(data_s5_3_cutoffs[,"string"],percentiles)))
  data_s5_cutoff_percentiles[5,] <- c("Num_Pairs",t(quantile(data_s5_4_cutoffs[,"num_pairs"],percentiles)))
  data_s5_cutoff_percentiles[6,] <- c("Per_Neg",t(quantile(data_s5_1_cutoffs[,"per_neg1"],percentiles)))
  data_s5_cutoff_percentiles[7,] <- c("Per_Neg",t(quantile(data_s5_1_cutoffs[,"per_neg2"],percentiles)))
  data_s5_cutoff_percentiles[8,] <- c("Per_Neg",t(quantile(data_s5_1_cutoffs[,"per_neg3"],percentiles)))
  
  
  ###############################################################################
  #cat("OUTPUT FLAGS","\n")
  ###############################################################################
  
  assign(paste("cutoff_simulation",sprintf("%03d",obs_per_sim),sep=""),data_s5_cutoff_percentiles,envir=.GlobalEnv)
  
  write.csv(data_s5_cutoff_percentiles,file=paste(output_directory_sim,output_name,sep="\\"),na="",quote=TRUE,row.names=FALSE)
  
  rm(rounding_digit_temp,draws,simulations,obs_per_sim,output_name)
  #rm(num_obs,location,scale,skewness,df)
  
  #return(x)
  
},sim_type=simulation_type,analysis_col=analysis_col,percentiles=percentiles,output_directory_sim=output_directory_sim,
.expand=TRUE,.progress="none",.inform=FALSE,.parallel=FALSE,.paropts=NULL)

