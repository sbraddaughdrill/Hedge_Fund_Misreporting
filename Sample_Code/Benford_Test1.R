library("plyr","reshape2")

#####################################################################
### ADAPTED FROM http://www.math.utah.edu/~treiberg/M3074BenfordEg.pdf
#####################################################################

state.x77 <- datasets::state.x77

############## PICK OFF FIRST SIGNIFICANT DIGIT ######################
# The function as.character() converts number to string
# The function substr(x,a,b) extracts characters a-b from string x
substr(as.character(345678),1,1)

substr(as.character(state.area),1,1)


# Apply to all state data. The function table(x) tabulates number of
# occurences of each entry in x. Note zeros appearing from illiteracy
#rates.
table(substr(as.character(state.x77),1,1))


# Multiply by 10 to clear leading zeros.
table(substr(as.character(10*state.x77),1,1))

table(substr(as.character(100*state.x77),1,1))

# One zero remains. it comes from the number of frost days in Hawaii.
# Convert the zero to NA and tabulate again.
 dat <- state.x77
dat["Hawaii","Frost"]<- NA
ta <- table(substr(as.character(100*dat),1,1))
ta


dat_df0 <- as.data.frame.matrix(dat)
dat_df <- data.frame(State=NA,dat_df0,stringsAsFactors=FALSE)
dat_df[,"State"] <- row.names(dat_df)
row.names(dat_df) <- seq(nrow(dat_df))

ta_df0 <- melt(dat_df, id.vars = "State")
ta_df1 <- data.frame(ta_df0,first_digit=NA,stringsAsFactors=FALSE)
ta_df1[,"first_digit"] <- substr(as.character(100*ta_df1[,"value"]),1,1)
#ta_df2 <- ta_df1
ta_df2 <- ta_df1[!is.na(ta_df1[,"first_digit"]),]
row.names(ta_df2) <- seq(nrow(ta_df2))

ta_df <- count(ta_df2, "first_digit")

rm(state.x77)
rm(dat)
rm(dat_df0,dat_df)
rm(ta_df0,ta_df1,ta_df2)


################### MAKE A TABLE COMPARING OBSERVED AND THEORETICAL PROP #####
sta <- sum(ta)
# The Benford pmf
pb <- sapply(1:9, function(x) log10(1+1/x)) ; pb
sum(pb)

m <- cbind(ta/sta,pb)
colnames(m)<- c("Observed_Prop", "Theoretical_Prop") ; m

m_df <- data.frame(ta_df,Observed_Prop=NA,Theoretical_Prop = NA,stringsAsFactors=FALSE)
m_df[,"first_digit"] <- as.integer(m_df[,"first_digit"])
m_df[,"Observed_Prop"] <- prop.table(m_df[,"freq"])
m_df[,"Theoretical_Prop"] <- log10(m_df[,"first_digit"]+ 1) - log10(m_df[,"first_digit"] + 0)

rm(ta_df)


############## MAKE SIDE BY SIDE HISTOGTORAM OF OBSERVED VS THEORETICAL #####

#data <- rbind(ta/sta,pb/sum(pb))
data <- rbind(m_df[,c("Observed_Prop")],m_df[,c("Theoretical_Prop")])

data_mat <- as.matrix(data, dimnames=list(c(), seq(1,ncol(data))))

barplot(data_mat, beside = T, col = rainbow(7)[c(2,5)], xlab = "First Digit")
title("Benford’s Law Compared to States Data")
legend(16,.28, legend = c("From States Data", "Theoretical"), fill = rainbow(7)[c(2,5)],bg="white")
# M3074Benford1.pdf
  
rm(data,data_mat)


############ CHI SQ TEST FOR PROPORTION #####################################

#chisq.test(ta,p=pb)
chisq.test(m_df[,"freq"],p=m_df[,"Theoretical_Prop"])

# Small p-value indicates that these digits don’t satisfy Benford’s Law.
# same computation by hand:

#ep <- sta*pb
#chisq <- sum((ta-ep)^2/ep) ; chisq
#nu <- length(ta)-1 ; nu
#pchisq(chisq,nu,lower.tail=F)
#rm(ep,chisq,nu)
rm(ta,sta,pb,m)


ep_df <- sum(m_df[,"freq"]) * m_df[,"Theoretical_Prop"]
chisq_df <- sum((m_df[,"freq"]-ep_df)^2/ep_df)
chisq_df

nu_df <- length(m_df[,"freq"])-1 ; nu_df

pchisq(chisq_df,nu_df,lower.tail=F)

rm(ep_df,chisq_df,nu_df)
rm(m_df)
