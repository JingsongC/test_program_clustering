##########################################################################
# Measure % contribution of networ to program viewing
# poisson model, negative binomial
#############################################################################

library(data.table)
library(MASS)		## nb model

setwd("../")
load(file="data/ModelInput_2016_02_08.RData")

######################## 1. filter programs
programs[,aa_raw:=round(aa_raw*100,4)]

# sum(programs$aa_raw<=0.01)/nrow(programs)
# table(programs$netname[programs$aa_raw<=0.01])
# 420 programs had very low ratings. dropped
sum(programs$aa_raw<0.01)
programs <- programs[aa_raw>=0.01,]
## 84 "generic" movies
sum((regexpr("MOVIE", programs$pgmname) > 0))
sum((programs$pgmtype == "FEATURE FILM") & (regexpr("MOVIE", programs$pgmname) > 0))
programs <- programs[!(regexpr("MOVIE", programs$pgmname) > 0),]
nrow(programs)
## 2836 programs left
programs$dec10 <- equal_freq(programs$aa_raw, 10)
setnames(programs, "totduration", "programbroadcast")

vv <- vbyperson[program_id %in% programs$program_id,]
vv <- vv[, list(hhperson_id, program_id, totalminutes)]
setnames(vv, "totalminutes", "programview")

################### test first using one person 
vv1 <- vv[hhperson_id == unique(vv$hhperson_id)[1],]
vv1 <- merge(programs[,list(program_id, programbroadcast, netname)], vv1, by="program_id", all=TRUE)
vv1$programview[is.na(vv1$programview)] <- 0
vv1$netname <- as.factor(vv1$netname)

############# poisson model
g1 <- glm(programview ~ netname, family="poisson", data=vv1)
summary(g1)
## http://www.r-bloggers.com/count-data-and-glms-choosing-among-poisson-negative-binomial-and-zero-inflated-models/
1 - pchisq(summary(g1)$deviance, summary(g1)$df.residual)	### p=0, reject the model

### power transformed (Witten 2012)
vv1$programview_a <- round(vv1$programview^alpha,0)	#### alpha =0.3, as computed by PoissonDistance()
vv1$programview_a <- round(vv1$programview^(1),0)	#### alpha =0.3, as computed by PoissonDistance()
g1.a <- glm(programview_a ~ netname, family="poisson", data=vv1)
1 - pchisq(summary(g1.a)$deviance, summary(g1.a)$df.residual)	### p=0, reject the model

############# NB model
g2 <- glm.nb(programview ~ netname, data=vv1) 	### did not converge

### fix 1: add small value
vv1[,programview2:=programview+0.1]		   
summary(g2 <- glm.nb(programview2 ~ netname, data=vv1))	### not good

#### fix 2: group bottom networks - better
s1 <- vv1[,list(networkview=sum(programview)), by="netname"]
s2 <- s1[networkview>0,]
s2[,networkg:=as.factor(equal_freq(networkview,3))]
s3 <- merge(s2[,list(netname, networkg)], vv1, by="netname", all=TRUE)
s3[,newnetname := ifelse(networkg %in% c(2,3), as.character(netname), "___GROUPED")]

g3 <- glm.nb(programview ~ newnetname, data=s3)
## http://www.r-bloggers.com/count-data-and-glms-choosing-among-poisson-negative-binomial-and-zero-inflated-models/
1 - pchisq(summary(g3)$deviance, summary(g3)$df.residual)	#### p=1 model fits well

#### compare NB with Poisson models
g3b <- glm(programview ~ newnetname, family=poisson, data=s3)
1 - pchisq(summary(g3b)$deviance, summary(g3b)$df.residual)		#### p=0 
############## http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
pchisq(2 * (logLik(g3) - logLik(g3b)), df = 1, lower.tail=FALSE)	## chi-sq test, NB is better


####################### use NB model to estimate the size of "network effect" for 100 people
vv <- vbyperson[program_id %in% programs$program_id,]
N <- 100	#### sampling 100 persons
samplePe <- sample(unique(vv$hhperson_id), N)

sumtable <- NULL
for (i in 1:N)	{

x <- vv[hhperson_id==samplePe[i],]
x <- merge(x[,list(program_id, totalminutes)], programs[,list(program_id, netname, pgmname)], by="program_id", all=TRUE)
x$totalminutes[is.na(x$totalminutes)] <- 0
setnames(x, "totalminutes", "programview")

#### group bottom networks
totalview <- sum(x$programview)
xn <- x[,list(networkview=sum(programview)), by="netname"]
x <- merge(x, xn, by="netname", all=TRUE)
x[,newnetname := ifelse(networkview>=totalview*0.05, as.character(netname), "___GROUPED")]

setkey(x, program_id)
res.nb <- glm.nb(programview ~ newnetname, data=x)
sumtable <- rbind(sumtable, data.frame(i=i, hhperson_id=samplePe[i], network_pct = 1-res.nb$deviance/res.nb$null.deviance))
}

summary(sumtable$network_pct)	### mean: 0.30, median: 0.22

###### use NB model, with offset, to adjust network minutes by person
uniids <- unique(vv$hhperson_id)
newvv <- NULL
ptm <- proc.time()

for (i in 1:length(uniids))	{

x <- vv[hhperson_id==uniids[i],]
x <- merge(x[,list(program_id, totalminutes)], programs[,list(program_id, netname, pgmname, programbroadcast)], by="program_id", all=TRUE)
x$totalminutes[is.na(x$totalminutes)] <- 0
setnames(x, "totalminutes", "programview")

#### group bottom networks
totalview <- sum(x$programview)
xn <- x[,list(networkview=sum(programview)), by="netname"]
x <- merge(x, xn, by="netname", all=TRUE)
x[,newnetname := ifelse(networkview>=totalview*0.05, as.character(netname), "___GROUPED")]

setkey(x, program_id)
res.nb <- glm.nb(programview ~ newnetname+offset(log(programbroadcast)), data=x)
x <- cbind(x, res=res.nb$residuals)

newvv <- rbind(newvv, x[programview>0,])
if (i %% 100 == 0)
	cat("############## Person ", i, " Took ",round((proc.time()-ptm)[3]/60,1)," minutes. \n"); flush.console()
}
}