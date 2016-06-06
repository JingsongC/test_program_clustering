##########################################################################
# cluster progrmas using kmedoids
# Main steps:
# 1. Clean data - filter programs & persons
# 2. Create new variables/features
#   2.a. Adjust viewing minutes by network-ratings
# 3. K-medoids
#   3.a. create canberry distance
#   3.b  Select k
#   3.c. run k-medoids
#   3.d. generate additional diagnostics - cluster, silhouette, etc.
#############################################################################


library(data.table)
library(cluster)	## k-medoids
library(fpc)		## pamk() to choose k using silouette

# setwd("../")
getwd()
source("R/_myFunctions.r")
load(file="data/ModelInput_2016_04_21.RData")

######################## 1. clean data
programs[,aa_raw:=round(aa_raw*100,4)]
## remove programs with low rations < 0.01
sum(programs$aa_raw<0.01)
programs <- programs[aa_raw>=0.01,]
## remove programs for "generic" movies
sum((regexpr("MOVIE", programs$pgmname) > 0))
sum((programs$pgmtype == "FEATURE FILM") & (regexpr("MOVIE", programs$pgmname) > 0))
programs <- programs[!(regexpr("MOVIE", programs$pgmname) > 0),]
## # of remaining programs
nrow(programs)

######################## 2. create new variables
programs$dec10 <- equal_freq(programs$aa_raw, 10)

vv <- vbyperson[program_id %in% programs$program_id,]
vv <- vv[, list(hhperson_id, program_id, l7view_person)]

### broadcast by program, and netowrk, in minutes
broadcast <- programs[, list(program_id, netname, duration_tot)]
setnames(broadcast, "duration_tot", "programbroadcast")
b1 <- broadcast[, list(networkbroadcast=sum(programbroadcast)), by="netname"]
broadcast <- merge(broadcast, b1, by="netname")

### view and aa by program, network, and program|netowrk
vv <- merge(vv, broadcast, by="program_id")
setnames(vv, "l7view_person", "view_pgm")
v1 <- vv[, list(view_network=sum(view_pgm)), by=c("hhperson_id", "netname")]
vv <- merge(vv, v1, by=c("hhperson_id", "netname"))

# FIXEDNETWORKBROADCAST
FNB <- 182*3*60
vv[, `:=`(	view_pgm = pmin(view_pgm, programbroadcast)
           ,	view_network = pmin(view_network, FNB))]
vv[, `:=`(	aa_pgm = view_pgm/programbroadcast
           ,	aa_network = view_network/FNB)]

################ using binomial proportion CI
###### https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Normal_approximation_interval
vv[,z_pgm := (aa_pgm-aa_network)/((aa_network*(1-aa_network)/programbroadcast)^0.5)]
####### choosing the threshold
par(mfrow=(c(1,2)))
hist(vv$z, breaks=100, main="Histogram: differences b/w program and network ratings", xlim=c(-50, 50))
plotz <- plotConcentration(vv$z_pgm, vv$view_pgm)
### threshold for keeping 80% of viewing minutes
plotz[abs(plotz$accu-0.20)<=0.001,][1,]
### threshold for keeping 70% of viewing minutes
plotz[abs(plotz$accu-0.30)<=0.001,][1,]

###### using 70% min threshold
THRESHOLD <- round(plotz$z_threshold[abs(plotz$accu-0.30)<=0.001][1],0); THRESHOLD
# vv[, view_pgm_a3 := ifelse(aa_pgm>(aa_network + THRESHOLD*((aa_network*(1-aa_network)/programbroadcast)^0.5)), view_pgm, 0)]


### not ran below for time constraint
if (FALSE)  {
######################## 3. model: k-medoids
########### 3.1 choose # of clusters
x <- dcast.data.table(vv, program_id ~ hhperson_id, value.var=view_pgm_a3, fun=sum)
x <- data.frame(ds)[,-1]
usepgms <- (rowSums(x)>0)
x <- x[usepgms,]

##### use wss and plot looking for "elbow"
ptm <- proc.time()
wss <- (nrow(x)-1)*sum(apply(x,2,var))
for (i in 2:80) wss[i] <- sum(kmeans(x, centers=i)$withinss)
cat(paste0("Time used: ", round((proc.time()-ptm)[3]/60,1), " minutes\n"))
plot(1:80, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

## iterative process to find best k using fpc/pamk
#### fpc/pamk uses silouette to choose the # of clusters
##### large data set, usepam=FALSE
##### use canberra distance matrix
d.can <- dist(x, method = "canberra")	
testk <- pamk(d.can, krange=c(8:14)*10, criterion="asw", usepam=FALSE)
testk <- pamk(d.can, krange=c(120:140), criterion="asw", usepam=FALSE)
# the optimal number of clusters	> 130
testk$nc		

########### 3.2 run final k-medoids model
k <- 132					### the number of clusters could be based upon multiple factors (do not be rigid)
reslist <- NULL; fitlist <- NULL
set.seed(327)

ptm <- proc.time()
fit <- pam(x=d.can, k=k) 
res <- data.table(programs[usepgms,list(program_id, pgmname, pgmtype, netname, aa_raw, dec10)], cluster=fit$cluster)
checkResult(res)
cat("############## Took ",round((proc.time()-ptm)[3]/60,1)," minutes. \n"); flush.console()
  
save(fit, res, file=paste0("results/ProgramClusteringModels_", format(Sys.Date(),"%Y_%m_%d"), ".m"))


############ output to excel workbook
library(xlsx)
wkname <- paste0("result/ProgramClustering_", format(Sys.Date(),"%Y_%m_%d"), ".xlsx")
ssnames <- c("persons_netadj")

#### merge in silhouette info
sil <- cbind(row_id=as.numeric(dimnames(fit$silinfo$widths)[[1]]), data.table(fit$silinfo$widths))
setkey(sil, row_id)
sil <- sil[, 3:4, with=FALSE]
res <- cbind(res, sil)
#### write cluster result spreadsheet
write.xlsx(res, wkname, sheetName = ssnames, col.names=TRUE, row.names=FALSE, append=TRUE)
### concentration
xt <- as.data.table(table(res$netname, res$cluster))
xt$V2 <- as.numeric(xt$V2)
xt <- dcast.data.table(xt, V1 ~ V2, value.var="N", fun=sum)
nets <- xt$V1; xt <- xt[,-1,with=FALSE]
pct <- apply(xt, 1, function(r) max(r)/sum(r))
cnt <- apply(xt, 1, function(r) sum(r))
write.xlsx(cbind(netname=nets, concentration=round(pct,2), programs=cnt), wkname, sheetName = paste0(ssnames,"_conc"), col.names=TRUE, row.names=FALSE, append=TRUE)
}