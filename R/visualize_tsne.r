#############################################################################
# visualize using tsne
# interactive highcharts
#############################################################################

library(data.table)
library(tsne)
library(Rtsne)
library(rCharts)

setwd("../")
load(file="data/ModelInput_2016_02_08.RData")

programs[,aa_raw:=round(aa_raw*100,4)]
# 420 programs had very low ratings. dropped
sum(programs$aa_raw<0.01)
programs <- programs[aa_raw>=0.01,]
## 84 "generic" movies
sum((regexpr("MOVIE", programs$pgmname) > 0))
sum((programs$pgmtype == "FEATURE FILM") & (regexpr("MOVIE", programs$pgmname) > 0))
programs <- programs[!(regexpr("MOVIE", programs$pgmname) > 0),]
nrow(programs)
## 2836 programs left

setkey(programs, "program_id")
## top viewed programs
programs$dec10 <- equal_freq(programs$aa_raw, 10)

## 17: raw aa
## 18 - 37: average viewing minutes by demo
x <- programs[,18:37, with=FALSE]
## average viewing minutes by person, row normalized
viewmm.n <- x / (programs$totaa/N)
# summary(apply(viewmm.n,1,mean))

## ratings by demo groups
aa <- x/(programs$totduration)*100
aa.n <- aa /(programs$aa_raw)


ppm <- dcast.data.table(vbyperson[program_id %in% programs$program_id,], 
						program_id ~ hhperson_id, value.var="totalminutes", fun=sum)

x <- ppm[,-1, with=FALSE]
x <- x[,runif(ncol(x))<=0.1, with=FALSE]

#### using clusters from a prervious model	
# clusters <- m1$classification
clusters <- round(runif(nrow(x))*50,0)

######### run Rtsne with barnes-hut for quick tests
#####	perplexity: avg size of cluster
rtsne_out <- Rtsne(as.matrix(x), dims=2, perplexity=20)
#### use tsne for final output
####### check convergence 	
set.seed(327)
### change max_iter & min_cost to get better convergence
rtsne_out <- tsne(as.matrix(x), k=2, perplexity=20, max_iter = 10000, min_cost = 1e-3)

## input for highcharts
hc.table <- data.table(rtsne_out)		## rtsne_out$Y for Rtsne, rtsne_out for tsne
hc.table <- data.table(rtsne_out$Y)		## rtsne_out$Y for Rtsne, rtsne_out for tsne
names(hc.table) <-  c("x", "y")
hc.table$z <- subprograms$aa_raw^0.5
hc.table[, group := clusters]
hc.table[, label := paste0("c", clusters, "_", subprograms$netname, "<br />", subprograms$pgmname, "<br />", "d",  subprograms$dec10)]
hc.js <- plot_BubbleHC(hc.table); hc.js

hc.js$save("results/tsne_dec910demoaanorm.html", cdn=FALSE)					### publish with local dependence
# hc.js$publish('rCharts D3 Spatial Packages', host = 'gist')				### publish to GitHub