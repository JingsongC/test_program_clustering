#######################################################
# format viewing data 
# Inputs:   raw data from DBMS
# Outputs:
####### programs: programs attribute, with average minutes by demo group, by program_id
####### persons: person list, with demos, by hhperson_id
####### vbyperson: viewing minutes by person, with demos
#######################################################

library(data.table)

# setwd("../")
# logfile <- paste0("data/log_formatData_", format(Sys.Date(),"%Y_%m_%d"),".log")
outputfile <- paste0("data/ModelInput_", format(Sys.Date(),"%Y_%m_%d"), ".RData")

## log file
# sink(file = logfile, append = FALSE, type = c("output", "message"), split = TRUE)

formatfile <- function(filename)	{
  x <- read.csv(file=paste0("data/", filename),
                sep="\t", stringsAsFactors=FALSE, row.names=NULL)
  x <- data.table(x)
  setnames(x, names(x), tolower(names(x)))
  return(x)
}

hhs <- formatfile("PGMCLUST_HOUSEHOLD_CLASS.tsv.gz")
persons <- formatfile("PGMCLUST_PERSON_CLASS.tsv.gz")
telecasts <- formatfile("PGMCLUST_TELECAST_LIST.tsv.gz")
samples <- formatfile("PGMCLUST_UNIFIED_SAMPLE.tsv.gz")
viewing <- formatfile("PGMCLUST_PROGRAM_VIEWING.tsv.gz")


###### generate programs data set 
####    unique by program_id
####    contains program information inc. network, name, broadcast

### unique program list with name, type, network type and name
### - using last telecasts
setkeyv(telecasts, c("program_id", "telecast_broadcast_date", "telecast_report_start_time"))
tmp <- telecasts[!duplicated(telecasts$program_id, fromLast=TRUE),]
programs <- data.table(program_id=tmp$program_id,
                         pgmname=gsub("^\\s+|\\s+$", "", tmp$program_long_name),
                         pgmtype=tmp$program_standard_type_desc,
                         nettype=tmp$program_distributor_type_desc,
                         netname=tmp$program_distributor_name)
### program summary
### - average starting time
### - average duration
### - day of week
tmp <- setkeyv(telecasts, c("telecast_key", "telecast_broadcast_date", "telecast_report_start_time"))
tmp[, `:=`(telstarttime = as.numeric(substr(telecast_report_start_time,1,2))
           + as.numeric(substr(telecast_report_start_time,4,5))/60,
           teldow = paste0("dow",wday(telecast_broadcast_date)))]
ps1 <- tmp[,list(telcount=.N, 
                 starttime_avg=mean(telstarttime),
                 duration_tot=sum(telecast_report_duration)), by=program_id]
### count of telecasts by day of week
ps2 <- tmp[,list(count=.N), by=c("program_id", "teldow")]
ps2 <- dcast.data.table(ps2, program_id ~ teldow, value.var="count", fun=sum)
pgmsummary <- merge(ps1, ps2, by="program_id")		
setnames(pgmsummary, paste0("dow", (1:7)), paste0("telcountdow", (1:7)))

### programs list
programs <- merge(programs, pgmsummary, by="program_id")
### filter programs
programs <- programs[telcount>=4,]		
cat(paste0("--## 4+ telecast: cut down the number of programs from ", nrow(tmp), " to ", nrow(programs), "\n\n"))
setkey(programs, "program_id")

######  generate viewing by person data
viewing[,hhperson_id:=household_id*1000+person_id]
viewing <- merge(viewing, telecasts[,list(telecast_key, program_id)], by="telecast_key", all.x=TRUE)
viewing <- viewing[program_id %in% programs$program_id,]
### viewing by person 
vbyperson <- viewing[,list(l7view_person=sum(liveplus7_aa_minute)), by=c("program_id", "hhperson_id")]
setkeyv(vbyperson, c("program_id", "hhperson_id"))

### programs data set: total viewing minutes by program
ta <- viewing[,list(l7view_tot=sum(liveplus7_aa_minute)), by="program_id"]
programs <- merge(programs, ta, by="program_id")
## raw ratings (not weighted)
n <- length(unique(viewing$hhperson_id))
programs[, aa_raw:=l7view_tot/n/duration_tot]

### Summary data sets
str(programs)
str(vbyperson)

save(programs, vbyperson, file=outputfile)
# sink()