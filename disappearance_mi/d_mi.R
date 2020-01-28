# List up files
files = list.files('disappearance_mi/data',full.names=T)
f = length(files)

si = gsub(".*(..)DATE.*","\\1", files)
n = length(table(si))
usi = unique(si)

# Load data and store
temp = read.csv(files[1], stringsAsFactors = F)
temp$sub = si[1]
temp$sn <- 1 
for (i in 2:f) {
  d = read.csv(files[[i]], stringsAsFactors = F)
  d$sub = si[i]
  d$sn <- i
  temp = rbind(temp, d)
}


library(dplyr)
library(rethinking)
cols = colorRamp(c("#0080ff","yellow","#ff8000"))

# Plot indivisual data
par(mfrow=c(2,2))
for (i in 1:n){
  camp = subset(temp, temp$sub == usi[i])
  # Plot means and trial data
  plot(x=camp$width, y=camp$cdt, xlim=c(0,20), ylim=c(0,30), type="p", 
       xlab="width ratio", ylab="cdt(sec)",
       main=toupper(usi[i]), col='blue')
  par(new=T)
  plot(aggregate(x=camp$cdt, by=camp["width"], FUN=mean), 
       type="l", col="blue", xlim=c(0,20), ylim=c(0,30), xlab="", ylab="")
  par(new=F)
  
  # Plot mdt
  plot(x=camp$width, y=camp$mdt, xlim=c(0,20), ylim=c(0,10), type="p", 
       xlab="width ratio", ylab="mdt(sec)",
       main=toupper(usi[i]), col='blue')
  par(new=T)
  plot(aggregate(x=camp$mdt, by=camp["width"], FUN=mean), 
       type="l", col="blue", xlim=c(0,20), ylim=c(0,10), xlab="", ylab="")
  par(new=F)
  
  # Plot latency
  plot(x=camp$width, y=camp$latency, xlim=c(0,20), ylim=c(0,20), type="p", 
       xlab="width ratio", ylab="latency(sec)",
       main=toupper(usi[i]), col='blue')
  par(new=T)
  plot(aggregate(x=camp$latency, by=camp["width"], FUN=mean), 
       type="l", col="blue", xlim=c(0,20), ylim=c(0,20), xlab="", ylab="")
  par(new=F)
  
  # Plot number of transients
  plot(x=camp$width, y=camp$transient_counts, xlim=c(0,20), ylim=c(0,30), type="p", 
       xlab="width ratio", ylab="transient_counts",
       main=toupper(usi[i]), col='blue')
  par(new=T)
  plot(aggregate(x=camp$transient_counts, by=camp["width"], FUN=mean), 
       type="l", col="blue", xlim=c(0,20), ylim=c(0,30), xlab="", ylab="")
  par(new=F)


# Particular disapperance times
  par(mfrow=c(2,2))
  for (j in unique(camp$width)){
    onset <- subset(camp, width==j, press_timing)
    onset$press_timing <- gsub('\\[]', '0', onset$press_timing)
    onset$press_timing <- gsub('\\[', '', onset$press_timing)
    onset$press_timing <- gsub(']', '', onset$press_timing)

    offset <- subset(camp, width==j, release_timing)
    offset$release_timing <- gsub('\\[]', '0', offset$release_timing)
    offset$release_timing <- gsub('\\[', '', offset$release_timing)
    offset$release_timing <- gsub(']', '', offset$release_timing)

    melt_onset <- strsplit(onset$press_timing, ' ') %>% 
      reshape2::melt() %>% select(2,1) %>% setNames(c("k","v"))
    melt_onset$v <- as.numeric(as.character(melt_onset$v))
    melt_onset <- na.omit(melt_onset)
  
    melt_offset <- strsplit(offset$release_timing, ' ') %>% 
      reshape2::melt() %>% select(2,1) %>% setNames(c("k","v"))
    melt_offset$v <- as.numeric(as.character(melt_offset$v))
    melt_offset <- na.omit(melt_offset)
  
    plot(0, type='n', xlim=c(0, 30), ylim=c(1, 5),
         main=paste('time table; width ', j), xlab='time', ylab='trials')
    segments(melt_onset$v, melt_onset$k, melt_offset$v, melt_offset$k)

    dotchart(melt_offset$v-melt_onset$v, col=rgb(cols(melt_onset$k/50)/255), 
             main=j, xlim = c(0, 30), xlab='disappearance time')
  }
}

# Reshape data for anova
ano = aggregate(x=temp$cdt, by=temp[c("width","sub")], FUN=mean)
library("reshape2")
dc = dcast(ano, sub ~ width, fun.aggregate=mean, value.var="x")
dc = subset(dc, select = -sub)
ddd = ncol(dc)

# Anovakun
source("anovakun_483.txt") # encoding = "CP932"
anovakun(dc,"sA", 3, peta=T, cm = T, holm=T)

