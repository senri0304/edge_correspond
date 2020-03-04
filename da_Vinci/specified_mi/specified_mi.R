# List up files
files = list.files('da_Vinci/specified_mi/data',full.names=T)
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

library(ggplot2)
library(dplyr)
library(reshape2)
#library(ggplot2)
cols = colorRamp(c("#0080ff","yellow","#ff8000"))

# x data, y vector y-axis, z ylim, t title
gg <- function(x, y, z, t) {g <- ggplot(x, aes(x=test_eye, y=y, fill=disparity))
g <- g + geom_boxplot(outlier.shape = NA)
g <- g + geom_jitter(size = 0.5, col=x$sn)
g <- g + facet_wrap(~disparity, ncol = 3, scales = "free")
g <- g + ylim(0, z)
g <- g + stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2., fill = "white")
g <- g + ggtitle(paste(t, usi[i]))
plot(g)
}

camp = subset(temp, temp$sub == 'kt')
gg(camp, camp$cdt, 30, 'cdt')

# Plot indivisual data
for (i in 1:n){
  camp = subset(temp, temp$sub == usi[i])
  #  camp <- transform(camp, validity=factor(test_eye, levels = c("valid", "invalid", "local")))
  
  camp = subset(temp, temp$sub == usi[i])
  gg(camp, camp$cdt, 30, 'cdt of monocular image')
}
# Plot mdt
gg(camp, camp$mdt, 5, 'mean of dt')

# Plot latency
gg(camp, camp$latency, 10, 'latency')

# Plot number of transients
gg(camp, camp$transient_counts, 20, 'transient_counts')
}

# Plot indivisual data
for (i in 1:n){
  camp = subset(temp, temp$sub == usi[i])
  camp <- transform(camp, validity=factor(validity, levels = c("valid", "invalid", "local")))
  # Particular disapperance times
  par(mfrow=c(2,2))
  for (j in unique(camp$validity)){
    onset <- subset(camp, validity==j, press_timing)
    onset$press_timing <- gsub('\\[]', '0', onset$press_timing)
    onset$press_timing <- gsub('\\[', '', onset$press_timing)
    onset$press_timing <- gsub(']', '', onset$press_timing)
    
    offset <- subset(camp, validity==j, release_timing)
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
         main=paste('time table; validity ', j), xlab='time', ylab='trials')
    segments(melt_onset$v, melt_onset$k, melt_offset$v, melt_offset$k)
    
    dotchart(melt_offset$v-melt_onset$v, col=rgb(cols(melt_onset$k/50)/255), 
             main=j, xlim = c(0, 30), xlab='disappearance time')
  }
}

# Reshape data for anova
ano = aggregate(x=temp$cdt, by=temp[c('test_eye', "disparity", "sub")], FUN=mean)
library("reshape2")
dc = dcast(ano, sub ~ test_eye + disparity, fun.aggregate=mean, value.var="x")
dc = subset(dc, select = -sub)
ddd = ncol(dc)

temp <- subset(temp, temp$sub!='kn')

library(doBy)
mp2 <- summaryBy(cdt ~ test_eye + disparity + sn, data=temp, FUN=mean)
m2 <- summaryBy(cdt ~ test_eye + disparity, temp, FUN=mean)
g <- ggplot(mp2, aes(y=cdt.mean, x=test_eye, fill=test_eye))
g <- g + stat_summary(fun.y = "mean", geom='bar')+ coord_cartesian(ylim=c(0,30))
#g <- g + geom_bar(stat = "identity") + coord_cartesian(ylim=c(0,30))
g <- g + facet_wrap(~disparity, ncol = 3, scales = "free")
g <- g + geom_point(position=position_jitterdodge(jitter.width = 0.7, jitter.height = 0, dodge.width = .9), colour=mp2$sn)
plot(g)


m <- summaryBy(cdt ~ disparity + sub, data=temp, FUN=mean)
g <- ggplot(m, aes(x=disparity, y=cdt.mean, color=sub, group = sub)) +
  geom_point() +
  geom_line()
plot(g)

# paired t.test
t <- t.test(m[m$disparity!='uncross', ]$cdt.mean, m[m$disparity!='cross', ]$cdt.mean, paired=T)

# diff
diff <- pm
diff$inhibit <- m$cdt.mean - pm$cdt.mean
mutual_inhibit <- summaryBy(inhibit ~ validity + stim_cnd + disparity + sub, data=diff, FUN=c(length, mean, sd))
mutual_inhibit$se <- mutual_inhibit$inhibit.sd / sqrt(mutual_inhibit$inhibit.length)

gg(mutual_inhibit, mutual_inhibit$inhibit.mean, 15, 'mutual_inhibition')

# Anovakun
source("anovakun_483.txt") # encoding = "CP932"
anovakun(dc,"sAB", 2, 2, peta=T, cm = T, holm=T)

