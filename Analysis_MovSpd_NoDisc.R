
# required packages
library(dplyr)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(grid) 
library(cowplot)
library(viridis)
library(adehabitatLT)
library(adehabitatHR)
library(stringr)
library(circular)
library(psych)        # circadian.mean(); circadian.sd()
library(multcomp)     # glht()
#library(userfriendlyscience)

river3 = readOGR("C:/Users/Anna/Documents/GitHub/Fremont16/GIS/2004_channel","2004_channel_freTightclip")


## Movement Speed (mean spd per track)
#Using the dataset filtered and discretized by time, we'll calculate the mean movement speed per track
  load("Maestros/AllFish_FiltSec4Bursts.RData")
  red2 = red6
 
   options(digit.secs = 6)
   red2$date = as.POSIXct(red2$date)     


## add metadata
 # SacRiver Stage - pulled from CDEC, same as QAQCed data (according to Paul)
  frestg = read.csv("C:/Users/Anna/Documents/GitHub/Fremont16/Maestros/FRE_stage_2016study.csv")
  frestg = frestg[!is.na(frestg$stage_ft),]
  frestg = frestg[frestg$stage_ft > 5 ,] # some missing values are recorded as 2 or 5; remove these entirely
   frestg$timePST = str_pad(frestg$timePST, 4, pad = "0")
   frestg$datetimePST = as.POSIXct(paste(frestg$datePST, frestg$timePST), format="%Y%m%d %H%M", tz="Etc/GMT-8")
   frestg$datetimeUTC = as.POSIXct(as.character(frestg$datetimePST + (60*60*8)), tz="GMT")
   frestg = frestg[order(frestg$datetimePST),]
   frestg$stageid = rownames(frestg)
  
 ## merge onto position dataset
   red2 = red2[order(red2$date),]
    # magic step! thanks stack exchange!
  red2$cdecStgIndex <- 
     findInterval( red2$date, c(-Inf, head(frestg$datetimeUTC,-1))+ c(0,diff(as.numeric(frestg$datetimeUTC))/2 )) 
  red2 = merge( red2, frestg[,c("stage_ft", "datetimeUTC","stageid")], by.y="stageid", by.x="cdecStgIndex", all.x=T)
  red2 = red2[order(red2$date),]

  
  # Fish Releases
   fishrel = read.csv("C:/Users/Anna/Documents/GitHub/Fremont16/Maestros/TaggingDataRelEv.csv", 
                     colClasses=c("RelTime"="character"))
    fishrel$RelTime = str_pad(fishrel$RelTime, width=4, side="left", pad="0")
    fishrel$RelTime = str_pad(fishrel$RelTime, width=6, side="right", pad="0")
    
    fishrel$datetime = as.POSIXct(paste(fishrel$RelDate, fishrel$RelTime), format="%Y-%m-%d %H%M%S", tz="Etc/GMT-8")
    names(fishrel)[ncol(fishrel)] <- "datetime.Rel"
     fishrel$RelHr = as.POSIXlt(fishrel$datetime.Rel)$hour
        
  red2 = merge(red2, fishrel, by="id", all.x=T)
  red2$cdecStgIndex = NULL
   
   
## Calculate overall path speeds
#Using path length and passage time
   pathspd = summarize(group_by(red2,id),pathlength_m = round(sum(dist, na.rm=T),1), first=min(date), last= max(date), RelHr = unique(RelHr), RelEv=unique(RelEv), mnStg = mean(stage_ft)) #FL = unique(FL), weight = unique(Weight), 
   pathspd = data.frame(pathspd)
    pathspd$passagetime_s = as.numeric(difftime(pathspd$last, pathspd$first, units="sec"))
    pathspd$mps = pathspd$pathlength_m / pathspd$passagetime_s
    
    
## Plot all speeds
  # plot speed for all fish
  # windows(3.5,4); par(mar=c(5,5.5,4,4))
  ggplot(data=pathspd, aes(y=mps, x=1)) + geom_boxplot() + ylab("Ground Speed (mps)") +
     theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
 
  # plot speed by RelHr
  #windows(5,5); par(mar=c(5,5.5,4,4))
  ggplot(data=pathspd, aes(x=factor(RelHr), y=mps)) + 
    geom_boxplot() + xlab("Hour of Release") + ylab("Ground Speed (mps)")
   
  ## plot speed by RelEv
 ggplot(data=pathspd, aes(x=factor(RelEv), y=mps)) + 
    geom_boxplot() + xlab("Release Event") + ylab("Ground Speed (mps)")
 
 ggplot(data=pathspd, aes(x=mps, group=factor(RelEv), fill=factor(RelEv))) + 
   geom_histogram(alpha=0.5, position="identity", binwidth=1, color="black") +
   scale_fill_discrete(name="Release\nEvent")

  ## plot speed by mean river stage
  ggplot(pathspd, aes(y=mps, x=mnStg, colour=factor(RelEv))) + 
    geom_point(pch=16, cex=4) +
    xlab("Mean River Stage (ft)") + ylab(expression("Ground Speed (m s"^"-1)")) + 
    ggtitle("Reach-scale Ground Speed by River Stage") + scale_colour_discrete(name="Release\nEvent")
  

## Statistical effect of release hour
#There is a significant effect of release hour, both then it is considered as a continuous variable and as a factor, but the models and the plots indicate that the effect is very small. We will include the release hour as a mixed effect in future models, but because there is such a huge spread in release hours we won't analyze these groups seperately. 
  mps.RelHr <- lm(mps ~ (RelHr), data=pathspd)  
   summary(mps.RelHr) 
   #plot(mps.RelHr)  # meets assumptions just fine! 
   
   # post-hoc test designed for linear models with factor predictor: multcomp::glht() was recommended
     pathspd$RelHrfac = factor(pathspd$RelHr)
     testmod = lm(mps ~ 0+RelHrfac, data = pathspd)
      summary(testmod)
     posthoc.mod = glht(testmod, linfct = mcp(RelHrfac="Tukey"))
      summary(posthoc.mod)
 
     boxplot(mps ~ RelHrfac, data = pathspd)

     
## Statistical effect of release event
#Release Events 1,2&3 are not significantly different from one another, nor are 4 and 5 significantly different from one another, but the two groups (1,2,3 vs 4,5) are different. This make sense, as 1,2 and 3 were released before over topping, and 4 and 5 were during the overtopping event. 
  mps.RelEv <- lm(mps ~ RelEv, data=pathspd)  
   summary(mps.RelEv) 
   #plot(mps.RelEv)  # meets assumptions pretty well? 
   
   # as an anova test, with tukey posthoc test
  #   testmod = aov(mps ~ 0+RelEvfac, data = pathspd)
  #    summary(testmod)
  #   TukeyHSD(testmod)
   pathspd$RelEvfac = factor(pathspd$RelEv)
   t.test(mps ~ 0+RelEvfac, data = pathspd)
   
   boxplot(mps ~ RelEvfac, data = pathspd, ylab="Mean Track Speed (mps)", xlab="ReleaseEvent")



