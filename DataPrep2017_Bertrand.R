###############################################
#
#  Sacramento Fish Bank Preference Permutation test
#  6 January 2017 
#  Bertrand Lemasson
#  for Anna Steel & D. SMithY
#  
# --- workspace: Riverwork_DataPrep2.RData
# 
###############################################

# --------------------------------------------
# useful links:
# http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html
# http://www2.stat.duke.edu/~cr173/Sta523_Fa15/spatial_data.html 
# http://personal.colby.edu/personal/m/mgimond/RIntro/03_Reading_and_writing_files.html#export-to-a-rds-file

sessionInfo()
# R version 3.2.3 (2015-12-10)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.12.2 (unknown)

# Libraries
source('../libraries.txt')


# fish data
path2017 = '../../annastuff/spaceuseanalysisfor2016/'

fdata <- read_csv(paste0(path2017,'DataForBertrandv3_Filt3metagrp.csv'))

fdata

# A tibble: 82,061 x 11
# TagID            DateTime  Easting Northing      Lat      Long       HPE   grp RelEv  FLmm stage_ft
# <int>              <time>    <dbl>    <dbl>    <dbl>     <dbl>     <dbl> <int> <int> <int>    <dbl>
#   1  36390 2016-02-22 05:47:00 615489.8  4290936 38.75958 -121.6708 0.3905258     1     1   153    18.38
# 2  36390 2016-02-22 05:47:00 615500.5  4290931 38.75954 -121.6707 0.2807287     1     1   153    18.38
# 3  36390 2016-02-22 05:47:00 615506.2  4290929 38.75951 -121.6706 0.2999061     1     1   153    18.38
# 4  36390 2016-02-22 05:47:00 615507.8  4290929 38.75951 -121.6706 0.3776327     1     1   153    18.38
# 5  36390 2016-02-22 05:47:00 615537.6  4290918 38.75941 -121.6702 0.4359597     1     1   153    18.38
# 6  36390 2016-02-22 05:48:00 615547.1  4290919 38.75942 -121.6701 0.4747570     1     1   153    18.38
# 7  36390 2016-02-22 05:48:00 615562.0  4290912 38.75935 -121.6700 0.3433422     1     1   153    18.38
# 8  36390 2016-02-22 05:48:00 615566.8  4290909 38.75933 -121.6699 0.4324774     1     1   153    18.38
# 9  36390 2016-02-22 05:48:00 615571.0  4290907 38.75931 -121.6699 0.2290471     1     1   153    18.38
# 10 36390 2016-02-22 05:48:00 615573.5  4290906 38.75929 -121.6698 0.1500634     1     1   153    18.38
# ... with 82,051 more rows
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"Merge map data with fish data"
" Both are still in Long/Lat"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# add an index tag for later merging
fdata$i <- 1:nrow(fdata)


# import the clipped river map created last time [a SpatialLinesDataFrame over the river's thalweg that a.steel provided]
river.map.inset <- readRDS("/Users/Nid/Research/2015/Riverwork/R/data/Rdata_files/rivermapinset.rds")

# =================================================
# project fish positions onto a 1D 
# variable (distance to the center of the river)
# dist2Line() needs Long/Lat coordinates
# =================================================

# For each fish position, calculate nearest point on the rb line (distance/bearing to channel center, dbcc)
# make sure you pass ri as the SpatialLinesDataFrame and NOT a regular data frame. Note also that while
# dist2Line takes positions in Lat/Long, the distances are exported in meters.

system.time(d.data <- as.data.frame(dist2Line(fdata[,c(6,5)],river.map.inset)))		
# user  system elapsed 
# 282.167   7.752 289.918 

# --- write/read data to save time later
write_tsv(d.data, "../RDataFiles/d2ChannelCenterLine.tsv",col_names=T)

# extract bearings from lon & lat
b.data <- bearing((d.data[,2:3]),as.matrix(fdata[,c(6,5)]))

write.table(b.data, "../RDataFiles/b2ChannelCenterLine.txt",row.names=F, col.names=T)


# =================================================================
# visualize
# =================================================================

# ---- Merge &Order by tag & time

ftmp <- cbind(fdata,distance = d.data$distance,bearing=b.data)

# order by tag id & date
ftmp2 <- ftmp %>% arrange(DateTime, TagID)


# ----- check 
# par(mfcol=c(1,2))
plot(river.map.inset,col="black",axes=T,ylim=c(38.7589,38.763))

points(ftmp2[ftmp2$bearing < 0,]$Long, ftmp2[ftmp2$bearing < 0,]$Lat,pch=19, cex=0.25,col=rgb(1,0,0,0.05))
points(ftmp2[ftmp2$bearing > 0,]$Long, ftmp2[ftmp2$bearing > 0,]$Lat,pch=19, cex=0.25,col=rgb(0,0,1,0.05))

# ---------------------------------------
# ---- fix bearing breakdown at inflection
# ---------------------------------------
rixy <- data.frame(coordinates(river.map.inset))

inflection <- rixy$x[which(rixy$y==min(rixy$y))]
inflection
#[1] -121.6693

# check
# plot(rixy)
# abline(v=rixy$x[which(rixy$y==min(rixy$y))])


# ----- parse data into up/down stream sections relative to the anomaly
upstream <- ftmp2[ftmp2$Long < inflection,]
downstream <- ftmp2[ftmp2$Lon > inflection,]

# =============================
# upstream fix
# =============================

upstream$bearing <- -1 * upstream$bearing
ftmp3 <- rbind(upstream,downstream)


plot(river.map.inset,col="black",axes=T,ylim=c(38.7589,38.763))
points(ftmp3[ftmp3$bearing < 0,]$Long, ftmp3[ftmp3$bearing < 0,]$Lat,pch=19, cex=0.25,col=rgb(1,0,0,0.05))
points(ftmp3[ftmp3$bearing > 0,]$Long, ftmp3[ftmp3$bearing > 0,]$Lat,pch=19, cex=0.25,col=rgb(0,0,1,0.05))

# =============================
# ----- Fish distance to channel center data
# =============================

fdata <- ftmp3

# pick a side and center distribution on zero
fdata[fdata$bearing < 0,]$distance <- -1 * fdata[fdata$bearing < 0,]$distance

# this look almost identical to last year's distribution.

hist(fdata$distance,freq=FALSE)


# --- store (temporary)
write_tsv(fdata,"../RDataFiles/FishDis2Channel2017data.tsv",col_names=T)
rm(ftmp, ftmp2,ftmp3, upstream, downstream,d.data,b.data)


pdf("../Figs/FishPosition2ThalwegMap2017.pdf", width=4, height=4)
par(mai=c(0.75,0.75,0.75,0.75),cex = 0.5,cex.lab=1)
plot(river.map.inset,col="black",ylim=c(38.7589,38.763),xlab = "Longitude", ylab = "Latitude",axes=T)
points(fdata[fdata$bearing < 0,]$Long, fdata[fdata$bearing < 0,]$Lat,pch=19, cex=0.1,col=rgb(1,0,0,0.05))
points(fdata[fdata$bearing > 0,]$Long, fdata[fdata$bearing > 0,]$Lat,pch=19, cex=0.1,col=rgb(0,0,1,0.05))
dev.off()


# =============================
# ----- Merge Fish & Hydro data
# Not done in 2017 
# =============================


# ---------------------------------------
# add time to all tracks
# ---------------------------------------

fetmp <- fdata %>%
  group_by(TagID) %>%
  #select(i, DateTime) %>%
  mutate(i = i,
         dts = round(as.numeric(DateTime-DateTime[1])), 
         step = seq(1,length(dts)),
         # add another time variable to look at the time of day
         tday = hour(DateTime) + minute(DateTime)/60 + second(DateTime)/3600
         
  ) %>%

  # sort by Date & Tag
  arrange(DateTime, TagID)
  
  
# ----- save
write_tsv(fetmp,"../RDataFiles/FishDis2Channel2017data.tsv",col_names=T)

