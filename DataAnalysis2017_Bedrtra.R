###############################################
#
#  Sacramento Fish Bank Preference Permutation test
#  6 January 2017 
#  Bertrand Lemasson
#  for Anna Steel & D. SMithY
#  
# workspace: RiverworkDataAnalysis2016.RData
# 
###############################################

sessionInfo()
# R version 3.2.3 (2015-12-10)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.12.2 (unknown)


# Libraries
source('../libraries.txt')

# fish data

f.data <- read_tsv("../RDataFiles/FishDis2Channel2017data.tsv",col_names=T)

# # A tibble: 82,061 x 17
# TagID            DateTime  Easting Northing      Lat      Long       HPE   grp RelEv  FLmm stage_ft     i   distance    bearing   dts  step     tday
# <int>              <time>    <dbl>    <dbl>    <dbl>     <dbl>     <dbl> <int> <int> <int>    <dbl> <int>      <dbl>      <dbl> <int> <int>    <dbl>
#   1  36390 2016-02-22 05:47:00 615489.8  4290936 38.75958 -121.6708 0.3905258     1     1   153    18.38     1 -11.132090 -16.590588     0     1 5.783333
# 2  36390 2016-02-22 05:47:00 615500.5  4290931 38.75954 -121.6707 0.2807287     1     1   153    18.38     2  -9.729902 -17.878790     0     2 5.783333
# 3  36390 2016-02-22 05:47:00 615506.2  4290929 38.75951 -121.6706 0.2999061     1     1   153    18.38     3  -9.165141 -19.152388     0     3 5.783333


# how many fish ?
length(unique(f.data$TagID))
# [1] 430

# how many fish by run? No run variable. is this grp? - No
tab <- table(f.data[,c(1,8)])
head(tab, 3)

# well, whatever grp is there is little difference
nrow(tab[tab[,1] > 0,])
[1] 214
nrow(tab[tab[,2] > 0,])
[1] 216


# gp <- ggplot(data = f.data, aes(x = distance))
# gp + geom_histogram(data = subset(f.data,grp == 1),fill="blue",alpha=0.25)
#    + geom_histogram(data = subset(f.data,grp == 2),fill="green",alpha=0.25)
# 
# # plot of distribution of groups (Runs?) - No, release groups
# D2CxGrp <- 
#   ggplot() + 
#   geom_histogram(aes(x=distance, y = (..count..)/sum(..count..), fill="b"), colour="black", alpha=.4, data=subset(f.data,grp == 1), stat = "bin") +
#   geom_histogram(aes(x=distance, y = (..count..)/sum(..count..), fill="g"), colour="black", alpha=.4, data=subset(f.data,grp == 2), stat = "bin") +
#   
#   scale_fill_manual(name="Grp", values=c("b"="blue","g" = "green"), labels=c( "b"="1","g"="2")) +
#   ggtitle("D2C across grps") +
#   ylab("Frequency") +
#   theme(plot.title = element_text(lineheight=.8, face="bold"),legend.key = element_rect(colour = "black"))
# 
# ggsave("../Figs/D2CxGrp.jpeg",D2CxGrp, width = 5, height=4, units="in",dpi = 150)

# summary of track length by fish (step)
summary(tapply(f.data$step, f.data$TagID,max))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.0   123.2   165.0   190.8   226.0   578.0 

# summary of track duration by fish (minutes)
summary(tapply(f.data$dts, f.data$TagID,max))/60
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 5.00000 11.00000 12.00000 12.90167 14.00000 73.00000

# --------------------------------------
# So, most fish tracks are based on ~ 225 relocations and
# stick around for < 14 min. Pretty similar to last time

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
" Bank bias (measured as distance to river channel centerline)"
# First stab, no regards to issues of pseudo-replication. Just
# taking a peek. Result: modestly skewed. not a strong signal.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


pdf("../Figs/Histogram_D2C_fulldata.pdf",width=4,height=4)
par(mai=c(1,1,0.5,0.5), bg = 'transparent',las=1, cex.axis=0.8,cex.lab = 1.2)
hist(f.data$distance, freq=FALSE,ylim=c(0,0.04),col="grey",xlab="D2C (m)",main="")
dev.off()

summary(f.data$distance)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -42.600  -3.594   6.221   4.536  13.460  39.630 


# sanity check. compare to last year's data
f.data2015 <- read_csv("~/Research/2015/Riverwork/R/data/Rdata_files/FishEnv_data.csv")

ggplot() +
  geom_histogram(aes(x=distance, y = (..count..)/sum(..count..), fill="r"), colour="red", alpha=.4, data=f.data2015, stat = "bin") +
  geom_histogram(aes(x=distance, y = (..count..)/sum(..count..), fill="b"), colour="blue", alpha=.4, data=f.data, stat = "bin") +
  
  scale_fill_manual(name="Year", values=c("r" = "red", "b"="blue"), labels=c("b"="2015", "r"="2016")) +
  ggtitle("D2C across years") +
  ylab("Frequency") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
ggsave('../Figs/D2CHistos2015and2016.jpeg',width=4,height=3)

 # skew of raw 2016 data
s1 <- skew_G1(f.data$distance, G1 = F) 
#[1] -0.4576898
# slight to moderate skew [less than last year]

# --- quick check on 2015 skew
skew_G1(f.data2015$distance, G1 = F)
# [1] -0.6973693

# ---- NOTE - the final skew and t-value reported are from the bootstrap of the
# filtered data (see lines 421-433)

# ----------------------------------
# while the 2015 data were modestly skewed towards the outer bank,
# the 2016 date are pretty symmetric around a median value that is closer to the outside of the bend.

# cursory test for symmetry 
df <- f.data2015$distance - median(f.data2015$distance)

hist(df)
abline(v=median(f.data2015$distance),col="blue")
wilcox.test(df, alternative = 'two.sided', mu = median(f.data2015$distance))
# reject's a null hypothesis of symmetry


# no real need for population correction 
# moderately skewed (source: Bulmer, M. G. 1979. Principles of Statistics. Dover.)
# rules of thumb: http://www.tc3.edu/instruct/sbrown/stat/shape.htm#SkewnessInterpret

# ----------- significance of skew (without accounting for pseudo-replication)
# Crawley, M.J. 2007. The R book;
# similar summary: http://brownmath.com/stat/shape.htm

# standard error of skew [based on: ]
# sqrt((6*n*(n-1))/((n-2)*(n+1)*n+3))
# well approximated by sqrt(6/n) Crawley, 2007
nn <- length(f.data$distance)
# sqrt((6*nn*(nn-1))/((nn-2)*(nn+1)*nn+3))
# [1] 0.008550809

# use skew as a t value (it's a standardized moment)
t1 <- s1/sqrt(6/nn)

# [1] -53.52591 # Huge. 

# P-value (useful: http://www.cyclismo.org/tutorial/R/pValues.html)
P1 = 2 * pt(-abs(t1), df = nn-1)

# 0 P value of 0 is usually a bad sign. way small. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Temporal patterns & autocorrelations

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# mean D2C x travel time

fe_dXtime <- f.data %>%
  select(step,distance) %>%
  group_by(step) %>%
  summarize(w = length(distance),d2c.m = mean(distance), sd = sd(distance), se=sd(distance)/sqrt(n()))

gp <- ggplot(fe_dXtime, aes(x = d2c.m, y = step, colour = w))

p.D2Cxtime <- gp +  geom_point(size = 0.5) + 
                    geom_vline(xintercept=0, col="darkred") +
                    xlim(-5,25) + ylab("Time steps (unitless)") + xlab("Distance to channel center (D2C, meters)") +
                    ggtitle("") +
                    theme_bw() +
                    theme(axis.title.x = element_text(face="bold",size="10"),
                          axis.title.y = element_text(face="bold",size="10"))
                          
ggsave("../Figs/D2Cxtime2016.jpeg",p.D2Cxtime, width=5, height=4, units="in",dpi=150)
            
# ======================================
# filter each fish track by the lag at which the acf values
# become insignificant. Funtions are below under the 'Functions' section.
# ======================================
                    
system.time(TimeFilteredData <- TemporalFilter(f.data, ci=0.95, plot = T))
                    
                    
# range of lag to initial decorrelation
hist(TimeFilteredData$Lags)
summary(TimeFilteredData$Lags)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   18.00   28.00   34.28   43.00  155.00   

# plot some examples
pdf("../Figs/Temporal_acfsEx2016.pdf",width=9, height=3)

par(mfcol=c(1,4))
hist(TimeFilteredData$Lags,freq = F, main="Temporal lag (d2c)", xlab = "Initial decorrelation point (Lag)",ylim=c(0,0.02))
ex = f.data[f.data$TagID == 39344,]
acf(ex$distance,lag.max=nrow(ex)/2,main = paste0("Tag ID: ",unique(ex$TagID)))
ex = f.data[f.data$TagID == 39365,]
acf(ex$distance,lag.max=nrow(ex)/2,main = paste0("Tag ID: ",unique(ex$TagID)))
ex = f.data[f.data$TagID == 39314,]
acf(ex$distance,lag.max=nrow(ex)/2,main = paste0("Tag ID: ",unique(ex$TagID)))

dev.off()

# extract the filtered data sets from the list

f.data.Ft <- TimeFilteredData$filtered.data

# how much is lost (most)
nrow(f.data.Ft)/nrow(f.data)
# [1] 0.03772803

# -----------------------------------------------------------
# How much overlap is there likely to be in time across 
# individuals? 
# -----------------------------------------------------------

# monthly
plot(f.data.Ft$DateTime,f.data.Ft$distance,pch=19,cex=0.5,col=rgb(0.5,0.5,0.5,0.25))


#daily [pretty clumped ]
febru <- f.data.Ft[month(f.data.Ft$DateTime) == 2,]
march <- f.data.Ft[month(f.data.Ft$DateTime) == 3,]

pdf("../Figs/D2CxDayOfMonth.pdf",width=8, height = 8)

par(mfrow=c(2,2))
plot(day(febru$DateTime),febru$distance,pch=19,cex=0.5,col=rgb(0.5,0.5,0.5,0.25),xlab="Day", ylab="D2C (meters)", main="February, 2016")
plot(day(march$DateTime),march$distance,pch=19,cex=0.5,col=rgb(0.5,0.5,0.5,0.25),xlab="Day", ylab="D2C (meters)", main="March, 2016")

hist(day(febru$DateTime),col="slategrey",xlab="Day", ylab="D2C (meters)", main="February, 2016")
hist(day(march$DateTime),col="slategrey",xlab="Day", ylab="D2C (meters)", main="March, 2016")
dev.off()


# hourly

FebClump <- febru %>% filter(day(DateTime) %in% c(22,23))

MarClump <- march %>% filter(day(DateTime) == 7)

pdf("../Figs/D2CxHourOfDay.pdf",width=8, height = 8)
par(mfrow=c(2,2))
plot(hour(FebClump$DateTime),FebClump$distance,pch=19,cex=0.5,col=rgb(0.5,0.5,0.5,0.25),xlab="Hour", ylab="D2C (meters)", main="February 22-23, 2016")
plot(hour(MarClump$DateTime),MarClump$distance,pch=19,cex=0.5,col=rgb(0.5,0.5,0.5,0.25),xlab="Hour", ylab="D2C (meters)", main="March 7, 2016")

hist(hour(FebClump$DateTime),col="slategrey",xlab="Hour", ylab="D2C (meters)", main="February 22-23, 2016")
hist(hour(MarClump$DateTime),col="slategrey",xlab="Hour", ylab="D2C (meters)", main="March 7, 2016")
dev.off()

# The data are mostly concentrated across 3 days (two in February and 1 in March), but
# during the day the fish relocations seem to be pretty well distributed across the day

# -----------------------------------------------------------
# D2C x Stage
# -----------------------------------------------------------
f.data.Ft$stage.meters = f.data.Ft$stage_ft * 0.3048

# bi-modal; two 'stages at 6 & 8-9 m' 
hist(f.data.Ft$stage.meters)

plot(f.data.Ft$stage.meters, f.data.Ft$distance)


# extract mid-points for breaks
#hist(f.data.Ft$stage.meters,plot = FALSE)

#stage1 <- f.data.Ft %>% filter(stage.meters >= 5.75 && stage.meters <= 6.25)
stage1 <- f.data.Ft %>% filter(stage.meters >= 5.25) %>% filter(stage.meters <= 6.25)
stage2 <- f.data.Ft %>% filter(stage.meters >= 8) %>% filter(stage.meters <= 9)


pdf("../Figs/D2CxStages.pdf",width=8, height = 8)

par(mfrow=c(2,2))
hist(f.data.Ft$stage.meters,col="slategrey",main="")
plot(f.data.Ft$stage.meters, f.data.Ft$distance,pch=19,col=rgb(0.5,0.5,0.5,0.25),ylab="D2C",xlab="Stage (meters)")
plot(stage1$stage.meters, stage1$distance,pch=19,col=rgb(0.5,0.5,0.5,0.25),ylab="D2C",xlab="Stage (meters)", main="Stage 1")
plot(stage2$stage.meters, stage2$distance,pch=19,col=rgb(0.5,0.5,0.5,0.25),ylab="D2C",xlab="Stage (meters)", main="Stage 2")
dev.off()

stage1b <- stage1 %>% filter(stage.meters >= 5.5) %>% filter(stage.meters <= 5.7)
plot(stage1b$stage.meters, stage1b$distance,pch=19,col=rgb(0.5,0.5,0.5,0.25),ylab="D2C",xlab="Stage (meters)", main="Stage 1b")



# -----------------------------------------------------------
# Any change in skew or significance after filtering down the
# data (substantial amount of trimming here - excessive?).
# -----------------------------------------------------------

hist(f.data.Ft$distance)
s2 <- skew_G1(f.data.Ft$distance, G1 = F)
# [1] [1] -0.5098288    # borderline moderate

n2 <- nrow(f.data.Ft)


t2 <- s2/sqrt(6/n2)
# [1]  -11.58108

# 1 - pt(t2, df = n-1)
P2 = 2 * pt(-abs(t2),df = n2-1)
#[1] 2.155059e-30           # Time filtered data are moderately skewed and significant


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
"Spatial patterns & correlations"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# ---- Check how many points do we have per fish?

length(unique(f.data.Ft$TagID)) # just checking
# [1] 430

obs <-tapply(f.data.Ft$step,f.data.Ft$TagID,length)
summary(obs)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.0     5.0     6.0     7.2     9.0    27.0 

# ======================================================
# 1) Spatial structure on all time-filtered data (Ft)
#
# 2) Spatial structure on sub-sample of the 
#   time-filtered data
#
# ======================================================

# ----------------------
# --- Full Ft data
# ----------------------

# quick look at all remaining positions 
gp <- ggplot(data=f.data.Ft, aes(x=Long, y=Lat)) 

gp + geom_point()


gp <- ggplot(data=f.data.Ft, aes(x=Easting, y=Northing)) 

gp + geom_point()



# minimal spatial model exploration [no covariates this time]
lm.sp0 <- lm(distance ~ step, data = f.data.Ft)

datFt<-data.frame(x = f.data.Ft$Easting,y = f.data.Ft$Northing,resids=rstandard(lm.sp0))
coordinates(datFt)<-c('x','y')

pdf("../Figs/BubblePlot_spatial_residuals_Ft.pdf",width=4, height=4)
par(mfcol=c(1,2),cex.lab = 0.8)

bubble(datFt,zcol='resids', maxsize = 1.2,col=c(rgb(0,0,1,0.25),rgb(1,0,0,0.25)),main="Residuals (lm)")
dev.off()

hist(datFt$resids)

# well, some of the larger deviations are certainly on the margins, but I wouldn't call this a strong pattern

# general
var.GmodFt<-variogram(resids~1,data=datFt)

pdf("../Figs/D2c_variogram_global_Ft.pdf",width=4,height=4)
plot(var.GmodFt,pch=19,col="blue",cex=1.2, cex.lab = 0.8)
dev.off()

# by direction
var.Dmod<-variogram(resids~1,data=datFt,alpha=c(0,45,90,135))
pdf("../Figs/variogram_directional_Ft.pdf",width=4,height=4)
par(cex=0.9)
plot(var.Dmod,pch=19,col="blue") # unsurprisingly spatial correlations are strongly anisotropic
dev.off()

# At this point any spatial correlation in the reduced (data_mre) dataset
# is not biologically relevant by itself, and likely is due to an underlying covariate.

# ----------------------------
# --- Spatial sub-set of 
#     Ft data & repeat above
# ----------------------------

f.data.FSt <- f.data.Ft %>%
  group_by(TagID) %>%
  sample_n(1)

# spatial model exploration
lm.sp0 <- lm(distance ~ step, data = f.data.FSt)

datFSt<-data.frame(x = f.data.FSt$Easting,y = f.data.FSt$Northing,resids=rstandard(lm.sp0))
coordinates(datFSt)<-c('x','y')

pdf("../Figs/bubble_spatial_residuals_FSt.pdf",width=4, height=4)
par(cex.lab = 0.8)
bubble(datFSt,zcol='resids', maxsize = 1.2,col=c(rgb(0,0,1,0.25),rgb(1,0,0,0.25)),main="Residuals (lm)")
dev.off()

# general
var.Gmod_FSt<-variogram(resids~1,data=datFSt)

pdf("../Figs/D2c_variogram_global_FSt.pdf",width=4,height=4)
plot(var.Gmod_FSt,pch=19,col="blue",cex=1.2, cex.lab = 0.8)
dev.off()

# by direction
var.Dmod_FST<-variogram(resids~1,data=datFSt,alpha=c(0,45,90,135))
pdf("../Figs/variogram_directional_FSt.pdf",width=4,height=4)
par(cex=0.9)
plot(var.Dmod_FST,pch=19,col="blue") # unsurprisingly spatial correlations are strongly anisotropic
dev.off()

# Interpretation - subsampling the data down to 1 point per fish doesn't really impact the spatial patterns
# That is, global spatial autocorrelations still decay at the same scale (~ 0.005m)

par(mfcol=c(1,2))
plot(var.GmodFt,pch=19,col="blue",cex=1.2, cex.lab = 0.8)

plot(var.Gmod_FSt,pch=19,col="darkred",cex=1.2, cex.lab = 0.8)


# skew of spatial and temporal filtering

hist(f.data.FSt$distance)
abline(v=median(f.data.FSt$distance),col="blue")

#visual comparison across all three data sets (full, temporally filtered, temporal & spatial)
par(mfrow=c(3,1))
hist(f.data$distance, main = 'Full dataset')
hist(f.data.Ft$distance, main = 'Temporal filter')
hist(f.data.FSt$distance, main = 'Spatial/Temporal filter')

s3 <- skew_G1(f.data.FSt$distance, G1 = F)
# [1] -0.2977558

n3 <- nrow(f.data.FSt)

# P [use normal dist. for large sampel sizes instead of t]

t3 <- s3/sqrt(6/n3)

P3 = 2 * pt(-abs(t3),df = n3-1)
# [1] 0.01207429


# ------------------------------------------
# Bootstrap of fully filtered dataset for
# skew significance
# -----------------------------------------



# --- (FT)
zFt.boot <- z1boot(z = f.data.Ft, N = 1000)

names(zFt.boot)
# [1] "i"    "skew" "t" 

summary(zFt.boot$skew)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-0.6913 -0.4921 -0.4274 -0.4306 -0.3731 -0.1605 

summary(zFt.boot$t)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-5.852  -4.166  -3.618  -3.646  -3.159  -1.359 


hist(zFt.boot$t)
abline(v=-3.646,col="blue")




@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  " FUNCTIONS "
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
  
  # x <- Fxy_dc$distance
  skew_G1 <- function (x, G1 = TRUE) {
    # based on: http://www.amstat.org/publications/jse/v19n2/doane.pdf
    n <- length(x)
    m2 <- sum((x-mean(x))^2)/n  # 2nd momment
    m3 <- sum((x-mean(x))^3)/n  # 3rd momment
    
    # Fisher - Pearson standardized moment coefficient
    if(G1){
      (sqrt(n*(n-1))/(n-2)) * (m3/(m2^(3/2)))
    }else{
      
      m3/(m2^(3/2))
      
    }
  }


                    # =======================================================
                    # dat <- fe.data; ci = 0.95; plot = T; i =1
                    
                    TemporalFilter <- function(dat, ci = 0.95, plot=T){
                      
                      Lag <- numeric()
                      for(i in 1:length(unique(dat$TagID))){
                        
                        ti <- dat[dat$TagID == unique(dat$TagID)[i],]
                        # plot(ti$utm.x, ti$utm.y, pch = 19, col="grey")
                        # acf significance level (as foundin plot.acf [ a masked function, use getAnywhere() to see source])
                        if(plot){
                          x <- acf(ti$distance,lag.max=nrow(ti)/2,main = paste0("Tag ID: ",unique(ti$TagID)))
                        }else{
                          
                          x <- acf(ti$distance,plot=F, lag.max=nrow(ti)/2)
                        } 
                        sig <- qnorm((1 + ci)/2)/sqrt(x$n.used)
                        Lag[i] <- min(which(abs(x$acf) <= abs(sig)))
                      }
                      
                      #	---- filter out correlations for now, but revisit and look into detrending to retain information
                      tmp <- data.frame()
                      for(i in 1:length(unique(dat$TagID))){
                        ti <- dat[dat$TagID == unique(dat$TagID)[i],]
                        
                        tmp <- rbind(tmp,ti[seq(1,nrow(ti),Lag[i]),])
                        
                      }	
                      
                      return(list(filtered.data = tmp, Lags = Lag))
                    }

# -----------------------------------------------
# -----------------------------------------------

# z = f.data.Ft
z = zpois; N = 1000

z1boot <- function(z, N, progress = FALSE)
{
  #require(gstat)
  #var.z1 = list()
  skew= numeric()
  t = numeric()
  
  nn = length(unique(z$TagID))
  
  nn = 400
  for(i in 1: N){
    
    #z1 <- z %>%
    #  group_by(TagID) %>%
    #  sample_n(1)
    
    z1 <- sample(z,nn)
    #lm.z1 <- lm(distance ~ step*depth, data = z1)
    
    #df <-data.frame(x = z1$.x,y = z1$utm.y,resids=rstandard(lm.z1))
    #df <-data.frame(x = z1$.x,y = z1$utm.y,resids=rstandard(lm.z1))
    #coordinates(df)<-c('x','y')
    
    #var.z1[[i]] <- variogram(resids~1,data=df)[,1:3]
    skew[i] = skew_G1(z1, G1 = F)
    t[i] <- skew[i]/sqrt(6/nn)
    
    #skew[i] = skew_G1(z1$distance, G1 = F)
    
    #t[i] <- skew[i]/sqrt(6/n)
    
    if(progress) print(paste("boot replicate = ",i));
  }
  
  # return(list(stats = list(i = i, skew = skew, t = t), variograms = var.z1))
  return(list(i = i, skew = skew, t = t))
}
# -----------------------------------------------

