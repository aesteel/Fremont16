
dat12 = read.csv("C:/Users/Anna/Documents/03_SacBankUSACE/FremontWeir2016/YBUS-DataFromUSGS_Sept2016/Data/Acoustic.Telemetry.Data/VEMCO.2D positioning.data.8-11-16/Analysis Package 1/Positions_RelEv12/AllFish.csv")
dim(dat12)
length(unique(dat12$Id))
 dat12b = dat12[dat12$Id < 65000,]
  dim(dat12b)
  length(unique(dat12b$Id))

dat345= read.csv("C:/Users/Anna/Documents/03_SacBankUSACE/FremontWeir2016/YBUS-DataFromUSGS_Sept2016/Data/Acoustic.Telemetry.Data/VEMCO.2D positioning.data.8-11-16/Analysis Package 3/Positions_RelEv345/AllFish.csv")
dim(dat345)
length(unique(dat345$Id))
 dat345b = dat345[dat345$Id < 65000,]
  dim(dat345b)
  length(unique(dat345b$Id))

allpos = rbind(dat12, dat345)
dim(allpos)
length(unique(allpos$Id))

allpos2 = allpos[allpos$Id < 65000,]
dim(allpos2)
 122971 + 32770
length(unique(allpos2$Id))
 433 + 212
 
## must be one tag that is overlapping, b/c 433+212 = 645, but there are only 644 unique tags in combined df. that's okay. 
 
 
 saveRDS(allpos, "C:/Users/Anna/Documents/GitHub/Fremont16/Maestros/AllPos1to5.RData")
 