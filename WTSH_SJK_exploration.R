# Exploratory analyses to see if WTSH appear to track fine-scale SKJ movements on long trips 2015

library(raster)

dat<-read.csv("~/grive/phd/results/GPS_2014_15_trip_fpt.csv", h=T)

ncz<-list.files("~/grive/phd/sourced_data/SEAPODYM/Inna_hires/SKJ_2013_2015")

ncz<-ncz[grep("2015", ncz)] #just 2015

ncz<-ncz[5:17] #just start Feb to End Apr.. test different 7 day sections, could do whole year to test against random sample

for(i in ncz)
{
ras<-raster(paste("~/grive/phd/sourced_data/SEAPODYM/Inna_hires/SKJ_2013_2015/", i, sep=""), varname="skj_adu")
if(which(i==ncz)==1){out<-ras}else{out<-stack(out, ras)}
print(i)
} 
# holy shit thats fast in lubuntu
# read in nc, note using adult SKJ, test others too
names(out)<-substr(ncz, 23,30) # give proper names to stacked rasters

ext<-extract(out, cbind(dat$Longitude, dat$Latitude))

dat<-data.frame(dat, ext)

#quick test of idea

dat$forage_binom<-0
dat[dat$hmm_all_trips<3,]$forage_binom<-1 # new variable; transit=0, forage=1


cor(dat[,29:41]) # ok so super correlated, not surprising

m1<-glm(forage_binom~X20150225, data=dat[with(dat, trip_type=="L"& Month==2),], family=binomial)

