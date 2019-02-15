# API Call to MSA Data
library(reshape2)
library(fredr)
library(blsAPI)
library(tidyverse)
library(zoo)
library(xgboost)
library(Matrix)
library(caret)
library(lubridate)

fredr_set_key("f71c3c11e9a38e7361a8f04ca2c6b889")

# Columbus
cbus<- fredr_series_observations(
  series_id = "COLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
cbus$series_id<- "Columbus"

# Cincinnati
cinci<- fredr_series_observations(
  series_id = "CTIAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
cinci$series_id<- "Cincinnati"

# Cleveland
cleve<- fredr_series_observations(
  series_id = "CVLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
cleve$series_id<- "Cleveland"
# New York
ny<- fredr_series_observations(
  series_id = "NYLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
ny$series_id<- "New York"
# LA
la<- fredr_series_observations(
  series_id = "LASAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
la$series_id<- "Los Angeles"
# Chicago
chi<- fredr_series_observations(
  series_id = "CHIAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
chi$series_id<- "Chicago"
# Dallas
dal<- fredr_series_observations(
  series_id = "DFWAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
dal$series_id<- "Dallas"
# Houston
hou<- fredr_series_observations(
  series_id = "HTNAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
hou$series_id<- "Houston"
# Philadelpia
philly<- fredr_series_observations(
  series_id = "PCWAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
philly$series_id<- "Philadelphia"
# DC
dc<- fredr_series_observations(
  series_id = "WAAAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
dc$series_id<- "Washington DC"
# MIA
mia<- fredr_series_observations(
  series_id = "MIMAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
mia$series_id<- "Miami"
# ATL
atl<- fredr_series_observations(
  series_id = "ATLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
atl$series_id<- "Atlanta"
# Boston
bos<- fredr_series_observations(
  series_id = "BSLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
bos$series_id<- "Boston"
# SanFran
sf<- fredr_series_observations(
  series_id = "SFCAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
sf$series_id<- "San Francisco"
# Phoenix
pho<- fredr_series_observations(
  series_id = "PHXAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
pho$series_id<- "Phoenix"
# Riverside
rvr<- fredr_series_observations(
  series_id = "RSBAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
rvr$series_id<- "Riverside"
# Detroit
detroit<- fredr_series_observations(
  series_id = "DWLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
detroit$series_id<- "Detroit"
# Seattle
seattle<- fredr_series_observations(
  series_id = "STWAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
seattle$series_id<- "Seattle"
# Minn
minn<- fredr_series_observations(
  series_id = "MSPAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
minn$series_id<- "Minneapolis"
# SD
sd<- fredr_series_observations(
  series_id = "SDIAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
sd$series_id<- "San Diego"
# Tampa
tampa<- fredr_series_observations(
  series_id = "TMAAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
tampa$series_id<- "Tampa"
# STL
stl<- fredr_series_observations(
  series_id = "STLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
stl$series_id<- "St. Louis"
# Baltimore
bal<- fredr_series_observations(
  series_id = "BTMAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
bal$series_id<- "Baltimore"
# Denver
den<- fredr_series_observations(
  series_id = "DNVAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
den$series_id<- "Denver"
# Charlotte
char<- fredr_series_observations(
  series_id = "CGRAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
char$series_id<- "Charlotte"
# Pittsburgh
pitt<- fredr_series_observations(
  series_id = "PITAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
pitt$series_id<- "Pittsburgh"
# Portland
port<- fredr_series_observations(
  series_id = "PORAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
port$series_id<- "Portland"
# SanAnt
sanant<- fredr_series_observations(
  series_id = "SATAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
sanant$series_id<- "San Antonio"
# Orlando
orl<- fredr_series_observations(
  series_id = "ORLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
orl$series_id<- "Orlando"
# Sacramento
sac<- fredr_series_observations(
  series_id = "SYOAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
sac$series_id<- "Sacramento"
# KC
kc<- fredr_series_observations(
  series_id = "KNCAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
kc$series_id<- "Kansas City"
# LV
lv<- fredr_series_observations(
  series_id = "LSVAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
lv$series_id<- "Las Vegas"
# Indi
indi<- fredr_series_observations(
  series_id = "INDAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
indi$series_id<- "Indianapolis"
# SJ
sj<- fredr_series_observations(
  series_id = "SSCAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
sj$series_id<- "San Jose"
# Austin
aus<- fredr_series_observations(
  series_id = "AUSAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
aus$series_id<- "Austin"
# Nashville
nash<- fredr_series_observations(
  series_id = "NVLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
nash$series_id<- "Nashville"
# Virginia Beach
vb<- fredr_series_observations(
  series_id = "NFKAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
vb$series_id<- "Virginia Beach"
# Providence
prov<- fredr_series_observations(
  series_id = "PPWAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
prov$series_id<- "Providence"
# Milwaukee
milw<- fredr_series_observations(
  series_id = "MWKAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
milw$series_id<- "Milwaukee"
# Jacksonville
jack<- fredr_series_observations(
  series_id = "JAXAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
jack$series_id<- "Jacksonville"
# Memphis
mem<- fredr_series_observations(
  series_id = "MPHAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
mem$series_id<- "Memphis"
# Oklahoma City
okc<- fredr_series_observations(
  series_id = "OKCAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
okc$series_id<- "Oklahoma City"
# Louisville
lvl<- fredr_series_observations(
  series_id = "LOIAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
lvl$series_id<- "Louisville"
# Richmond
rich<- fredr_series_observations(
  series_id = "RCPAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
rich$series_id<- "Richmond"
# New Orleans
newo<- fredr_series_observations(
  series_id = "NORAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
newo$series_id<- "New Orleans"
# Raleigh
ral<- fredr_series_observations(
  series_id = "RCYAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
ral$series_id<- "Raleigh"
# Hartford
hf<- fredr_series_observations(
  series_id = "HNBAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
hf$series_id<- "Hartford"
# Salt Lake City
slc<- fredr_series_observations(
  series_id = "SLCAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
slc$series_id<- "Salt Lake City"
# Birmingham
bir<- fredr_series_observations(
  series_id = "BIRAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
bir$series_id<- "Birmingham"
# Buffalo
buf<- fredr_series_observations(
  series_id = "BUFAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
buf$series_id<- "Buffalo"
# Norwalk
bridge<- fredr_series_observations(
  series_id = "NHRAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
bridge$series_id<- "Norwalk"
# New Haven
nhav<- fredr_series_observations(
  series_id = "NHAAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
nhav$series_id<- "New Haven"
# Worcester
worc<- fredr_series_observations(
  series_id = "WTRAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
worc$series_id<- "Worcester"
# Albany
alba<- fredr_series_observations(
  series_id = "ALBAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
alba$series_id<- "Albany"
# Allentown
allen<- fredr_series_observations(
  series_id = "ALLAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
allen$series_id<- "Allentown"
# Rochester
roch<- fredr_series_observations(
  series_id = "ROHAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
roch$series_id<- "Rochester"
# Grand Rapids
grra<- fredr_series_observations(
  series_id = "GRRAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
grra$series_id<- "Grand Rapids"

# Columbia
cbaa<- fredr_series_observations(
  series_id = "CBAAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
cbaa$series_id<- "Columbia"
# Greensboro
gnsa<- fredr_series_observations(
  series_id = "GNSAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
gnsa$series_id<- "Greensboro"
# Greenville
gnva<- fredr_series_observations(
  series_id = "GNVAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
gnva$series_id<- "Greenville"
# Little Rock
lrsa<- fredr_series_observations(
  series_id = "LRSAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
lrsa$series_id<- "Little Rock"
# Sarasota
sara<- fredr_series_observations(
  series_id = "SARAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
sara$series_id<- "Sarasota"
# Albuquerque
abqa<- fredr_series_observations(
  series_id = "ABQAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
abqa$series_id<- "Albuquerque"
# Tucson
tuca<- fredr_series_observations(
  series_id = "TUCAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
tuca$series_id<- "Tucson"
# Tulsa
tula<- fredr_series_observations(
  series_id = "TULAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
tula$series_id<- "Tulsa"
# Bakersfield
baka<- fredr_series_observations(
  series_id = "BAKAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
baka$series_id<- "Bakersfield"
# Ventura
vena<- fredr_series_observations(
  series_id = "VENAGRIDX",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
vena$series_id<- "Ventura"


all_msa <- as.data.frame(rbind(atl, aus, bal, bir, bos, buf, cbus, char, chi, cinci, cleve, dal, dc,
                    den, detroit, hf, hou, indi, jack, kc, la, lv, lvl, mem, mia, milw,
                    minn, nash, newo, ny, okc, orl, philly, pho, pitt, port, prov, ral, 
                    rich, rvr, sac, sanant, sd, seattle, sf, sj, slc, stl, tampa, vb, bridge,
                    nhav, worc, alba, allen, roch, grra, cbaa, gnsa, gnva, lrsa, sara, abqa, tuca,
                    tula, baka, vena))
names(all_msa)<- c("Date", "MSA", "Coincident")

all_msa <- all_msa %>% 
  mutate(Date= as.character(as.yearmon(Date, format="%Y-%m-%d")))

#################################################################################################
### Recession Determination

recessioning<- function(x) {
  
  y<- all_msa %>% 
    filter(MSA==x)
  x<- y %>% 
    select(Coincident)
  x<- x$Coincident

  
  peaks<- list()
  troughs<- list()

  for(i in 3:(length(y$Date)-6)) {
    peaks[i]<- if(x[i]<0) {
      0} else if(x[i+1]>0) {
        0} else if(sum(x[i],x[i-1],x[i-2])<0) {
          0} else if(sum(x[i+1],x[i+2],x[i+3])>0) {
            0} else if(sum(x[i+4],x[i+5],x[i+6])>0) {
              0} else {1}
    
    troughs[i]<- if(x[i]>0) {
      0} else if(x[i+1]<0) {
        0} else if(sum(x[i],x[i-1],x[i-2])>0) {
          0} else if(sum(x[i+1],x[i+2],x[i+3])<0) {
            0} else if(sum(x[i+4],x[i+5],x[i+6])<0) {
              0} else {1}
  }

  peaks<- as.data.frame(t(as.data.frame(peaks[3:(length(y$Date)-6)])))
  names(peaks)<- "Peaks"
  rownames(peaks)<- NULL
  date<- y$Date[3:(length(y$Date)-6)]

  
  troughs<- as.data.frame(t(as.data.frame(troughs[3:(length(y$Date)-6)])))
  names(troughs)<- "Troughs"
  rownames(troughs)<- NULL
  
  numbers<- cbind(date, peaks, troughs)

  tabling2<- numbers %>% 
    mutate(Peaks=as.numeric(Peaks)) %>% 
    mutate(Troughs=as.numeric(Troughs)) %>% 
    mutate(Index= 1:length(numbers$date))
  
  
  # Condition 4, There Cannot be two troughs in a quarter or two peaks in a quarter
  for(i in 3:length(tabling2$date)) {
    if(tabling2$Troughs[i]==1 & tabling2$Troughs[i-2]==1) {
      tabling2$Troughs[i]<- 0
    }
  }
  for(i in 3:length(tabling2$date)) {
    if(tabling2$Peaks[i]==1 & tabling2$Peaks[i-2]==1) {
      tabling2$Peaks[i]<- 0
    }
  }
  # Condition 5, Choosing between two troughs in between peaks
  peaktablecheck<- tabling2 %>% 
    filter(Peaks==1)
  troughtablecheck<- tabling2 %>% 
    filter(Troughs==1)
  
  # Finding Competing Peaks
  if(peaktablecheck$Index[1]<troughtablecheck$Index[1]) {
    peaktablecheck$ind2[1]<- 0
  }
  for(i in 1:length(troughtablecheck$Trough)) {
    for(j in 1:length(peaktablecheck$Peak)) {
      if(peaktablecheck$Index[j]>troughtablecheck$Index[length(troughtablecheck$Trough)]) {
        peaktablecheck$ind2[j]<- length(troughtablecheck$Trough)
      } else if(peaktablecheck$Index[j]>troughtablecheck$Index[i] & peaktablecheck$Index[j]<troughtablecheck$Index[i+1]) {
        peaktablecheck$ind2[j]<- i
      }}
  }
  
  # Removing competing Peak losers
  for(i in 1:length(unique(peaktablecheck$ind2))) {
    if(is.na(peaktablecheck$ind2[i+1])) {
      print("")
    } else if(peaktablecheck$ind2[i] == peaktablecheck$ind2[i+1]) {
      if(sum(x[peaktablecheck$Index[i]:peaktablecheck$Index[i+1]])<0) {
        peaktablecheck<- peaktablecheck[-(i+1),]
      } else {
        peaktablecheck<- peaktablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(peaktablecheck$ind2))) {
    if(is.na(peaktablecheck$ind2[i+1])) {
      print("")
    } else if(peaktablecheck$ind2[i] == peaktablecheck$ind2[i+1]) {
      if(sum(x[peaktablecheck$Index[i]:peaktablecheck$Index[i+1]])<0) {
        peaktablecheck<- peaktablecheck[-(i+1),]
      } else {
        peaktablecheck<- peaktablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(peaktablecheck$ind2))) {
    if(is.na(peaktablecheck$ind2[i+1])) {
      print("")
    } else if(peaktablecheck$ind2[i] == peaktablecheck$ind2[i+1]) {
      if(sum(x[peaktablecheck$Index[i]:peaktablecheck$Index[i+1]])<0) {
        peaktablecheck<- peaktablecheck[-(i+1),]
      } else {
        peaktablecheck<- peaktablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(peaktablecheck$ind2))) {
    if(is.na(peaktablecheck$ind2[i+1])) {
      print("")
    } else if(peaktablecheck$ind2[i] == peaktablecheck$ind2[i+1]) {
      if(sum(x[peaktablecheck$Index[i]:peaktablecheck$Index[i+1]])<0) {
        peaktablecheck<- peaktablecheck[-(i+1),]
      } else {
        peaktablecheck<- peaktablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(peaktablecheck$ind2))) {
    if(is.na(peaktablecheck$ind2[i+1])) {
      print("")
    } else if(peaktablecheck$ind2[i] == peaktablecheck$ind2[i+1]) {
      if(sum(x[peaktablecheck$Index[i]:peaktablecheck$Index[i+1]])<0) {
        peaktablecheck<- peaktablecheck[-(i+1),]
      } else {
        peaktablecheck<- peaktablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(peaktablecheck$ind2))) {
    if(is.na(peaktablecheck$ind2[i+1])) {
      print("")
    } else if(peaktablecheck$ind2[i] == peaktablecheck$ind2[i+1]) {
      if(sum(x[peaktablecheck$Index[i]:peaktablecheck$Index[i+1]])<0) {
        peaktablecheck<- peaktablecheck[-(i+1),]
      } else {
        peaktablecheck<- peaktablecheck[-(i),]
      }} 
  }
  # This is working to determine competing troughs
  if(troughtablecheck$Index[1]<peaktablecheck$Index[1]) {
    troughtablecheck$ind2[1]<- 0
  }
  for(i in 1:length(peaktablecheck$Peak)) {
    for(j in 1:length(troughtablecheck$Trough)) {
      if(troughtablecheck$Index[j]>peaktablecheck$Index[length(peaktablecheck$Peak)]) {
        troughtablecheck$ind2[j]<- length(peaktablecheck$Peak)
      } else if(troughtablecheck$Index[j]>peaktablecheck$Index[i] & troughtablecheck$Index[j]<peaktablecheck$Index[i+1]) {
        troughtablecheck$ind2[j]<- i
      }}
  }
  
  # Removing competing trough losers, Peak Losers, multiple iterations
  for(i in 1:length(unique(troughtablecheck$ind2))) {
    if(is.na(troughtablecheck$ind2[i+1])) {
      print("")
    } else if(troughtablecheck$ind2[i] == troughtablecheck$ind2[i+1]) {
      if(sum(x[troughtablecheck$Index[i]:troughtablecheck$Index[i+1]])>0) {
        troughtablecheck<- troughtablecheck[-(i+1),]
      } else {
        troughtablecheck<- troughtablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(peaktablecheck$ind2))) {
    if(is.na(peaktablecheck$ind2[i+1])) {
      print("")
    } else if(peaktablecheck$ind2[i] == peaktablecheck$ind2[i+1]) {
      if(sum(x[peaktablecheck$Index[i]:peaktablecheck$Index[i+1]])<0) {
        peaktablecheck<- peaktablecheck[-(i+1),]
      } else {
        peaktablecheck<- peaktablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(troughtablecheck$ind2))) {
    if(is.na(troughtablecheck$ind2[i+1])) {
      print("")
    } else if(troughtablecheck$ind2[i] == troughtablecheck$ind2[i+1]) {
      if(sum(x[troughtablecheck$Index[i]:troughtablecheck$Index[i+1]])>0) {
        troughtablecheck<- troughtablecheck[-(i+1),]
      } else {
        troughtablecheck<- troughtablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(peaktablecheck$ind2))) {
    if(is.na(peaktablecheck$ind2[i+1])) {
      print("")
    } else if(peaktablecheck$ind2[i] == peaktablecheck$ind2[i+1]) {
      if(sum(x[peaktablecheck$Index[i]:peaktablecheck$Index[i+1]])<0) {
        peaktablecheck<- peaktablecheck[-(i+1),]
      } else {
        peaktablecheck<- peaktablecheck[-(i),]
      }} 
  }
  for(i in 1:length(unique(troughtablecheck$ind2))) {
    if(is.na(troughtablecheck$ind2[i+1])) {
      print("")
    } else if(troughtablecheck$ind2[i] == troughtablecheck$ind2[i+1]) {
      if(sum(x[troughtablecheck$Index[i]:troughtablecheck$Index[i+1]])>0) {
        troughtablecheck<- troughtablecheck[-(i+1),]
      } else {
        troughtablecheck<- troughtablecheck[-(i),]
      }} 
  }
  # Above iterations should get rid of all competing points
  tabling3<- peaktablecheck %>% 
    filter(Peaks==1) %>% 
    select(date) %>% 
    mutate(date=as.character(date))
  names(tabling3)<- "Peak"
  tabling4<- troughtablecheck %>% 
    filter(Troughs==1) %>% 
    select(date) %>% 
    mutate(date=as.character(date))
  names(tabling4)<- "Trough"
  
  startdate<- as.data.frame(as.yearmon("Apr 1990", format="%b %Y"))
  startdate2<- as.data.frame("Apr 1990")
  nas<- as.data.frame(NA)
  names(nas)<- "Trough"
  names(startdate)<- "Peak"
  names(startdate2)<- "Peak"
  if(nrow(tabling4)>nrow(tabling3)) {
    tabling3<- as.data.frame(rbind(startdate2, tabling3))
  }
  if(nrow(tabling3)>nrow(tabling4)) {
    tabling4<- as.data.frame(rbind(tabling4, nas))
  }
  
  tabling3<- tabling3 %>% 
    mutate(Peak=as.yearmon(Peak, "%b %Y"))
  tabling4<- tabling4 %>% 
    mutate(Trough=as.yearmon(Trough, "%b %Y"))
  
  if(tabling3[1,]>tabling4[1,]) {
    tabling3<- as.data.frame(rbind(startdate, tabling3))
    tabling4<- as.data.frame(rbind(tabling4, nas))
  }
  
  msarecessiontable<- as.data.frame(cbind(tabling3, tabling4))
  names(msarecessiontable)<- c("Peaks", "Troughs")
  msarecessiontable<- msarecessiontable %>% 
    mutate(Troughs= as.yearmon(Troughs, format="%b %Y"))
  currentdate<- as.yearmon(Sys.Date(), format="%Y-%m-%d")
  if(is.na(msarecessiontable$Troughs[length(msarecessiontable$Troughs)])) {
    msarecessiontable$Troughs[length(msarecessiontable$Troughs)]<- currentdate
  }
  msarecessiontable<<- msarecessiontable %>% 
    mutate(Months= abs((Peaks-Troughs)*12))
  
}

recessioning("Columbus")
cbus<- msarecessiontable %>% 
  mutate(MSA="Columbus", State="OH", Index=1)

recessioning("Cincinnati")
cincy<- msarecessiontable %>% 
  mutate(MSA="Cincinnati", State="OH", Index=2)

recessioning("Cleveland")
cleve<- msarecessiontable %>% 
  mutate(MSA="Cleveland", State="OH", Index=3)

recessioning("New York")
ny<- msarecessiontable %>% 
  mutate(MSA="New York", State="NY", Index=4)

recessioning("Los Angeles")
la<- msarecessiontable %>% 
  mutate(MSA="Los Angeles", State="CA", Index=5)

recessioning("Chicago")
chitown<- msarecessiontable %>% 
  mutate(MSA="Chicago", State="IL", Index=6)

recessioning("Dallas")
dal<- msarecessiontable %>% 
  mutate(MSA="Dallas", State="TX", Index=7)

recessioning("Houston")
hous<- msarecessiontable %>% 
  mutate(MSA="Houston", State="TX", Index=8)

recessioning("Philadelphia")
philly<- msarecessiontable %>% 
  mutate(MSA="Philadelphia", State="PA", Index=9)

recessioning("Washington DC")
dc<- msarecessiontable %>% 
  mutate(MSA="Washington DC", State="MD", Index=10)

recessioning("Miami")
mia<- msarecessiontable %>% 
  mutate(MSA="Miami", State="FL", Index=11)

recessioning("Atlanta")
atl<- msarecessiontable %>% 
  mutate(MSA="Atlanta", State="GA", Index=12)

recessioning("Boston")
bos<- msarecessiontable %>% 
  mutate(MSA="Boston", State="MA", Index=13)

recessioning("San Francisco")
sf<- msarecessiontable %>% 
  mutate(MSA="San Francisco", State="CA", Index=14)

recessioning("Phoenix")
pho<- msarecessiontable %>% 
  mutate(MSA="Phoenix", State="AZ", Index=15)

recessioning("Riverside")
riv<- msarecessiontable %>% 
  mutate(MSA="Riverside", State="CA", Index=16)

recessioning("Detroit")
det<- msarecessiontable %>% 
  mutate(MSA="Detroit", State="MI", Index=17)

recessioning("Seattle")
sea<- msarecessiontable %>% 
  mutate(MSA="Seattle", State="WA", Index=18)

recessioning("Minneapolis")
min<- msarecessiontable %>% 
  mutate(MSA="Minneapolis", State="MN", Index=19)

recessioning("San_Diego")
sd<- msarecessiontable %>% 
  mutate(MSA="San Diego", State="CA", Index=20)

recessioning("Tampa")
tam<- msarecessiontable %>% 
  mutate(MSA="Tampa", State="FL", Index=21)

recessioning("St. Louis")
stl<- msarecessiontable %>% 
  mutate(MSA="St. Louis", State="MO", Index=22)

recessioning("Baltimore")
bal<- msarecessiontable %>% 
  mutate(MSA="Baltimore", State="MD", Index=23)

recessioning("Denver")
den<- msarecessiontable %>% 
  mutate(MSA="Denver", State="CO", Index=24)

recessioning("Charlotte")
cha<- msarecessiontable %>% 
  mutate(MSA="Charlotte", State="NC", Index=25)

recessioning("Pittsburgh")
pit<- msarecessiontable %>% 
  mutate(MSA="Pittsburgh", State="PA", Index=26)

recessioning("Portland")
por<- msarecessiontable %>% 
  mutate(MSA="Portland", State="OR", Index=27)

recessioning("San Antonio")
san<- msarecessiontable %>% 
  mutate(MSA="San Antonio", State="TX", Index=28)

recessioning("Orlando")
orl<- msarecessiontable %>% 
  mutate(MSA="Orlando", State="FL", Index=29)

recessioning("Sacramento")
sac<- msarecessiontable %>% 
  mutate(MSA="Sacramento", State="CA", Index=30)

recessioning("Kansas City")
kc<- msarecessiontable %>% 
  mutate(MSA="Kansas City", State="MO", Index=31)

recessioning("Las Vegas")
lv<- msarecessiontable %>% 
  mutate(MSA="Las Vegas", State="NV", Index=32)

recessioning("Indianapolis")
ind<- msarecessiontable %>% 
  mutate(MSA="Indianapolis", State="IN", Index=33)

recessioning("San Jose")
sjo<- msarecessiontable %>% 
  mutate(MSA="San Jose", State="CA", Index=34)

recessioning("Austin")
aus<- msarecessiontable %>% 
  mutate(MSA="Austin", State="TX", Index=35)

recessioning("Nashville")
nas<- msarecessiontable %>% 
  mutate(MSA="Nashville", State="TN", Index=36)

recessioning("Virginia Beach")
vir<- msarecessiontable %>% 
  mutate(MSA="Virginia Beach", State="VA", Index=37)

recessioning("Providence")
pro<- msarecessiontable %>% 
  mutate(MSA="Providence", State="RI", Index=38)

recessioning("Milwaukee")
mil<- msarecessiontable %>% 
  mutate(MSA="Milwaukee", State="WI", Index=39)

recessioning("Jacksonville")
jac<- msarecessiontable %>% 
  mutate(MSA="Jacksonville", State="FL", Index=40)

recessioning("Memphis")
mem<- msarecessiontable %>% 
  mutate(MSA="Memphis", State="TN", Index=41)

recessioning("Oklahoma City")
okc<- msarecessiontable %>% 
  mutate(MSA="Oklahoma City", State="OK", Index=42)

recessioning("Louisville")
lou<- msarecessiontable %>% 
  mutate(MSA="Louisville", State="KY", Index=43)

recessioning("Richmond")
ric<- msarecessiontable %>% 
  mutate(MSA="Richmond", State="VA", Index=44)

recessioning("New Orleans")
no<- msarecessiontable %>% 
  mutate(MSA="New Orleans", State="LA", Index=45)

recessioning("Raleigh")
ral<- msarecessiontable %>% 
  mutate(MSA="Raleigh", State="NC", Index=46)

recessioning("Hartford")
har<- msarecessiontable %>% 
  mutate(MSA="Hartford", State="CT", Index=47)

recessioning("Salt Lake City")
salt<- msarecessiontable %>% 
  mutate(MSA="Salt Lake City", State="UT", Index=48)

recessioning("Birmingham")
bir<- msarecessiontable %>% 
  mutate(MSA="Birmingham", State="AL", Index=49)

recessioning("Buffalo")
buf<- msarecessiontable %>% 
  mutate(MSA="Buffalo", State="NY", Index=50)

recessioning("Norwalk")
bridge<- msarecessiontable %>% 
  mutate(MSA="Norwalk", State="CT", Index=51)

recessioning("New Haven")
nhav<- msarecessiontable %>% 
  mutate(MSA="New Haven", State="CT", Index=52)

recessioning("Worcester")
worc<- msarecessiontable %>% 
  mutate(MSA="Worcester", State="MA", Index=53)

recessioning("Albany")
alba<- msarecessiontable %>% 
  mutate(MSA="Albany", State="NY", Index=54)

recessioning("Allentown")
allen<- msarecessiontable %>% 
  mutate(MSA="Allentown", State="PA", Index=55)

recessioning("Rochester")
roch<- msarecessiontable %>% 
  mutate(MSA="Rochester", State="NY", Index=56)

recessioning("Grand Rapids")
grra<- msarecessiontable %>% 
  mutate(MSA="Grand Rapids", State="MI", Index=57)

recessioning("Columbia")
cbaa<- msarecessiontable %>% 
  mutate(MSA="Columbia", State="SC", Index=58)

recessioning("Greensboro")
gnsa<- msarecessiontable %>% 
  mutate(MSA="Greensboro", State="NC", Index=59)

recessioning("Greenville")
gnva<- msarecessiontable %>% 
  mutate(MSA="Greenville", State="SC", Index=60)

recessioning("Little Rock")
lrsa<- msarecessiontable %>% 
  mutate(MSA="Little Rock", State="AR", Index=61)

recessioning("Sarasota")
sara<- msarecessiontable %>% 
  mutate(MSA="Sarasota", State="FL", Index=62)

recessioning("Albuquerque")
abqa<- msarecessiontable %>% 
  mutate(MSA="Albuquerque", State="NM", Index=63)

recessioning("Tucson")
tuca<- msarecessiontable %>% 
  mutate(MSA="Tucson", State="AZ", Index=64)

recessioning("Tulsa")
tula<- msarecessiontable %>% 
  mutate(MSA="Tulsa", State="OK", Index=65)

recessioning("Bakersfield")
baka<- msarecessiontable %>% 
  mutate(MSA="Bakersfield", State="CA", Index=66)

recessioning("Ventura")
vena<- msarecessiontable %>% 
  mutate(MSA="Ventura", State="CA", Index=67)



msarecessiontotal<- as.data.frame(rbind(
  cbus, cincy, cleve, ny,
  la, chitown, dal, hous, philly,
  dc, mia, atl, bos, sf, pho,
  riv, det, sea, min, sd, tam,
  stl, bal, den, cha, pit, por,
  san, orl, sac, kc, lv, 
  ind, sjo, aus, nas, vir,
  pro, mil, jac, mem, okc, 
  lou, ric, no, ral, har, 
  salt, bir, buf, bridge,
  nhav, worc, alba, allen, 
  roch, grra, cbaa, gnsa, gnva,
  lrsa, sara, abqa, tuca,
  tula, baka, vena))

ab    <- (c("AL",
            "AK", "AZ", "KS", "UT", "CO", "CT",
            "DE", "FL", "GA", "HI", "ID", "IL",
            "IN", "IA", "AR", "KY", "LA", "ME",
            "MD", "MA", "MI", "MN", "MS", "MO",
            "MT", "NE", "NV", "NH", "NJ", "NM",
            "NY", "NC", "ND", "OH", "OK", "OR",
            "PA", "RI", "SC", "SD", "TN", "TX",
            "CA", "VT", "VA", "WA", "WV", "WI",
            "WY", "DC", "US"))
st    <- c("Alabama",
           "Alaska", "Arizona", "Kansas",
           "Utah", "Colorado", "Connecticut",
           "Delaware", "Florida", "Georgia",
           "Hawaii", "Idaho", "Illinois",
           "Indiana", "Iowa", "Arkansas",
           "Kentucky", "Louisiana", "Maine",
           "Maryland", "Massachusetts", "Michigan",
           "Minnesota", "Mississippi", "Missouri",
           "Montana", "Nebraska", "Nevada",
           "New Hampshire", "New Jersey", "New Mexico",
           "New York", "North Carolina", "North Dakota",
           "Ohio", "Oklahoma", "Oregon",
           "Pennsylvania", "Rhode Island", "South Carolina",
           "South Dakota", "Tennessee", "Texas",
           "California", "Vermont", "Virginia",
           "Washington", "West Virginia", "Wisconsin",
           "Wyoming", "District of Columbia", "United States")
colp<- as.data.frame(cbind(st, ab))
names(colp)<- c("Long", "State")
msarecessiontotal<- left_join(msarecessiontotal, colp, by="State")
msarecessiontotal<- msarecessiontotal[c(1,2,4,7,3,6)]
names(msarecessiontotal)[4]<- "State"
save(msarecessiontotal, file="msa.peak.table.RData")


enlarged<- all_msa %>% 
  filter(MSA=="Atlanta") %>% 
  select(Date)

enlarged<- as.data.frame(enlarged[3:(length(enlarged$Date)-6),]) 
names(enlarged)<- "Date"
enlarged<- enlarged %>% 
  mutate(Date= as.yearmon(as.character(Date), format="%b %Y"))


blankcol<- rep(0, times=length(enlarged$Date))
enlarged<- cbind(enlarged, blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol, 
      blankcol, blankcol, blankcol, blankcol, blankcol,
      blankcol, blankcol, blankcol, blankcol, blankcol,
      blankcol, blankcol, blankcol, blankcol, blankcol,
      blankcol, blankcol, blankcol, blankcol, blankcol,
      blankcol, blankcol)
names(enlarged)<- c("Date", unique(msarecessiontotal$MSA))


for(i in 1:67){
  x<- msarecessiontotal %>% 
    filter(Index==i)
  for(j in 1:length(x$Peak)){
    for(k in 1:length(enlarged$Date)) {
      if(enlarged$Date[k]>=x$Peaks[j] & enlarged$Date[k]<=x$Troughs[j]){
        enlarged[k, i+1]<- 1}
    }
  }
}

enlarged2<- melt(enlarged, id.vars="Date") %>% 
  mutate(Date=as.character(Date))
names(enlarged2)<- c("Date", "MSA", "Recession")

mastermsa<- left_join(all_msa, enlarged2, by=c("Date", "MSA"))
mastermsa$Date<- as.yearmon(mastermsa$Date, format="%b %Y")
save(mastermsa, file="msa.recession.coincident.RData")

############################################################################
## BLS API

statistical_areas<- c("Columbus",       "Cincinnati",     "Cleveland",      "New York",       "Los Angeles",    "Chicago",       
                      "Dallas",         "Houston",        "Philadelphia",   "Washington DC",  "Miami",          "Atlanta",       
                      "Boston",         "San Francisco",  "Phoenix",        "Riverside",      "Detroit",        "Seattle",       
                      "Minneapolis",    "San Diego",      "Tampa",          "St. Louis",      "Baltimore",      "Denver",        
                      "Charlotte",      "Pittsburgh",     "Portland",       "San Antonio",    "Orlando" ,       "Sacramento"  ,  
                      "Kansas City",    "Las Vegas" ,     "Indianapolis",   "San Jose",       "Austin" ,        "Nashville")

statistical_areas2<- c("Virginia Beach", "Providence",     "Milwaukee",      "Jacksonville",   "Memphis" ,       "Oklahoma City" ,
                      "Louisville",     "Richmond",       "New Orleans",    "Raleigh" ,       "Hartford"  ,     "Salt Lake City",
                      "Birmingham",     "Buffalo",        "Norwalk",        "New Haven" ,     "Worcester"  ,    "Albany",
                      "Allentown",      "Rochester",      "Grand Rapids",   "Columbia",       "Greensboro",     "Greenville",
                      "Little Rock",    "Sarasota",       "Albuquerque",    "Tucson",         "Tulsa",          "Bakersfield",
                      "Ventura")

series_ids<- c("SMS39181400000000001", "SMS39171400000000001", "SMS39174600000000001", "SMS36356200000000001", "SMS06310840000000001", "SMS17169800000000001",
               "SMS48191000000000001", "SMS48264200000000001", "SMS42379800000000001", "SMS11479000000000001", "SMS12331000000000001", "SMS13120600000000001",
               "SMS25716500000000001", "SMS06418600000000001", "SMS04380600000000001", "SMS06401400000000001", "SMS26198200000000001", "SMS53426600000000001",
               "SMS27334600000000001", "SMS06417400000000001", "SMS12453000000000001", "SMS29411800000000001", "SMS24125800000000001", "SMS08197400000000001",
               "SMS37167400000000001", "SMS42383000000000001", "SMS41389000000000001", "SMS48417000000000001", "SMS12367400000000001", "SMS06409000000000001",
               "SMS29281400000000001", "SMS32298200000000001", "SMS18269000000000001", "SMS06419400000000001", "SMS48124200000000001", "SMS47349800000000001")
               
series_ids2<- c("SMS51472600000000001", "SMS44772000000000001", "SMS55333400000000001", "SMS12272600000000001", "SMS47328200000000001", "SMS40364200000000001", 
               "SMS21311400000000001", "SMS51400600000000001", "SMS22353800000000001", "SMS37395800000000001", "SMS09734500000000001", "SMS49416200000000001", 
               "SMS01138200000000001", "SMS36153800000000001", "SMS09719500000000001", "SMS09757000000000001", "SMS25796000000000001", "SMS36105800000000001",
               "SMS42109000000000001", "SMS36403800000000001", "SMS26243400000000001", "SMS45179000000000001", "SMS37246600000000001", "SMS45248600000000001",
               "SMS05307800000000001", "SMS12358400000000001", "SMS35107400000000001", "SMS04460600000000001", "SMS40461400000000001", "SMS06125400000000001", 
               "SMS06371000000000001")

payload <- list('seriesid'=series_ids, 'startyear'='1990', 'endyear'='2009',
                'registrationKey'='467ca3bcef764cde8b120f5de59f8c15')
payload2 <- list('seriesid'=series_ids, 'startyear'='2010', 'endyear'='2019',
                 'registrationKey'='467ca3bcef764cde8b120f5de59f8c15')
payloadx <- list('seriesid'=series_ids2, 'startyear'='1990', 'endyear'='2009',
                'registrationKey'='467ca3bcef764cde8b120f5de59f8c15')
payloadx2 <- list('seriesid'=series_ids2, 'startyear'='2010', 'endyear'='2019',
                 'registrationKey'='467ca3bcef764cde8b120f5de59f8c15')

multiple <- blsAPI(payload, 2, return_data_frame = TRUE)
multiple2 <- blsAPI(payload2, 2, return_data_frame = TRUE)

multiple3 <- blsAPI(payloadx, 2, return_data_frame = TRUE)
multiple4 <- blsAPI(payloadx2, 2, return_data_frame = TRUE)


translated<- as.data.frame(cbind(statistical_areas, series_ids))
translated2<- as.data.frame(cbind(statistical_areas2, series_ids2))
names(translated)<- c("MSA", "seriesID")
names(translated2)<- c("MSA", "seriesID")
translated<- as.data.frame(rbind(translated, translated2))
city <- as.data.frame(rbind(multiple2, multiple4, multiple, multiple3))
city<- left_join(city, translated, by="seriesID")


city<- city %>% 
  mutate(Date=as.yearmon(paste(periodName, year), format="%B %Y")) %>% 
  mutate(value= as.numeric(value)) %>% 
  select(Date, MSA, value) %>% 
  arrange(MSA, Date)

for(i in 2:length(city$value)){
  city$Employment_Change[i]<- (city$value[i]-city$value[i-1])/city$value[i-1]
}

names(city)[3]<- "Employed_Workforce"

city<- city %>% 
  filter(Date>as.yearmon("Jan 1990", format="%b %Y")) %>% 
  arrange(Date, MSA)

mastermsa<- left_join(city, mastermsa, by=c("Date", "MSA")) %>% 
  arrange(Date, MSA)
###################################################################################
# Add Current Month Data, and Lags

ab<- as.data.frame(rep(as.character(as.yearmon(Sys.Date(), format="%Y-%m-%d")), times=67))
bc<- mastermsa$MSA[(length(mastermsa$MSA)-66):length(mastermsa$MSA)]
cd<- mastermsa$Employed_Workforce[(length(mastermsa$MSA)-66):length(mastermsa$MSA)]
de<- mastermsa$Employment_Change[(length(mastermsa$MSA)-66):length(mastermsa$MSA)]
ef<- rep(NA, times=67)

currentmonthdata<- as.data.frame(cbind(ab, bc, cd, de, ef, ef))
names(currentmonthdata)<- c(names(mastermsa))
currentmonthdata <- currentmonthdata %>% 
  mutate(Date=as.yearmon(Date, format="%b %Y")) %>% 
  mutate(MSA= as.character(MSA)) %>% 
  mutate_if(is.logical, as.numeric)
###############
elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

addlmonths<- elapsed_months(as.yearmon(Sys.Date(), format="%Y-%m-%d"), mastermsa$Date[length(mastermsa$Date)])-1
if(addlmonths==1){
  ab<- as.data.frame(rep(as.yearmon(Sys.Date() %m+% months(-1), format="%Y-%m-%d"), times=67))
  othermonthdata<- as.data.frame(cbind(ab, bc, cd, de, ef, ef))
  names(othermonthdata)<- c(names(mastermsa))
  
  othermonthdata <- othermonthdata %>% 
    mutate(Date=as.yearmon(Date, format="%b %Y")) %>% 
    mutate(MSA= as.character(MSA)) %>% 
    mutate_if(is.logical, as.numeric)
  currentmonthdata<- rbind(othermonthdata, currentmonthdata)
}

mastermsa<- as.data.frame(rbind(mastermsa, currentmonthdata))

mastermsa<- mastermsa %>% 
  arrange(MSA, Date)

Coincident_Lag_4 <- rep(0, length(mastermsa$Date))
Coincident_Lag_5 <- rep(0, length(mastermsa$Date))
Coincident_Lag_6 <- rep(0, length(mastermsa$Date))
Coincident_Lag_7 <- rep(0, length(mastermsa$Date))
Coincident_Lag_8 <- rep(0, length(mastermsa$Date))
Coincident_Lag_9 <- rep(0, length(mastermsa$Date))
Coincident_Lag_10 <- rep(0, length(mastermsa$Date))
Coincident_Lag_11 <- rep(0, length(mastermsa$Date))
Coincident_Lag_12 <- rep(0, length(mastermsa$Date))
Recession_Lag_10 <- rep(0, length(mastermsa$Date))
Recession_Lag_11 <- rep(0, length(mastermsa$Date))

msadata <- as.data.frame(cbind(mastermsa, 
                               Coincident_Lag_4, Coincident_Lag_5, Coincident_Lag_6, Coincident_Lag_7,
                               Coincident_Lag_8, Coincident_Lag_9, Coincident_Lag_10, Recession_Lag_10,
                               Recession_Lag_11))
for(i in 12:length(msadata$Date)) {
  msadata$Coincident_Lag_4[i]<- msadata$Coincident[i-4]
  msadata$Coincident_Lag_5[i]<- msadata$Coincident[i-5]
  msadata$Coincident_Lag_6[i]<- msadata$Coincident[i-6]
  msadata$Coincident_Lag_7[i]<- msadata$Coincident[i-7]
  msadata$Coincident_Lag_8[i]<- msadata$Coincident[i-8]
  msadata$Coincident_Lag_9[i]<- msadata$Coincident[i-9]
  msadata$Coincident_Lag_10[i]<- msadata$Coincident[i-10]
  msadata$Recession_Lag_10[i]<- msadata$Recession[i-10]
  msadata$Recession_Lag_11[i]<- msadata$Recession[i-11]
}

msadatarest<- msadata%>% 
  filter(Date>=as.yearmon("Mar 1991", format="%b %Y"))
###################################################################################
# Clusters Diverged

load("msaclusters.RData")

msadatarest<- left_join(msadatarest, msacluster, by="MSA")

Cluster_2 <- rep(0, length(msadatarest$Date))
Cluster_3 <- rep(0, length(msadatarest$Date))
Cluster_4 <- rep(0, length(msadatarest$Date))
Cluster_5 <- rep(0, length(msadatarest$Date))
msadatarest <- as.data.frame(cbind(msadatarest, Cluster_2, Cluster_3, Cluster_4, Cluster_5))
msadatarest<- msadatarest %>% 
  mutate(Cluster= as.numeric(as.character(msadatarest$Cluster)))

for(i in 1:length(msadatarest$Cluster)){
  if(msadatarest$Cluster[i]==2) {
    msadatarest$Cluster_2[i]=1} else 
      if(msadatarest$Cluster[i]==3) {
        msadatarest$Cluster_3[i]=1} else
          if(msadatarest$Cluster[i]==4) {
            msadatarest$Cluster_4[i]=1} else
              if(msadatarest$Cluster_5[i]==5) {
                msadatarest$Cluster_5[i]=1}}

msadatarest<-msadatarest %>% 
  select(-Cluster, -Latitude, -Longitude)

###################################################################################
# Other FRED Data via API

Consumer_Sentiment<- fredr_series_observations(
  series_id = "UMCSENT",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
Cons<- rep(NA, times=length(Consumer_Sentiment$date))
Consumer_Sentiment<- cbind(Consumer_Sentiment, Cons)
names(Consumer_Sentiment)[4]<- "Consumer_Sentiment"
for(i in 2:length(Consumer_Sentiment$date)){
  Consumer_Sentiment$Consumer_Sentiment[i]<- Consumer_Sentiment$value[i]-Consumer_Sentiment$value[i-1]
}

Consumer_Sentiment<- Consumer_Sentiment %>% 
  select(date, Consumer_Sentiment) %>% 
  filter(!is.na(Consumer_Sentiment)) %>% 
  mutate(Date=as.yearmon(as.character(date), format="%Y-%m-%d")) %>% 
  select(-date)

US_Leading_Indicator<- fredr_series_observations(
  series_id = "USSLIND",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)

US_Leading_Indicator<- US_Leading_Indicator %>% 
  mutate(Date=as.yearmon(as.character(date), format="%Y-%m-%d")) %>% 
  select(Date, value)
names(US_Leading_Indicator)[2]<- "US_Leading_Indicator"

fredindicators<- left_join(Consumer_Sentiment, US_Leading_Indicator, by="Date") %>% 
  select(Date, Consumer_Sentiment, US_Leading_Indicator)
fredindicators<- as.data.frame(fredindicators)

bcrude<- fredr_series_observations(
  series_id = "DCOILBRENTEU",
  observation_start = as.Date("1990-02-01"),
  frequency = "m",
  units="lin"
)
bcrude<- bcrude %>% 
  mutate(Date=as.yearmon(as.character(date), format="%Y-%m-%d")) %>% 
  mutate(Brent_Crude=value) %>% 
  select(Date, Brent_Crude)

fredindicators<- left_join(bcrude, fredindicators, by="Date")  

msadatarest<- left_join(msadatarest, fredindicators, by="Date") 

msadatarest <- msadatarest %>% 
  arrange(MSA)

for(i in 4:length(msadatarest$MSA)){
  
  if(is.na(msadatarest$Brent_Crude[i])){
    msadatarest$Brent_Crude[i]<- msadatarest$Brent_Crude[i-1]}
  
  if(is.na(msadatarest$Consumer_Sentiment[i]) & is.na(msadatarest$Consumer_Sentiment[i-1])){
    msadatarest$Consumer_Sentiment[i]<- msadatarest$Consumer_Sentiment[i-2]
    msadatarest$Consumer_Sentiment[i-1]<- msadatarest$Consumer_Sentiment[i-2]
  } else if(is.na(msadatarest$Consumer_Sentiment[i])){
    msadatarest$Consumer_Sentiment[i]<- msadatarest$Consumer_Sentiment[i-1]}
  
  if(is.na(msadatarest$US_Leading_Indicator[i]) & is.na(msadatarest$US_Leading_Indicator[i-1]) & is.na(msadatarest$US_Leading_Indicator[i-2])){
    msadatarest$US_Leading_Indicator[i]<- msadatarest$US_Leading_Indicator[i-3]
    msadatarest$US_Leading_Indicator[i-1]<- msadatarest$US_Leading_Indicator[i-3]
    msadatarest$US_Leading_Indicator[i-2]<- msadatarest$US_Leading_Indicator[i-3]
  } else if(is.na(msadatarest$US_Leading_Indicator[i]) & is.na(msadatarest$US_Leading_Indicator[i-1])){
    msadatarest$US_Leading_Indicator[i]<- msadatarest$US_Leading_Indicator[i-2]
    msadatarest$US_Leading_Indicator[i-1]<- msadatarest$US_Leading_Indicator[i-2]
  } else if(is.na(msadatarest$US_Leading_Indicator[i])){
    msadatarest$US_Leading_Indicator[i]<- msadatarest$US_Leading_Indicator[i-1]  
    }
}

# Balancing dataset (Data release on the 8th paradox)
if(day(Sys.Date())<=8){
  for(i in 1:length(msadatarest$MSA)){
    if(is.na(msadatarest$Coincident_Lag_4[i])){
      msadatarest$Coincident_Lag_4[i]<- msadatarest$Coincident_Lag_4[i-1]
      msadatarest$Recession_Lag_10[i]<- msadatarest$Recession_Lag_10[i-1]
    }
  }
}

for(i in 1:length(msadatarest$MSA)){
  if(is.na(msadatarest$Coincident_Lag_4[i])){
    msadatarest$Coincident_Lag_4[i]<- msadatarest$Coincident_Lag_4[i-1]
    msadatarest$Recession_Lag_10[i]<- msadatarest$Recession_Lag_10[i-1]
  }
}

# Dataset for Nowcasting


msadatarest$Threemonthsout<- rep(0, times=length(msadatarest$Date))
msadatarest$Sixmonthsout<- rep(0, times=length(msadatarest$Date))

for(i in 1:length(msadatarest$Date)){
  if(msadatarest$Date[i] > as.yearmon("Aug 1991", format="%b %Y") & msadatarest$Date[i] < msadatarest$Date[length(msadatarest$Date)-5]){
    msadatarest$Threemonthsout[i]<- msadatarest$Recession[i+3]
    msadatarest$Sixmonthsout[i]<- msadatarest$Recession[i+6]
  } else {
    msadatarest$Threemonthsout[i]<- NA
    msadatarest$Sixmonthsout[i]<- NA
  }
}

msaboostdata<- msadatarest %>% 
  filter(Date>as.yearmon("Aug 1991", format="%b %Y")) %>% 
  arrange(Date, MSA)

# Dataset for Forecasting
save(msaboostdata, file="msaboostdata2.RData")


##############################################################################################
# Datasets Complete, moving to XGBOOST Nowcasting



#load("msaforecasting.RData")
#load("msanowcasting.RData")
afteradd2<- msaboostdata[1:2]
msaboostdata<- msaboostdata[c(-1,-2, -5)]
f4<- msaboostdata[c(3, 1, 2, 4:18)]

traina2<- f4[1:8300,]
testa2<- f4[8301:length(msaboostdata$Employed_Workforce),]

train.label2<- traina2$Recession
test.label2<- testa2$Recession

train.data2<- as.matrix(traina2[-1])
test.data2<- as.matrix(testa2[-1])

dtrain2 <- xgb.DMatrix(data = train.data2, label = train.label2)
dtest2 <- xgb.DMatrix(data = test.data2, label = test.label2)

bstDMatrix3 <- xgboost(data = dtrain2, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
predx<- as.data.frame(predict(bstDMatrix3, test.data2))
predx2<- as.data.frame(predict(bstDMatrix3, train.data2))

importance_matrix <- xgb.importance(model = model.now)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

names(predx)<- "Probability"
names(predx2)<- "Probability"
predictions2<- rbind(predx2, predx)

resultsxx2<- as.data.frame(cbind(afteradd2, predictions2, msaboostdata$Recession))
names(resultsxx2)[4]<- "Actual"
resultsxx2$Date<- as.yearmon(resultsxx2$Date, format="%b %Y")

f42<- function(x){
  resultsx22<- resultsxx2 %>% 
    filter(MSA==x)
  
  plot(x=resultsx22$Date, y=resultsx22$Actual, col="darkorange3", type='l', ylab="",
       xlab="")
  lines(x=resultsx22$Date, y=resultsx22$Probability, col="deepskyblue")
}


########
xgbpred <- ifelse (predx > 0.3,1,0)
xgbpred<- as.factor(as.vector(xgbpred))
test.label2<- as.factor(test.label2)
a <- confusionMatrix(xgbpred, test.label2)

ActualValue <- factor(c(0, 0, 1, 1))
PredictedValue <- factor(c(1, 0, 1, 0))
Yzx2      <- c(a$table[2,1], a$table[1,1], a$table[2,2], a$table[1,2])
dfxx2 <- data.frame(ActualValue, PredictedValue, Yzx2)

a
ggplot(data =  dfxx2, mapping = aes(x = ActualValue, y = PredictedValue)) +
  geom_tile(aes(fill = Yzx2), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Yzx2)), vjust = 1) +
  scale_fill_gradient(low = "deepskyblue", high = "firebrick") +
  theme_bw() + theme(legend.position = "none")+
  ggtitle("MSA Confusion Matrix for Nowcasting 2005-2018")+
  theme(plot.title = element_text(hjust = 0.5))





