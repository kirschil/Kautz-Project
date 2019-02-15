library(reshape2)
library(fredr)
library(blsAPI)
library(tidyverse)
library(zoo)
library(xgboost)
library(Matrix)
library(caret)
library(readxl)
library(httr)
library(lubridate)

fredr_set_key("f71c3c11e9a38e7361a8f04ca2c6b889")

url <- "https://www.philadelphiafed.org/-/media/research-and-data/regional-economy/indexes/coincident/coincident-revised.xls?la=en"
download.file(url = url, mode = "wb", destfile = "statecoincidentdata.xls")
# Start

statecoin<- read_excel("statecoincidentdata.xls")
statecoin<- statecoin[c(4:length(statecoin$Date)),]
Date<- as.character(statecoin$Date[-1])
statecoin<- as.data.frame(statecoin[,-1])
statecoin<- apply(statecoin, 2, diff)
statecoin<- as.data.frame(cbind(Date, statecoin)) %>% 
  melt(id="Date")
statecoin$Date<- as.yearmon(statecoin$Date, format="%Y-%m-%d")

ab    <- (c("AL",
            "AK", "AZ", "KS", "UT", "CO", "CT",
            "DE", "FL", "GA", "HI", "ID", "IL",
            "IN", "IA", "AR", "KY", "LA", "ME",
            "MD", "MA", "MI", "MN", "MS", "MO",
            "MT", "NE", "NV", "NH", "NJ", "NM",
            "NY", "NC", "ND", "OH", "OK", "OR",
            "PA", "RI", "SC", "SD", "TN", "TX",
            "CA", "VT", "VA", "WA", "WV", "WI",
            "WY", "US", "DC"))
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
           "Wyoming", "United States", "District of Columbia")
colp<- as.data.frame(cbind(st, ab))
names(colp)<- c("State", "variable")

statecoin <- left_join(statecoin, colp, by="variable")
statecoin<- statecoin[c(1,4,3)]
names(statecoin)<- c("Date", "State", "Coincident")


recessioning<- function(x) {
  
  y<- statecoin %>% 
    filter(State==x)
  x<- y %>% 
    select(Coincident)
  x<- as.numeric(x$Coincident)
  
  
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
  
  startdate<- as.data.frame(as.yearmon("May 1979", format="%b %Y"))
  startdate2<- as.data.frame("May 1979")
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
  
  staterecessiontable<- as.data.frame(cbind(tabling3, tabling4))
  names(staterecessiontable)<- c("Peaks", "Troughs")
  staterecessiontable<- staterecessiontable %>% 
    mutate(Troughs= as.yearmon(Troughs, format="%b %Y"))
  currentdate<- as.yearmon(Sys.Date(), format="%Y-%m-%d")
  if(is.na(staterecessiontable$Troughs[length(staterecessiontable$Troughs)])) {
    staterecessiontable$Troughs[length(staterecessiontable$Troughs)]<- currentdate
  }
  staterecessiontable<<- staterecessiontable %>% 
    mutate(Months= abs((Peaks-Troughs)*12)) %>% 
    filter(Months>=5)
  
}




recessioning("Alabama")
AL<- staterecessiontable %>% 
  mutate(Index=1)
recessioning("Alaska")
AK<- staterecessiontable %>% 
  mutate(Index=2)
recessioning("Arizona")
AZ<- staterecessiontable %>% 
  mutate(Index=3)
recessioning("Kansas")
KS<- staterecessiontable %>% 
  mutate(Index=4)
recessioning("Utah")
UT<- staterecessiontable %>%    mutate(Index=5)
recessioning("Colorado")
CO<- staterecessiontable %>%    mutate(Index=6)
recessioning("Connecticut")
CT<- staterecessiontable %>%    mutate(Index=7)
recessioning("Delaware")
DE<- staterecessiontable %>%    mutate(Index=8)
recessioning("Florida")
FL<- staterecessiontable %>%    mutate(Index=9)
recessioning("Georgia")
GA<- staterecessiontable %>%    mutate(Index=10)
recessioning("Hawaii")
HI<- staterecessiontable %>%    mutate(Index=11)
recessioning("Idaho")
ID<- staterecessiontable %>%    mutate(Index=12)
recessioning("Illinois")
IL<- staterecessiontable %>%    mutate(Index=13)
recessioning("Indiana")
IN<- staterecessiontable %>%    mutate(Index=14)  
recessioning("Iowa")
IA<- staterecessiontable %>%    mutate(Index=15)
recessioning("Arkansas")
AR<- staterecessiontable %>%    mutate(Index=16)
recessioning("Kentucky")
KY<- staterecessiontable %>%    mutate(Index=17)
recessioning("Louisiana")
LA<- staterecessiontable %>%    mutate(Index=18)
recessioning("Maine")
ME<- staterecessiontable %>%    mutate(Index=19)
recessioning("Maryland")
MD<- staterecessiontable %>%    mutate(Index=20)
recessioning("Massachusetts")
MA<- staterecessiontable %>%    mutate(Index=21)
recessioning("Michigan")
MI<- staterecessiontable %>%    mutate(Index=22)
recessioning("Minnesota")
MN<- staterecessiontable %>%    mutate(Index=23)
recessioning("Mississippi")
MS<- staterecessiontable %>%    mutate(Index=24)
recessioning("Missouri")
MO<- staterecessiontable %>%    mutate(Index=25)
recessioning("Montana")
MT<- staterecessiontable %>%    mutate(Index=26)
recessioning("Nebraska")
NE<- staterecessiontable %>%    mutate(Index=27)
recessioning("Nevada")
NV<- staterecessiontable %>%    mutate(Index=28)
recessioning("New Hampshire")
NH<- staterecessiontable %>%    mutate(Index=29)
recessioning("New Jersey")
NJ<- staterecessiontable %>%    mutate(Index=30)
recessioning("New Mexico")
NM<- staterecessiontable %>%    mutate(Index=31)
recessioning("New York")
NY<- staterecessiontable %>%    mutate(Index=32)
recessioning("North Carolina")
NC<- staterecessiontable %>%    mutate(Index=33)
recessioning("North Dakota")
ND<- staterecessiontable %>%    mutate(Index=34)
recessioning("Ohio")
OH<- staterecessiontable %>%    mutate(Index=35)
recessioning("Oklahoma")
OK<- staterecessiontable %>%    mutate(Index=36)
recessioning("Oregon")
OR<- staterecessiontable %>%    mutate(Index=37)
recessioning("Pennsylvania")
PA<- staterecessiontable %>%    mutate(Index=38)
recessioning("Rhode Island")
RI<- staterecessiontable %>%    mutate(Index=39)
recessioning("South Carolina")
SC<- staterecessiontable %>%    mutate(Index=40)
recessioning("South Dakota")
SD<- staterecessiontable %>%    mutate(Index=41)
recessioning("Tennessee")
TN<- staterecessiontable %>%    mutate(Index=42)
recessioning("Texas")
TX<- staterecessiontable %>%    mutate(Index=43)
recessioning("California")
CA<- staterecessiontable %>%    mutate(Index=44)
recessioning("Vermont")
VT<- staterecessiontable %>%    mutate(Index=45)
recessioning("Virginia")
VA<- staterecessiontable %>%    mutate(Index=46)
recessioning("Washington")
WA<- staterecessiontable %>%    mutate(Index=47)
recessioning("West Virginia")
WV<- staterecessiontable %>%    mutate(Index=48)
recessioning("Wisconsin")
WI<- staterecessiontable %>%    mutate(Index=49)
recessioning("Wyoming")
WY<- staterecessiontable %>%    mutate(Index=50)
recessioning("United States")
US<- staterecessiontable %>%    mutate(Index=51)
  
  
statetables<- as.data.frame(rbind(AL, AK, AZ, KS, UT, CO, CT, DE, FL, GA, HI, ID, IL,
                                  IN, IA, AR, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT,
                                  NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, 
                                  SC, SD, TN, TX, CA, VT, VA, WA, WV, WI, WY, US))
  
  
colp2<- as.data.frame(cbind(st[1:51], 1:51))
names(colp2)<- c("State", "Index")
colp2$Index<- as.numeric(as.character(colp2$Index))



statetables<- left_join(statetables, colp2, by="Index")

save(statetables, file = "state.peak.table.RData")
statetables<- statetables %>% 
  filter(State!="United States")

enlarged<- statecoin %>% 
  filter(State=="Alabama") %>% 
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
                 blankcol, blankcol, blankcol, blankcol, blankcol)
names(enlarged)<- c("Date", c(as.character(unique(statetables$State))))


for(i in 1:50){
  x<- statetables %>% 
    filter(Index==i)
  for(j in 1:length(x$Peak)){
    for(k in 1:length(enlarged$Date)) {
      if(enlarged$Date[k]>=x$Peaks[j] & enlarged$Date[k]<=x$Troughs[j]){
        enlarged[k, i+1]<- 1}
    }
  }
}

enlarged2<- melt(enlarged, id.vars="Date")
names(enlarged2)<- c("Date", "State", "Recession")

masterstate<- left_join(statecoin, enlarged2, by=c("Date", "State"))
masterstate$Coincident<- as.numeric(masterstate$Coincident)
masterstate<- masterstate %>% 
  arrange(Date, State)
save(masterstate, file = "state.recession.coincident.RData")

masterstate<- masterstate %>% 
  filter(State!="United States")
##########################################################################
ab<- as.data.frame(rep(as.character(as.yearmon(Sys.Date(), format="%Y-%m-%d")), times=50))
bc<- masterstate$State[(length(masterstate$State)-49):length(masterstate$State)]
ef<- rep(NA, times=50)

currentmonthdata<- as.data.frame(cbind(ab, bc, ef, ef))
names(currentmonthdata)<- c(names(masterstate))
currentmonthdata <- currentmonthdata %>% 
  mutate(Date=as.yearmon(Date, format="%b %Y")) %>% 
  mutate(State= as.character(State)) %>% 
  mutate_if(is.logical, as.numeric)

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

addlmonths<- elapsed_months(as.yearmon(Sys.Date(), format="%Y-%m-%d"), masterstate$Date[length(masterstate$Date)])-1
if(addlmonths==1){
  ab<- as.data.frame(rep(as.yearmon(Sys.Date() %m+% months(-1), format="%Y-%m-%d"), times=50))
  othermonthdata<- as.data.frame(cbind(ab, bc, ef, ef))
  names(othermonthdata)<- c(names(masterstate))
  
  othermonthdata <- othermonthdata %>% 
    mutate(Date=as.yearmon(Date, format="%b %Y")) %>% 
    mutate(State= as.character(State)) %>% 
    mutate_if(is.logical, as.numeric)
  currentmonthdata<- rbind(othermonthdata, currentmonthdata)
}


masterstate<- as.data.frame(rbind(masterstate, currentmonthdata))

masterstate<- masterstate %>% 
  arrange(State, Date)

Coincident_Lag_2 <- rep(NA, length(masterstate$Date))
Coincident_Lag_3 <- rep(NA, length(masterstate$Date))
Coincident_Lag_4 <- rep(NA, length(masterstate$Date))
Coincident_Lag_5 <- rep(NA, length(masterstate$Date))
Coincident_Lag_6 <- rep(NA, length(masterstate$Date))
Coincident_Lag_7 <- rep(NA, length(masterstate$Date))
Coincident_Lag_8 <- rep(NA, length(masterstate$Date))
Recession_Lag_8 <- rep(NA, length(masterstate$Date))
Recession_Lag_9 <- rep(NA, length(masterstate$Date))
Recession_Lag_10 <- rep(NA, length(masterstate$Date))

statesdata <- as.data.frame(cbind(masterstate, 
                               Coincident_Lag_2, Coincident_Lag_3, Coincident_Lag_4, Coincident_Lag_5,
                               Coincident_Lag_6, Coincident_Lag_7, Coincident_Lag_8, Recession_Lag_8,
                               Recession_Lag_9, Recession_Lag_10))

for(i in 11:length(statesdata$Date)) {
  statesdata$Coincident_Lag_2[i]<- statesdata$Coincident[i-2]
  statesdata$Coincident_Lag_3[i]<- statesdata$Coincident[i-3]
  statesdata$Coincident_Lag_4[i]<- statesdata$Coincident[i-4]
  statesdata$Coincident_Lag_5[i]<- statesdata$Coincident[i-5]
  statesdata$Coincident_Lag_6[i]<- statesdata$Coincident[i-6]
  statesdata$Coincident_Lag_7[i]<- statesdata$Coincident[i-7]
  statesdata$Coincident_Lag_8[i]<- statesdata$Coincident[i-8]
  statesdata$Recession_Lag_8[i]<- statesdata$Recession[i-8]
  statesdata$Recession_Lag_9[i]<- statesdata$Recession[i-9]
  statesdata$Recession_Lag_10[i]<- statesdata$Recession[i-10]
}

statesdata<- statesdata%>% 
  filter(Date>=as.yearmon("May 1980", format="%b %Y"))
##########################################################################
# BLS API


orderedstates<- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                  "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", 
                  "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
                  "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
                  "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
                  "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

series_ids_2<- c("SMS01000000000000001", "SMS02000000000000001", "SMS04000000000000001", "SMS05000000000000001",
                 "SMS06000000000000001", "SMS08000000000000001", "SMS09000000000000001", "SMS10000000000000001",
                 "SMS12000000000000001", "SMS13000000000000001", "SMS15000000000000001", "SMS16000000000000001",
                 "SMS17000000000000001", "SMS18000000000000001", "SMS19000000000000001", "SMS20000000000000001",
                 "SMS21000000000000001", "SMS22000000000000001", "SMS23000000000000001", "SMS24000000000000001",
                 "SMS25000000000000001", "SMS26000000000000001", "SMS27000000000000001", "SMS28000000000000001", 
                 "SMS29000000000000001", "SMS30000000000000001", "SMS31000000000000001", "SMS32000000000000001",
                 "SMS33000000000000001", "SMS34000000000000001", "SMS35000000000000001", "SMS36000000000000001",
                 "SMS37000000000000001", "SMS38000000000000001", "SMS39000000000000001", "SMS40000000000000001",
                 "SMS41000000000000001", "SMS42000000000000001", "SMS44000000000000001", "SMS45000000000000001",
                 "SMS46000000000000001", "SMS47000000000000001", "SMS48000000000000001", "SMS49000000000000001",
                 "SMS50000000000000001", "SMS51000000000000001", "SMS53000000000000001", "SMS54000000000000001",
                 "SMS55000000000000001", "SMS56000000000000001")

payload3 <- list('seriesid'=series_ids_2, 'startyear'='1990', 'endyear'='2009',
                 'registrationKey'='467ca3bcef764cde8b120f5de59f8c15')
payload4 <- list('seriesid'=series_ids_2, 'startyear'='2010', 'endyear'='2019',
                 'registrationKey'='467ca3bcef764cde8b120f5de59f8c15')

multiple3 <- blsAPI(payload3, 2, return_data_frame = TRUE)
multiple4 <- blsAPI(payload4, 2, return_data_frame = TRUE)

translated2<- as.data.frame(cbind(orderedstates, series_ids_2))
names(translated2)<- c("State", "seriesID")
state <- as.data.frame(rbind(multiple3, multiple4))
state<- left_join(state, translated2, by="seriesID")

state<- state %>% 
  mutate(Date=as.yearmon(paste(periodName, year), format="%B %Y")) %>% 
  mutate(value= as.numeric(value)) %>% 
  select(Date, State, value) %>% 
  arrange(State, Date)

for(i in 2:length(state$value)){
  state$Employment_Change[i]<- ((state$value[i]-state$value[i-1])/state$value[i-1])
}

names(state)[3]<- "Employed_Workforce"

state<- state %>% 
  filter(Date>as.yearmon("Jan 1990", format="%b %Y")) %>% 
  arrange(Date, State)

statesdata<- left_join(statesdata, state, by=c("Date", "State"))

###################################################################################
# Cluster
load("ab34.RData")
clusters<- as.data.frame(cbind(ab34$NAME, ab34$Cluster))
names(clusters)<- c("State", "Cluster")
statesdata<- left_join(statesdata, clusters, by="State")

Cluster_2 <- rep(0, length(statesdata$Date))
Cluster_3 <- rep(0, length(statesdata$Date))
Cluster_4 <- rep(0, length(statesdata$Date))
statesdata <- as.data.frame(cbind(statesdata, Cluster_2, Cluster_3, Cluster_4))
statesdata<- statesdata %>% 
  mutate(Cluster= as.numeric(as.character(statesdata$Cluster))) %>% 
  filter(State!="United States")



for(i in 1:length(statesdata$Cluster)){
  if(statesdata$Cluster[i]==2){
    statesdata$Cluster_2[i]<-1
    } else if(statesdata$Cluster[i]==3){
        statesdata$Cluster_3[i]<-1
        } else if(statesdata$Cluster[i]==4){
            statesdata$Cluster_4[i]<-1}}

statesdata<-statesdata %>% 
  select(-Cluster)
####################################################################################
# NAICS
load("full6b2.RData")
full6a<- full6b %>% 
  mutate(date2=as.yearmon(date, format="%b %Y")) %>% 
  arrange(date2)
full6a<- full6a[1:50, c(2, 13:30)]

statesdata<- left_join(statesdata, full6a, by="State")

####################################################################################
# FRED API DATA

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

save(bcrude, file="brentcrude.RData")

fredindicators<- left_join(bcrude, fredindicators, by="Date")  

statesdata<- left_join(statesdata, fredindicators, by="Date") 

statesdata <- statesdata %>% 
  arrange(State, Date)

for(i in 4:length(statesdata$State)){
  
  if(is.na(statesdata$Brent_Crude[i])){
    statesdata$Brent_Crude[i]<- statesdata$Brent_Crude[i-1]}
  
  if(is.na(statesdata$Consumer_Sentiment[i]) & is.na(statesdata$Consumer_Sentiment[i-1])){
    statesdata$Consumer_Sentiment[i]<- statesdata$Consumer_Sentiment[i-2]
    statesdata$Consumer_Sentiment[i-1]<- statesdata$Consumer_Sentiment[i-2]
  } else if(is.na(statesdata$Consumer_Sentiment[i])){
    statesdata$Consumer_Sentiment[i]<- statesdata$Consumer_Sentiment[i-1]}
  
  if(is.na(statesdata$US_Leading_Indicator[i]) & is.na(statesdata$US_Leading_Indicator[i-1]) & is.na(statesdata$US_Leading_Indicator[i-2])){
    statesdata$US_Leading_Indicator[i]<- statesdata$US_Leading_Indicator[i-3]
    statesdata$US_Leading_Indicator[i-1]<- statesdata$US_Leading_Indicator[i-3]
    statesdata$US_Leading_Indicator[i-2]<- statesdata$US_Leading_Indicator[i-3]
  } else if(is.na(statesdata$US_Leading_Indicator[i]) & is.na(statesdata$US_Leading_Indicator[i-1])){
    statesdata$US_Leading_Indicator[i]<- statesdata$US_Leading_Indicator[i-2]
    statesdata$US_Leading_Indicator[i-1]<- statesdata$US_Leading_Indicator[i-2]
  } else if(is.na(statesdata$US_Leading_Indicator[i])){
    statesdata$US_Leading_Indicator[i]<- statesdata$US_Leading_Indicator[i-1]  
  }
}

Three_Months_Out<- NA
Six_Months_Out<- NA

statesdata<- cbind(statesdata, Three_Months_Out, Six_Months_Out)
statesdata<- statesdata %>% 
  filter(Date>=as.yearmon("Apr 1990", format="%b %Y")) %>% 
  arrange(State, Date)

for(i in 1:(length(statesdata$Date)-6)){
  statesdata$Three_Months_Out[i]<- statesdata$Recession[i+3]
  statesdata$Six_Months_Out[i]<- statesdata$Recession[i+6]
}

statesdata<- statesdata %>% 
  arrange(Date, State)

for(i in 1:(length(statesdata$Date))){
  if(statesdata$Date[i]>=statesdata$Date[(length(statesdata$Date)-350)]){
    statesdata$Three_Months_Out[i]<- NA
    statesdata$Six_Months_Out[i]<- NA
}}

statesdata<- statesdata %>% 
  arrange(State, Date)

for(i in 4:length(statesdata$State)){
  if(is.na(statesdata$Employed_Workforce[i]) & is.na(statesdata$Employed_Workforce[i-1])){
    statesdata$Employed_Workforce[i]<- statesdata$Employed_Workforce[i-2]
    statesdata$Employed_Workforce[i-1]<- statesdata$Employed_Workforce[i-2]
  } else if(is.na(statesdata$Employed_Workforce[i])){
    statesdata$Employed_Workforce[i]<- statesdata$Employed_Workforce[i-1]}
  
  if(is.na(statesdata$Employment_Change[i]) & is.na(statesdata$Employment_Change[i-1])){
    statesdata$Employment_Change[i]<- statesdata$Employment_Change[i-2]
    statesdata$Employment_Change[i-1]<- statesdata$Employment_Change[i-2]
  } else if(is.na(statesdata$Employment_Change[i])){
    statesdata$Employment_Change[i]<- statesdata$Employment_Change[i-1]}
}

statesdata<- statesdata %>% 
  arrange(Date, State)
#############################################################################################
#############################################################################################
# Dataset for Forecasting
save(statesdata, file="statesdata.RData")


ohio_manu<- fredr_series_observations(
  series_id = "OHMFG",
  observation_start = as.Date("1990-01-01"),
  frequency = "m",
  units="lin"
)
ohio_manu<- ohio_manu[-2]
names(ohio_manu)<- c("Date", "Emp_Manufacturing_Thousands")
ohio_manu<- ohio_manu %>% 
  mutate(Date=as.yearmon(as.character(Date), format="%Y-%m-%d"))

save(ohio_manu, file="ohio_manu.RData")


