library(data.table)
library(stringr)
library(dplyr)

### read in data
season <- fread("seasonality.csv")
season[,yearweekid:=strftime(tran_wk,format = "%Y%U")]
season[yearweekid=="201552",yearweekid:="201600"]

holiday <- fread("holiday.csv")
holiday[,yearweekid:=strftime(tran_wk,format = "%Y%U")]
holiday[yearweekid=="201552",yearweekid:="201600"] # only 2 days in the last week of 2015, which is only week in 2015, so we bind it into the first week of 2016

wkdata <- fread("weekdata.csv")
wkdata$prod_id <- as.character(wkdata$prod_id)
names(wkdata)[2] <- "yearweekid"
wkdata$weekid <- str_sub(wkdata$yearweekid,-2,-1)
wkdata$yearweekid <- as.character(wkdata$yearweekid)

trans <- fread("clean_transaction.csv")

promo <- fread("promo_ad.csv")
setnames(promo,"prod_assoc","prod_id")
promo[,yearweekid:=strftime(tran_wk,format = "%Y%U")]

prod <- fread("product_table_supp.csv")
SMbeer <- prod$prod_id # three product

# transform the promotion for all to three records, each product in one line
promoall <- promo[prod_id=="ALL"]
promosgl <- promo[prod_id!="ALL"]

for (i in SMbeer){
  p <- promo[prod_id=="ALL"]
  p[,prod_id:=i]
  promoall = rbind(promoall,p)
}
promoall <- promoall[prod_id!="ALL"]
promo <- rbind(promoall,promosgl)

# bind the last week of 2015 to the first week of 2016 which "only has 2 days" according to R's logic
promo[yearweekid=="201552",yearweekid:="201600"]

unique(promo$vehicle)
unique(promo$prod_id)

alpha_TV = 1 - 0.5 ** (1 / 8) # calculate alpha of TV commercial based on TV half-life

wkdata_1 <- wkdata[prod_id==SMbeer[1]]
wkdata_2 <- wkdata[prod_id==SMbeer[2]]
wkdata_3 <- wkdata[prod_id==SMbeer[3]]

skipweek <- setdiff(wkdata_1$yearweekid, wkdata_3$yearweekid)

# product 1 and 2 sell everyday, while product 3 skip few days, which would generate bugs when using shift, will take care when dealing with it.

promo_1 <- promo[prod_id==SMbeer[1]]
promo_2 <- promo[prod_id==SMbeer[2]]
promo_3 <- promo[prod_id==SMbeer[3]]


# model data of product 1
wkmodel <- left_join(wkdata_1,promo_1[vehicle=="TV",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel,"amount","TVGRP")
wkmodel <-as.data.table(wkmodel)
wkmodel[is.na(TVGRP),TVGRP:=0]
wkmodel$TVadstock=0
for(i in 2:nrow(wkmodel)){
  wkmodel$TVadstock[i]=alpha_TV*wkmodel$TVGRP[i]+(1-alpha_TV)*wkmodel$TVadstock[i-1]
}
wkmodel[,TVreach:=0.95 * (1 - exp(-0.02 * TVadstock))]

alpha_Radio = 1 - 0.5 ** (1 / 4)
wkmodel <- left_join(wkmodel,promo_1[vehicle=="Radio",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel,"amount","RadioGRP")
wkmodel <-as.data.table(wkmodel)
wkmodel[is.na(RadioGRP),RadioGRP:=0]
wkmodel$Radioadstock=0
for(i in 2:nrow(wkmodel)){
  wkmodel$Radioadstock[i]=alpha_Radio*wkmodel$RadioGRP[i]+(1-alpha_Radio)*wkmodel$Radioadstock[i-1]
}
wkmodel[,Radioreach:=0.9 * (1 - exp(-0.025 * Radioadstock))]

wkmodel <- left_join(wkmodel,promo_1[vehicle=="Paid Search",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel,"amount","PaidSearch")

wkmodel <- left_join(wkmodel,promo_1[vehicle=="Web Display",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel,"amount","WebDisplay")
wkmodel <- as.data.table(wkmodel)
wkmodel[is.na(WebDisplay),WebDisplay:=0]

wkmodel <- left_join(wkmodel,promo_1[vehicle=="Email",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel,"amount","Email")
wkmodel <- as.data.table(wkmodel)
wkmodel[is.na(Email),Email:=0]

wkmodel <- left_join(wkmodel,promo_1[vehicle=="Flyer",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel,"amount","Flyer")
wkmodel <- as.data.table(wkmodel)
wkmodel[is.na(Flyer),Flyer:=0]

wkmodel <- left_join(wkmodel,season[,c("yearweekid","seas_index")],by="yearweekid")

wkmodel <- as.data.table(wkmodel)
wkmodel[yearweekid%in%holiday$yearweekid,is_hol:=1]
wkmodel[is.na(is_hol),is_hol:=0]

importanthol <- holiday[holiday%in%c("XMAS","PrXMAS","NEWYEAR")]$yearweekid
wkmodel[yearweekid%in%importanthol,is_imphol:=1]
wkmodel[is.na(is_imphol),is_imphol:=0]



fwrite(wkmodel,"modeldata_138936951.csv")


# model data of product 2
wkmodel2 <- left_join(wkdata_2,promo_2[vehicle=="TV",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel2,"amount","TVGRP")
wkmodel2 <-as.data.table(wkmodel2)
wkmodel2[is.na(TVGRP),TVGRP:=0]
wkmodel2$TVadstock=0
for(i in 2:nrow(wkmodel2)){
  wkmodel2$TVadstock[i]=alpha_TV*wkmodel2$TVGRP[i]+(1-alpha_TV)*wkmodel2$TVadstock[i-1]
}
wkmodel2[,TVreach:=0.95 * (1 - exp(-0.02 * TVadstock))]

alpha_Radio = 1 - 0.5 ** (1 / 4)
wkmodel2 <- left_join(wkmodel2,promo_2[vehicle=="Radio",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel2,"amount","RadioGRP")
wkmodel2 <-as.data.table(wkmodel2)
wkmodel2[is.na(RadioGRP),RadioGRP:=0]

wkmodel2$Radioadstock=0
for(i in 2:nrow(wkmodel2)){
  wkmodel2$Radioadstock[i]=alpha_Radio*wkmodel2$RadioGRP[i]+(1-alpha_Radio)*wkmodel2$Radioadstock[i-1]
}
wkmodel2[,Radioreach:=0.9 * (1 - exp(-0.025 * Radioadstock))]

wkmodel2 <- left_join(wkmodel2,promo_2[vehicle=="Paid Search",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel2,"amount","PaidSearch")

wkmodel2 <- left_join(wkmodel2,promo_2[vehicle=="Web Display",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel2,"amount","WebDisplay")
wkmodel2 <- as.data.table(wkmodel2)
wkmodel2[is.na(WebDisplay),WebDisplay:=0]

wkmodel2 <- left_join(wkmodel2,promo_2[vehicle=="Email",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel2,"amount","Email")
wkmodel2 <- as.data.table(wkmodel2)
wkmodel2[is.na(Email),Email:=0]

wkmodel2 <- left_join(wkmodel2,promo_2[vehicle=="Flyer",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel2,"amount","Flyer")
wkmodel2 <- as.data.table(wkmodel2)
wkmodel2[is.na(Flyer),Flyer:=0]

wkmodel2 <- left_join(wkmodel2,season[,c("yearweekid","seas_index")],by="yearweekid")

wkmodel2 <- as.data.table(wkmodel2)
wkmodel2[yearweekid%in%holiday$yearweekid,is_hol:=1]
wkmodel2[is.na(is_hol),is_hol:=0]

importanthol <- holiday[holiday%in%c("XMAS","PrXMAS","NEWYEAR")]$yearweekid
wkmodel2[yearweekid%in%importanthol,is_imphol:=1]
wkmodel2[is.na(is_imphol),is_imphol:=0]

wkmodel2 <- left_join(wkmodel2,promo_2[vehicle=="Store Display",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel2,"amount","whether_storedisplay")
wkmodel2 <- as.data.table(wkmodel2)
wkmodel2[is.na(whether_storedisplay),whether_storedisplay:=0]

fwrite(wkmodel2,"modeldata_138936952.csv")

# model data of product 3
fill <- data.frame(matrix(ncol = 10, nrow = 4))
names(fill) <- names(wkdata_3)
fill$prod_id <- SMbeer[3]
fill$yearweekid <- skipweek
fill$weekid <- str_sub(fill$yearweekid,-2,-1)
fill[is.na(fill)] <- 0

wkdata_3 <- rbind(wkdata_3,fill)
setorder(wkdata_3,yearweekid)

wkmodel3 <- left_join(wkdata_3,promo_3[vehicle=="TV",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel3,"amount","TVGRP")
wkmodel3 <-as.data.table(wkmodel3)
wkmodel3[is.na(TVGRP),TVGRP:=0]
wkmodel3$TVadstock=0
for(i in 2:nrow(wkmodel3)){
  wkmodel3$TVadstock[i]=alpha_TV*wkmodel3$TVGRP[i]+(1-alpha_TV)*wkmodel3$TVadstock[i-1]
}
wkmodel3[,TVreach:=0.95 * (1 - exp(-0.02 * TVadstock))]

alpha_Radio = 1 - 0.5 ** (1 / 4)
wkmodel3 <- left_join(wkmodel3,promo_3[vehicle=="Radio",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel3,"amount","RadioGRP")
wkmodel3 <-as.data.table(wkmodel3)
wkmodel3[is.na(RadioGRP),RadioGRP:=0]
wkmodel3$Radioadstock=0
for(i in 2:nrow(wkmodel3)){
  wkmodel3$Radioadstock[i]=alpha_Radio*wkmodel3$RadioGRP[i]+(1-alpha_Radio)*wkmodel3$Radioadstock[i-1]
}
wkmodel3[,Radioreach:=0.9 * (1 - exp(-0.025 * Radioadstock))]

wkmodel3 <- left_join(wkmodel3,promo_3[vehicle=="Paid Search",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel3,"amount","PaidSearch")

wkmodel3 <- left_join(wkmodel3,promo_3[vehicle=="Web Display",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel3,"amount","WebDisplay")
wkmodel3 <- as.data.table(wkmodel3)
wkmodel3[is.na(WebDisplay),WebDisplay:=0]

wkmodel3 <- left_join(wkmodel3,promo_3[vehicle=="Email",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel3,"amount","Email")
wkmodel3 <- as.data.table(wkmodel3)
wkmodel3[is.na(Email),Email:=0]

wkmodel3 <- left_join(wkmodel3,promo_3[vehicle=="Flyer",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel3,"amount","Flyer")
wkmodel3 <- as.data.table(wkmodel3)
wkmodel3[is.na(Flyer),Flyer:=0]

wkmodel3 <- left_join(wkmodel3,season[,c("yearweekid","seas_index")],by="yearweekid")

wkmodel3 <- as.data.table(wkmodel3)
wkmodel3[yearweekid%in%holiday$yearweekid,is_hol:=1]
wkmodel3[is.na(is_hol),is_hol:=0]

importanthol <- holiday[holiday%in%c("XMAS","PrXMAS","NEWYEAR")]$yearweekid
wkmodel3[yearweekid%in%importanthol,is_imphol:=1]
wkmodel3[is.na(is_imphol),is_imphol:=0]

wkmodel3 <- wkmodel3[!yearweekid%in%skipweek]

wkmodel3 <- left_join(wkmodel3,promo_3[vehicle=="Store Display",c("prod_id","yearweekid","amount")],by=c("prod_id","yearweekid"))
setnames(wkmodel3,"amount","whether_storedisplay")
wkmodel3 <- as.data.table(wkmodel3)
wkmodel3[is.na(whether_storedisplay),whether_storedisplay:=0]

fwrite(wkmodel3,"modeldata_138936953.csv")
