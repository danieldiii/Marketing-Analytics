library(data.table)
library(dplyr)
library(psych)
library(arules)

trans <- fread("all_transaction.csv")
prod <- fread("product_table.csv")

########### construct a product-store table #################

# the table works as a potential product-store combination pool
comb <- unique(trans[,.(prod_id,store_id,category_desc_eng)],by=c("prod_id","store_id"))
uniqueN(comb$prod_id) # unique product: 10755
uniqueN(comb$store_id)# unique store: 421
uniqueN(comb$category_desc_eng) # unique category: 418

# exclude fresh category
fresh_products <- unique(prod[prod$category_desc_eng %like% "FRESH "]$category_desc_eng)
comb[,whetherfresh:=fifelse(category_desc_eng%in%fresh_products,1,0)]
comb <- comb[whetherfresh==0]
comb[,whetherfresh:=NULL]

# exclude the products that are sold in less than 10 stores
comb[,storecnt:=.N,by=prod_id]
comb <- comb[storecnt>=10]

# only keep the stores with 200+ products
comb[,prodcnt:=.N,by=store_id]
comb <- comb[prodcnt>=200]

# only keep the stores with top 2 categories having more than 150 products
comb[,top2prodcnt:=sum(sort(table(category_desc_eng),decreasing=T)[c(1,2)]),by=store_id]
comb <- comb[top2prodcnt>=150]

# only keep the categories with 15+ products sold in 10+ stores
comb[,storecnt3:=sum(table(prod_id)>=15),by=category_desc_eng]
comb <- comb[storecnt3>=10]

# exclude those std dev of price is 0
trans[,pricesd:=sd(prod_unit_price),by=c("prod_id","store_id")]
psd <- unique(trans[,.(prod_id,store_id,pricesd)], by=c("prod_id", "store_id"))
comb <- left_join(comb,psd,by=c("prod_id","store_id"))
comb <- as.data.table(comb)
comb <- comb[pricesd>0,]
rm(psd)

# exclude those combinations with less than 10 weekly observations
trans[,weekid:=strftime(tran_dt,format = "%V")]
trans[,yearweekid:=strftime(tran_dt,format = "%Y%V")]
week = data.table(unique(trans[, tran_dt]))
colnames(week) <- 'tran_dt'
week[, yearweekid := strftime(tran_dt, format = "%Y%V")]
week = week[order(tran_dt)]
week = week[3:dim(week)[1], ] # there are only two days ('2016-01-01' & '2016-01-02') in the last week of 2015, deleted
week[, weekid := as.integer(strftime(tran_dt, format = "%V"))]
trans = merge(trans, week, by = 'tran_dt')
trans[,weekcnt:=uniqueN(weekid),by=c("prod_id","store_id")]
fwrite(trans,"trans.csv")
wcnt <- unique(trans[,.(prod_id,store_id,weekcnt)], by=c("prod_id", "store_id"))
comb <- left_join(comb,wcnt,by=c("prod_id","store_id"))
comb <- as.data.table(comb)
comb <- comb[weekcnt>=10,]

# exclude the categories inside the combs of categories with less than 10 stores
catecomb <- fread("catecomb.csv")
catecomb <- expand.grid(unique(comb$category_desc_eng),unique(comb$category_desc_eng))
catecomb <- as.data.table(catecomb)
catecomb <- catecomb[Var1!=Var2] 
catecomb[,sharedstore:=length(intersect(comb[category_desc_eng==Var1,store_id],comb[category_desc_eng==Var2,store_id])),by=c("Var1","Var2")]
catecomb <- catecomb[sharedstore>=10]
comb <- comb[category_desc_eng%in%unique(catecomb$Var1)]

# exclude the categories not inside the combs of categories with more than 120 products in 10 stores
catecomb[,sharedprod:=sum(table(comb[category_desc_eng==Var1|category_desc_eng==Var2,prod_id])>=120),by=c("Var1","Var2")]
catecomb <- catecomb[sharedprod>=10]
comb <- comb[category_desc_eng%in%unique(catecomb$Var1)]

# exclude the products that are sold in less than 10 stores
comb2 <- comb[,storecnt:=.N,by=prod_id]
comb2 <- comb2[storecnt>=10]

# only keep the stores with 100+ products
comb2[,prodcnt:=.N,by=store_id]
comb2 <- comb2[prodcnt>=100]

# only keep the stores with top 2 categories having more than 100 products
comb2[,top2prodcnt:=sum(sort(table(category_desc_eng),decreasing=T)[c(1,2)]),by=store_id]
comb2 <- comb2[top2prodcnt>=100]
#comb <- comb2

# only keep the categories with 10+ products
comb2[,storecnt4:=.N,by=category_desc_eng]
comb3 <- comb2[storecnt4>=10]

uniqueN(comb3$prod_id) # unique product: 2197
unique(comb3$store_id)# unique store: 10
uniqueN(comb3$category_desc_eng) # unique category: 140
# 13420 combinations left
fwrite(comb3,"comb3.csv")

# by this step, we have already decided the 10 stores. Then we should predict the revenue for each combination of product-store 

########### find the optimal price for each comb of product and store #################

# build the training set

modeldata <- comb3[,c(1,2,3)]
# holiday effects

por_na_holidays = c('2016-01-01', '2016-03-25', '2016-04-25', '2016-05-26', '2016-06-10', '2016-08-15', '2016-10-05', '2016-11-01', '2016-12-01', '2016-12-08', '2016-12-25', '2017-01-01', '2017-04-14', '2017-04-16', '2017-04-25', '2017-05-01', '2017-06-10', '2017-06-15', '2017-08-15', '2017-10-05', '2017-11-01', '2017-12-01', '2017-12-08', '2017-12-25')
trans[tran_dt %in% por_na_holidays, tran_hol := 1]
trans[is.na(tran_hol), tran_hol := 0]

trans[, tran_wk_hol := max(tran_hol), by = yearweekid]

# weekly seasonality index

# add wkly_volume, wkly_price, wkly_dct

wkly_data <- trans[,list(wkly_volume = sum(tran_prod_sale_qty),wkly_price = sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty), wkly_dct = -sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty)),by = c('store_id', 'prod_id', 'yearweekid','tran_wk_hol','weekid')]
wkly_data <- left_join(modeldata,wkly_data,by=c("store_id","prod_id"))
wkly_data <- left_join(wkly_data,wcnt,by=c("prod_id","store_id"))
wkly_data$tran_wk_hol <- as.factor(wkly_data$tran_wk_hol)
wkly_data <- as.data.table(wkly_data)
wkly_data[, max_wkly_volume := max(wkly_volume) * 1.1, by = .(store_id, prod_id)] # Assumption: 10% more than historical maximum
wkly_data[, T_volume := log(wkly_volume/(max_wkly_volume - wkly_volume))]
wkly_data$weekid <- as.factor(wkly_data$weekid)

fwrite(wkly_data,"wkly_data.csv") # training data

# build the test set

### check the week of April 13-19, 2017

### build test set
#benchmark <- trans[tran_dt%in%c("2017-04-13","2017-04-14","2017-04-15","2017-04-16","2017-04-17","2017-04-18","2017-04-19")]
#benchmark <- benchmark[,wkly_dct := -sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty),by = c('store_id', 'prod_id')]
benchmark2 <- trans[,listprice := sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty),by = c('store_id', 'prod_id')]
benchmark2 <- unique(benchmark2[,c("prod_id","store_id","listprice")],by=c("prod_id","store_id"))
# find that that week have no discount at all

testdata <- wkly_data
testdata[,yearweekid:=NULL]
strftime(c("2020-04-13","2020-04-19"),format = "%V") #weekid should be 16
testdata[,weekid:="16"]
testdata[,tran_wk_hol:=0]
testdata$tran_wk_hol <- as.factor(testdata$tran_wk_hol)
testdata$weekid <- as.factor(testdata$weekid)
testdata[,wkly_volume:=NULL]
testdata2 <- left_join(testdata,benchmark2,by=c("prod_id","store_id"))
testdata <- as.data.table(testdata2)
testdata[,"wkly_dct":=0]
testdata[,wkly_price:=NULL]
testdata <- unique(testdata)
testdata[, unique_id := do.call(paste, c(.SD, sep = "_")), .SDcols = c('store_id', 'prod_id')]
fwrite(testdata,"testdata.csv") # test set sample

testdata <- fread("testdata.csv")
testdata2 <- fread("testdata.csv")
for(i in seq(0.5, 2, 0.1)){
  newtest <- testdata2[,listprice:=i*listprice,by=c("prod_id","store_id")]
  testdata2 <- fread("testdata.csv")
  testdata <- rbind(testdata,newtest)
}

names(testdata)[8] <- "wkly_price"

testdata2 <- unique(testdata)
testdata2$T_volume=0
testdata2$tran_wk_hol <- as.factor(testdata2$tran_wk_hol)

fwrite(testdata2,"testdata2.csv")

###### sub and comp part #########

##This file aims to find substitute and complimentary goods for each pre target goods

#read in transaction history
all_tran <- fread("all_transaction.csv")
## transform into transaction object class
all_tran_list <- as.data.frame(all_tran[,c("newid","prod_id")])
all_tran_list2 <- as(split(all_tran_list[,"prod_id"], all_tran_list[,"newid"]), "transactions")
length(unique(all_tran_list$newid)) #2829795 unique trans
length(unique(all_tran_list$prod_id)) #10755 unique products


################substitute#######################
# construct rules, only consider relationship between two items
# need to set minlen=2, maxlen=2

rules = apriori(all_tran_list2, parameter = list(supp=0, conf=0, minlen=2, maxlen=2))

#get subset only related to our project
comb <- unique(fread("comb.csv")$prod_id)
test <- unique(fread("testdata.csv")$prod_id)
pre_target_list <- unique(append(comb,test)) #same 2168 products in test and comb
pre_target_list <- as.character(pre_target_list)
sub_rules <- subset(rules, items %in% pre_target_list)
inspect(sort(sub_rules, by = "lift",decreasing = FALSE)[1:5])

save(sub_rules, file = "sub_rule.RData")

#clean workspace

#transform to table
rules_df <- as(sub_rules, "data.frame") 
fwrite(rules_df,"sub_rules_df.csv")

rules_df <- as.data.table(rules_df) #41931288 rules
sub <- rules_df[lift<1] #21046410 rules
sub <- sub[lift>0] #4176378 rules

#View(sub[1:10,])
sub$rules <- as.character(sub$rules)
library(stringr)
prodlist <- str_split_fixed(sub$rules, "=", 2)
prodlist <- as.data.table(prodlist)
sub_table <- cbind(prodlist,sub[,c(2:5)])
colnames(sub_table)[1:2] <- c("prodA","prodB")
sub_table$prodA <- gsub("[^0-9.]", "",  sub_table$prodA)
sub_table$prodB <- gsub("[^0-9.]", "",  sub_table$prodB)
fwrite(sub_table,"sub_table.csv")

#only keep record with pre target product act as prodB 
sub_table2 <- sub_table[prodB %in% pre_target_list] #2509576 rules
length(unique(sub_table2$prodB)) #2168 products with sub rules
fwrite(sub_table2,"sub_table2.csv")


########complimentary####################
# set supp as 0.001, conf as 0.25 -only 326 rules
# set supp as 0.0001, conf as 0.25 -2437 rules
# set supp as 0.00005, conf as 0.25 -3336 rules
# set supp as 0.0001, conf as 0.2 -4619 rules
# set supp as 0.00005, conf as 0.2 -6619 rules
rules = apriori(all_tran_list2, parameter = list(supp=0.00005, conf=0.2, minlen=2, maxlen=2))

#get subset only related to our project
comp_rules <- subset(rules, items %in% pre_target_list) #2329 rules

#transform to table
comp_rules_df <- as(comp_rules, "data.frame") 
fwrite(comp_rules_df,"comp_rules_df.csv")

comp_rules_df <- as.data.table(comp_rules_df) #2329 rules
comp <- comp_rules_df[lift>1] #2329 rules

comp$rules <- as.character(comp$rules)
prodlist <- str_split_fixed(comp$rules, "=", 2)
prodlist <- as.data.table(prodlist)
comp_table <- cbind(prodlist,comp[,c(2:5)])
colnames(comp_table)[1:2] <- c("prodA","prodB")
comp_table$prodA <- gsub("[^0-9.]", "",  comp_table$prodA)
comp_table$prodB <- gsub("[^0-9.]", "",  comp_table$prodB)
fwrite(comp_table,"comp_table.csv")

#only keep record with pre target product act as prodB 
comp_table2 <- comp_table[prodB %in% pre_target_list] #334 rules
length(unique(comp_table2$prodB)) #198 products with comp rules
fwrite(comp_table2,"comp_table2.csv")


###########################get sub and comp price for each product-store-week / product-store#########

sub <- fread("sub_result.csv") 
comp <- fread("comp_result.csv")
wkly_data <- fread("wkly_data.csv")
testdata <- fread("testdata.csv")
traindata <- fread("wkly_data.csv")

all_tran <- fread("all_transaction.csv")
all_tran[,weekid:=strftime(tran_dt,format = "%V")]
all_tran[,yearweekid:=strftime(tran_dt,format = "%Y%V")]
weekly_data <- all_tran %>%
  group_by(prod_id,store_id,yearweekid) %>%
  summarise(weekly_list_price=sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty),weekly_paid_price=sum(tran_prod_paid_amt)/sum(tran_prod_sale_qty))
weekly_data$yearweekid <- as.integer(weekly_data$yearweekid)

fwrite(weekly_data,"weekly_data.csv")

hist_data <- all_tran %>%
  group_by(prod_id,store_id) %>%
  summarise(list_price=sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty),paid_price=sum(tran_prod_paid_amt)/sum(tran_prod_sale_qty))
fwrite(hist_data,"hist_data.csv")


######add to the training and test set##################
comp <- fread("comp_table2.csv")
sub <- fread("sub_table2.csv")
testdata <- fread("testdata.csv")
traindata <- fread("wkly_data.csv")
weekly_data <- fread("weekly_data.csv")
hist_data <- fread("hist_data.csv")
#######train#####

###add comp
traindatac <- full_join(traindata,comp[,c(1,2,5)],by=c("prod_id"="prodB"))
traindatac <- inner_join(traindatac,weekly_data,by=c("prodA"="prod_id","store_id"="store_id","yearweekid"="yearweekid"))
colnames(traindatac)[9:12] <- c("comp","lift","complistprice","compsaleprice")
traindatac <- traindatac %>% 
  group_by(prod_id,store_id,yearweekid) %>% 
  filter(lift == max(lift))
traindatac1 <- traindatac %>% 
  arrange(desc(lift)) %>% 
  group_by(prod_id,store_id,yearweekid) %>% 
  slice(1)
fwrite(traindatac,"traindatac.csv")
#####30758 out of 517102 has comp data

###add sub
#can join this large table
#traindatas <- full_join(traindata,sub[,c(1,2,5)],by=c("prod_id"="prodB"))
a<-sub %>%
  group_by(prodB)%>%
  summarise(n=n())

sub_top20 <- sub %>% 
  arrange(lift) %>% 
  group_by(prodB) %>% 
  slice(1:100)
fwrite(sub_top20,"sub_top20.csv")
sub_top20 <- fread("sub_top20.csv")
traindatas <- full_join(traindata,sub_top20[,c(1,2,5)],by=c("prod_id"="prodB"))
traindatas <- inner_join(traindatas,weekly_data,by=c("prodA"="prod_id","store_id"="store_id","yearweekid"="yearweekid"))
colnames(traindatas)[9:12] <- c("sub","lift","sublistprice","subsaleprice")
traindatas <- traindatas %>% 
  group_by(prod_id,store_id,yearweekid) %>% 
  filter(lift == min(lift))
traindatas1 <- traindatas %>% 
  arrange(lift) %>% 
  group_by(prod_id,store_id,yearweekid) %>% 
  slice(1)
#####509605 out of 517102 has sub data
traindatac <- fread("traindatac.csv")

traindata_v2 <- left_join(traindata,traindatas1[,c(1,2,3,9,10,11,12)],by=c("prod_id"="prod_id","store_id"="store_id","yearweekid"="yearweekid"))
traindata_v2 <- left_join(traindata_v2,traindatac1[,c(1,2,3,9,10,11,12)],by=c("prod_id"="prod_id","store_id"="store_id","yearweekid"="yearweekid"))

fwrite(traindata_v2,"traindatav2.csv")

##############test
uniquetest<-unique(testdata[,c(1:2)]) #only include 21178 combination
testdatas <- full_join(uniquetest,sub_top20[,c(1,2,5)],by=c("prod_id"="prodB"))
testdatas <- inner_join(testdatas,hist_data,by=c("prodA"="prod_id","store_id"="store_id"))
colnames(testdatas)[3:6] <- c("sub","lift","sublistprice","subsaleprice")
testdatas <- testdatas %>% 
  group_by(prod_id,store_id) %>% 
  filter(lift == min(lift))
testdatas1 <- testdatas %>% 
  arrange(lift) %>% 
  group_by(prod_id,store_id) %>% 
  slice(1)
#####21178 out of 21178 has sub data


testdatac <- full_join(uniquetest,comp[,c(1,2,5)],by=c("prod_id"="prodB"))
testdatac <- inner_join(testdatac,hist_data,by=c("prodA"="prod_id","store_id"="store_id"))
colnames(testdatac)[3:6] <- c("comp","lift","complistprice","compsaleprice")
testdatac <- testdatac %>% 
  group_by(prod_id,store_id) %>% 
  filter(lift == max(lift))
testdatac1 <- testdatac %>% 
  arrange(desc(lift)) %>% 
  group_by(prod_id,store_id) %>% 
  slice(1)
#####1890 out of 21178 has comp data
testdata_v2 <- left_join(uniquetest,testdatas1,by=c("prod_id"="prod_id","store_id"="store_id"))
testdata_v2 <- left_join(testdata_v2,testdatac1,by=c("prod_id"="prod_id","store_id"="store_id"))

fwrite(testdata_v2,"testdatav2.csv")


###### sub and comp part end ########

# combine the traning and test set with sub and comp info

# combine to test
testdatav1 <- fread("testdatav1.csv")
testdatav1 <- unique(testdatav1)
testdatav1 <- testdatav1[,c(1,2,9,12)]
testdatav1 <- unique(testdatav1)
testdatav2 <- fread("testdatav2.csv")
testdatav2 <- testdatav2[,c(1,2,5,9)]
testdatav2 <- unique(testdatav2)
testdatav2_addon <- fread("testdatav2_addon.csv")
testdatav2_addon <- testdatav2_addon[,c(1,2,5,9)]
testdatav2 <- rbind(testdatav2,testdatav2_addon)

testdata_sc <- left_join(testdata2,testdatav2,by=c("prod_id","store_id"))
testdata_sc <- as.data.table(testdata_sc)
sum(is.na(testdata_sc$sublistprice)) # all the test data have sublistprice
testdata_na <- testdata_sc[is.na(complistprice)] # comp is na
testdata_nonna <- testdata_sc[!(is.na(complistprice))] # comp is not na

# combine to train
traindatav2 <- fread("traindatav2.csv")
traindatav2 <- traindatav2[,c(1,2,3,11,15)]
traindatav2[, unique_id := do.call(paste, c(.SD, sep = "_")), .SDcols = c('store_id', 'prod_id')]
traindatav2 <- traindatav2[,c(3,4,5,6)]
#wkly_data_sc <- left_join(wkly_data,traindatav2,by=c("prod_id","store_id","yearweekid"))
traindata_sc <- as.data.table(wkly_data_sc)
traindata_na1 <- unique(traindata_sc[is.na(sublistprice)])
traindatav2_addon <- fread("traindatav2_addon.csv")
traindatav2_addon <- traindatav2_addon[,c(1,2,3,6,10)]

traindatav2_addon[, unique_id := do.call(paste, c(.SD, sep = "_")), .SDcols = c('store_id', 'prod_id')]
traindatav2_addon <- traindatav2_addon[,c(3,4,5,6)]
traindata_add <- rbind(traindatav2,traindatav2_addon)
traindata_final <- left_join(wkly_data,traindata_add,by=c("unique_id","yearweekid"))

### final test set: testdata_sc, final train set: traindata_final

testdata_nonna <- testdata_nonna[!unique_id%in%traindata_na$unique_id]
testdata_nonna$weekid <- as.factor(testdata_nonna$weekid)

traindata_final <- as.data.table(traindata_final)
sum(is.na(traindata_final[unique_id%in%testdata_nonna$unique_id,sublistprice]))

### model for both comp and sub exist

traindata1 <- traindata_final[unique_id%in%testdata_nonna$unique_id]
traindata1[,meancomplistprice:=mean(complistprice,na.rm=TRUE),by=unique_id]
traindata1[is.na(complistprice),complistprice:=meancomplistprice]
names(testdata_nonna)[9]<- "uid"
traindata1[is.na(complistprice),complistprice:=testdata_nonna[uid==unique_id,complistprice][1],by="unique_id"]
names(testdata_nonna)[9]<- "unique_id"
traindata1[,meansublistprice:=mean(sublistprice,na.rm=TRUE),by=unique_id]
traindata1[is.na(sublistprice),sublistprice:=meansublistprice]


for (id in unique(testdata_nonna$unique_id)){
  model <- traindata1[unique_id == id]
  if (0%in%model$tran_wk_hol&16%in%model$weekid){
    lm <- lm(T_volume ~ wkly_price + wkly_dct  + tran_wk_hol + weekid + sublistprice + complistprice, data = model)
  }else if(0%in%model$tran_wk_hol&!16%in%model$weekid){
    lm <- lm(T_volume ~ wkly_price + wkly_dct + tran_wk_hol + sublistprice + complistprice, data = model)
  }else if(!0%in%model$tran_wk_hol&16%in%model$weekid){
    lm <- lm(T_volume ~ wkly_price + wkly_dct + weekid + sublistprice + complistprice, data = model)
  }else{
    lm <- lm(T_volume ~ wkly_price + wkly_dct + sublistprice + complistprice, data = model)
  }  
  newdata = testdata_nonna[unique_id == id]
  for (i in 1:nrow(testdata_nonna[unique_id==id])){
    testdata_nonna[unique_id == id, c('T_volume')][i] <- predict(lm, newdata = newdata[i,])
  }
}

### model for only sub exist

traindata2 <- traindata_final[unique_id%in%testdata_na$unique_id]
sum(is.na(traindata2$sublistprice))
traindata2[,meansublistprice:=mean(sublistprice,na.rm=TRUE),by=unique_id]
traindata2[is.na(sublistprice),sublistprice:=meansublistprice]
sum(is.na(traindata2$sublistprice))

testdata_na$weekid <- as.factor(testdata_na$weekid)


for (id in unique(testdata_na$unique_id)){
  model <- traindata2[unique_id == id]
  if (0%in%model$tran_wk_hol&16%in%model$weekid){
    lm <- lm(T_volume ~ wkly_price + wkly_dct  + tran_wk_hol + weekid + sublistprice, data = model)
  }else if(0%in%model$tran_wk_hol&!16%in%model$weekid){
    lm <- lm(T_volume ~ wkly_price + wkly_dct + tran_wk_hol + sublistprice, data = model)
  }else if(!0%in%model$tran_wk_hol&16%in%model$weekid){
    lm <- lm(T_volume ~ wkly_price + wkly_dct + weekid + sublistprice, data = model)
  }else{
    lm <- lm(T_volume ~ wkly_price + wkly_dct + sublistprice, data = model)
  }  
  newdata = testdata_na[unique_id == id]
  for (i in 1:nrow(testdata_na[unique_id==id])){
    testdata_na[unique_id == id, c('T_volume')][i] <- predict(lm, newdata = newdata[i,])
  }
}

testdata_final <- rbind(testdata_na,testdata_nonna)
testdata_final <- left_join(testdata_final,unique(wkly_data_sc[,c("unique_id","max_wkly_volume")]),by="unique_id")
testdata_final <- as.data.table(testdata_final)
testdata_final[, est_volume := max_wkly_volume * exp(T_volume) / (exp(T_volume) + 1)] # refer to the formula

testdata_final[, revenue := (wkly_price - wkly_dct) * est_volume]

fwrite(testdata_final,"testdata_final.csv")

########### find the optimal 2 categories and optimal products #################

testdata_final1 <- testdata_final[revenue<1000000]
testdata_final1 <- testdata_final1[revenue>0]

# first step, choose potential categories

testdata_final1[,caterev:=mean(revenue),by="category_desc_eng"]
cateorder <- unique(testdata_final1[,c("category_desc_eng","caterev")],by=c("category_desc_eng","caterev"))
setorder(cateorder,-caterev)
barplot(cateorder$caterev,xlab="category",ylab="mean revenue",main="mean revenue of categories")

testdata_final1[,bestrev:=max(revenue),by=unique_id]
testdata_final1[revenue==bestrev,bestlistprice:=wkly_price]
testdata_final2 <- testdata_final1[!is.na(bestlistprice)]

benchmark <- fread("testdata.csv")
names(benchmark)[8]="wkly_price"
benchmark <- left_join(benchmark,testdata_final1[,c(8:18)],by=c("wkly_price","unique_id"))

compare <- left_join(testdata_final2,benchmark[,c(8,9,14,15)],by="unique_id")
compare <- as.data.table(compare)
compare[,revincre:=revenue.x-revenue.y]

result <- as.data.table(unique(compare$category_desc_eng))
names(result) <- "category_desc_eng"
result$commonprod <- 0
result$meanrevenue <- 0

compare[,prodcnt:=uniqueN(prod_id),by=category_desc_eng]
prodcnt <- unique(compare[,c("category_desc_eng","prodcnt")])
# the categories with mostproducts: yogurt health, fine wafers, fine wines

for (i in 1:140){
  result[i,2] <- length(Reduce(intersect,  list(v1 = compare[category_desc_eng==result[i,1]&store_id==344,prod_id],
                                         v2 = compare[category_desc_eng==result[i,1]&store_id==343,prod_id],
                                         v3 = compare[category_desc_eng==result[i,1]&store_id==349,prod_id],
                                         v4 = compare[category_desc_eng==result[i,1]&store_id==341,prod_id],
                                         v5 = compare[category_desc_eng==result[i,1]&store_id==157,prod_id],
                                         v6 = compare[category_desc_eng==result[i,1]&store_id==331,prod_id],
                                         v7 = compare[category_desc_eng==result[i,1]&store_id==342,prod_id],
                                         v8 = compare[category_desc_eng==result[i,1]&store_id==346,prod_id],
                                         v9 = compare[category_desc_eng==result[i,1]&store_id==588,prod_id],
                                         v0 = compare[category_desc_eng==result[i,1]&store_id==525,prod_id]))
  )
}
# the categories with most common products across ten stores: yogurt health, apple, fine wafers

# choose yogurt health and fine wafers

testdata_final3 <- compare[category_desc_eng=="YOGURT HEALTH"|category_desc_eng=="FINE WAFERS"]
uniqueN(testdata_final3$prod_id) # unique product: 186
uniqueN(testdata_final3$store_id)# unique store: 10
uniqueN(testdata_final3$category_desc_eng) # unique category: 2
# need to choose 100 products out of the 186

#first i need to expand the 1181 combinations to 1860 ones, and then find the best 100 ones.
fullcomb <- expand.grid(unique(testdata_final3$prod_id),unique(testdata_final3$store_id))
names(fullcomb) <- c("prod_id","store_id")
fullcomb <- left_join(fullcomb,testdata_final3[,c(1,3)],by="prod_id")
fullcomb <- unique(fullcomb)
fullcomb <- left_join(fullcomb,testdata_final3[,c(1,2,8,14,15,19,20,21,22)],by=c("prod_id","store_id"))
fullcomb <- as.data.table(fullcomb)
needassum <- fullcomb[is.na(wkly_price.x)]
history <- trans[,               list(wkly_volume = sum(tran_prod_sale_qty),
                           
                           wkly_price = sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty),
                           
                           wkly_dct = -sum(tran_prod_discount_amt)/sum(tran_prod_sale_qty)),
      
      by = c('store_id', 'prod_id',"yearweekid")]

history <- history[,list(meanvolume=mean(wkly_volume),meanprice=mean(wkly_price)),by=c("store_id","prod_id")]

needassum2 <- left_join(needassum[,c(1,2,3)],history)
needassum <- as.data.table(needassum2)
needassum[,meanvolume2:=mean(meanvolume,na.rm=TRUE),by=prod_id]
needassum[is.na(meanvolume),meanvolume:=meanvolume2]
needassum[,meanprice2:=mean(meanprice,na.rm=TRUE),by=prod_id]
needassum[is.na(meanprice),meanprice:=meanprice2]
needassum[,revenue:=meanvolume*meanprice]
needassum[,meanvolume2:=NULL]
needassum[,meanprice2:=NULL]
names(needassum)[c(4,5,6)]<- c("est_volume.x","wkly_price.x","revenue.x")
needassum[,wkly_price.y:=wkly_price.x]
needassum[,est_volume.y:=est_volume.x]
needassum[,revenue.y:=revenue.x]
needassum$revincre=0
fullcomb2 <- rbind(fullcomb[!is.na(wkly_price.x)],needassum)

# find the best products
fullcomb2[,sumrevenue:=sum(revincre),by="prod_id"]

top100prod <- unique(fullcomb2[,c("prod_id","category_desc_eng","sumrevenue")])
setorder(top100prod,-sumrevenue)

top100prodresult <- top100prod[2:101,]
sum(top100prodresult$category_desc_eng=="YOGURT HEALTH")
sum(top100prodresult$category_desc_eng=="FINE WAFERS")

fwrite(top100prodresult,"top100prodresult.csv")

finalchoice <- fullcomb2[prod_id%in%top100prod$prod_id[2:101]] 

finalchoice[,storerev:=sum(revincre,na.rm = TRUE),by=store_id]
storerevincre <- unique(finalchoice[,c("store_id","storerev")])

fwrite(storerevincre,"storerevincre.csv")

finalchoice[,volumeincre:=est_volume.x-est_volume.y]
finalchoice[,storevol:=sum(volumeincre,na.rm = TRUE),by=store_id]
storevolincre <- unique(finalchoice[,c("store_id","storevol")])

fwrite(storevolincre,"storevolincre.csv")

fwrite(finalchoice,"finalchoice.csv")

trans <- fread("trans.csv")

finalchoice3 <- left_join(finalchoice,trans[,c("store_id","prod_id","unitcost")],by=c("store_id","prod_id"))
finalchoice3 <- as.data.table(finalchoice3)
finalchoice3[,meancost:=mean(unitcost),by=c("store_id","prod_id")]
finalchoice3[,unitcost:=NULL]
finalchoice3 <- unique(finalchoice3)
sum(is.na(finalchoice3$meancost))
finalchoice3[,prodmeancost:=mean(meancost,na.rm=TRUE),by="prod_id"]
finalchoice3[is.na(meancost),meancost:=prodmeancost]
finalchoice3[,prodmeancost:=NULL]
finalchoice3[,profit.x:=(wkly_price.x-meancost)*est_volume.x]
finalchoice3[,profit.y:=(wkly_price.y-meancost)*est_volume.y]
finalchoice3[,profitincre:=profit.x-profit.y]
sum(finalchoice3$profitincre) # 5000
fwrite(finalchoice3,"finalincre.csv")
finalchoice3[,storeprofit:=sum(profitincre,na.rm = TRUE),by=store_id]
storeproincre <- unique(finalchoice3[,c("store_id","storeprofit")])

fwrite(storeproincre,"storeproincre.csv")
