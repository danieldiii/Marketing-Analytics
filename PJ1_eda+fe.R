### mkt pj
# EDA + FE

########### import packages and data ###################

library(data.table)
library(dplyr)
library(psych)

options(scipen=999) # cancel Scientific notation

product <- read.csv("C:/Users/feifa/Desktop/mkt/pj1/product_table.csv")
transaction <- fread("C:/Users/feifa/Desktop/mkt/pj1/transaction_table.csv")

##### check out some weird stuff of the dataset ########

head(transaction)
head(product)
nrow(unique(transaction,by=c("cust_id","tran_id","prod_id","store_id")))
length(unique(transaction$cust_id))
sum(transaction$tran_prod_sale_qty*transaction$prod_unit_price!=transaction$tran_prod_sale_amt)
head(transaction[transaction$tran_prod_sale_qty*transaction$prod_unit_price!=transaction$tran_prod_sale_amt])
sum(transaction$tran_prod_sale_amt+transaction$tran_prod_discount_amt!=transaction$tran_prod_paid_amt)

################## data cleaning #######################

# delete those with <=0 sales
transaction <- transaction[tran_prod_paid_amt>0]
# delete 2 times 1 product
transaction <- unique(transaction,by=c("cust_id","tran_id","prod_id","store_id"))

################## data preparation ####################

# create unit after-disc price
transaction <- transaction[,unitdiscountedprice:=tran_prod_paid_amt/tran_prod_sale_qty]

# does not know about cost

# sol: check the dist of unit price; set a threshold: 2.49 dollars, below min=cost; above min = ? cost

# we found out that 80 percent of products are selling at 2.49 or lower
test <- transaction[order(unitdiscountedprice),]
test[0.8*nrow(test),"unitdiscountedprice"]   #2.49

# for those with a product discounted price of 2.49 or lower, we calculate their costs as their 
# minimum discounted price since we assume they will never sell anything at a lost
transaction <- transaction[unitdiscountedprice<=2.49,cost:=min(unitdiscountedprice),by=prod_id]

# for those that have a price higher than 2.47, we calculate their costs as the lowest between the minimum discounted price
# and the 70 percent of its average discounted price
transaction <- transaction[unitdiscountedprice>2.49,cost:=min(min(unitdiscountedprice),0.7*mean(unitdiscountedprice)),by=prod_id]

# calculate profits
transaction <- transaction[,profit:=unitdiscountedprice-cost]

####################### Q1 #############################

# build the table storing customer-level information

cust_info <- transaction[,revenue_sum:=sum(tran_prod_paid_amt),by=cust_id]
cust_info[,recorddays:=as.numeric(max(as.Date(tran_dt))-min(as.Date(tran_dt))),by=cust_id]
cust_info$recorddays <- as.numeric(cust_info$recorddays)
cust_info <- cust_info[,profit_sum:=sum(profit),by=cust_id]
cust_info <- cust_info[,visitcnt_sum:=nrow(unique(.SD,by=c("tran_id","store_id"))),by=cust_id]
cust_info[,visitfreq:=visitcnt_sum/recorddays]
cust_info <- cust_info[,prodcnteach:=length(prod_id),by=c("cust_id","tran_id","store_id")]
cust_info <- cust_info[,prodcnt_avg:=mean(prodcnteach),by=cust_id]
cust_info_summary <- unique(cust_info[,c("cust_id","revenue_sum","profit_sum","visitfreq","prodcnt_avg")])

fwrite(cust_info_summary,"customer_info_2.csv")

prod_tran <- left_join(transaction,product,by="prod_id")
prod_tran <- as.data.table(prod_tran)

####################### Q2&5 #############################

# attribute 1: KVI1, KVI2
# measurement: revenue, profit

# attribute 2: traffic driver: you don't target at buy it, but every time you buy other stuff, you tend to take some this
# measurement: transaction times

# attribute 3: promotion
# measurement: average promotion count*day/ days=latest-earlist


########## Product ##################

# What are the products and product groups with the best 
# volumes, revenues, profits, transactions, customers, etc.?

prod_tran <- left_join(transaction,product,by="prod_id")
prod_tran <- as.data.table(prod_tran)

#volume #two kinds: KG and Count
prod_info <- prod_tran[,volume:=sum(tran_prod_sale_qty),by="prod_id"]

#revenue
prod_info <- prod_info[,revenue:=sum(tran_prod_paid_amt),by="prod_id"]

#profit
prod_info <- prod_info[,profit:=sum(profit),by="prod_id"]

#transcation times
prod_info <- prod_info[,tran:=nrow(unique(.SD,by=c("tran_id","store_id","cust_id"))),by="prod_id"]

#unique customer count
prod_info <- prod_info[,customercnt:=length(unique(cust_id)),by="prod_id"]

#unique store count
prod_info <- prod_info[,storecnt:=length(unique(store_id)),by="prod_id"]

#day on
prod_info$tran_dt <- as.Date(prod_info$tran_dt)
prod_info <- prod_info[,firstday:=min(tran_dt),by="prod_id"]
prod_info <- prod_info[,lastday:=max(tran_dt),by="prod_id"]
prod_info <- prod_info[,dayon:=as.numeric(lastday-firstday),by="prod_id"]

#average discount rate
prod_info <- prod_info[,avgdiscount:=(1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt))),by="prod_id"]


prod_info_summary <- unique(prod_info[order(volume,decreasing = TRUE),
                                      c("prod_id","prod_unit","subcategory_id","category_id","brand_desc",
                                        "volume","revenue","tran","storecnt","customercnt",
                                        "dayon","profit","avgdiscount")])



#promotion

prodref <- prod_info %>%
  group_by(prod_id,tran_dt,store_id) %>%
  summarise(discount = max(tran_prod_offer_cts))

prodref$ifdiscount <- prodref$discount>0
prodref$ifdiscount <- as.numeric(prodref$ifdiscount)

prodref2 <- prodref %>%
  group_by(prod_id,tran_dt) %>%
  summarise(storecnt = sum(ifdiscount)/n())

prodref3 <- prodref2 %>%
  group_by(prod_id) %>%
  summarise(salecnt = sum(storecnt))

prod_info_summary <- left_join(prod_info_summary,prodref3,by="prod_id")
prod_info_summary$promotion <- prod_info_summary$salecnt/prod_info_summary$dayon

fwrite(prod_info_summary, file = "prod_info_summary2.csv", sep = ",")


########## Product Group/Category ##################


#revenue
cate_info <- prod_tran[,revenue:=sum(tran_prod_paid_amt),by="category_id"]

#profit
cate_info <- cate_info[,profit:=sum(profit),by="category_id"]

#transcation times
cate_info <- cate_info[,tran:=nrow(unique(.SD,by=c("tran_id","store_id","cust_id"))),by="category_id"]

#unique customer count
cate_info <- cate_info[,customercnt:=length(unique(cust_id)),by="category_id"]

#unique store count
cate_info <- cate_info[,storecnt:=length(unique(store_id)),by="category_id"]

cate_info_summary <- unique(cate_info[order(volume,decreasing = TRUE),
                                      c("category_id","category_desc_eng","revenue","tran","storecnt","customercnt","profit")])

fwrite(cate_info_summary, file = "cate_info_summary2.csv", sep = ",")


####################### Q3 #############################

store_info <- transaction[,volume:=sum(tran_prod_sale_qty),by="store_id"]
store_info <- store_info[,revenue:=sum(tran_prod_paid_amt),by="store_id"]
store_info <- store_info[,storevisit:=nrow(unique(.SD,by=c("tran_id","cust_id"))),by="store_id"]
store_info <- store_info[,customercnt:=length(unique(cust_id)),by="store_id"]
store_info_summary <- unique(store_info[order(volume,decreasing = TRUE),c("store_id","volume","revenue","storevisit","customercnt")])

####################### Q4 ############################

# customer

# attribute 1: valuable or cherry-picker : do you give a shit to discount
# measurement: the percentage of discounted items out of all items he bought
cust_seg <- prod_tran[,dispercent:=mean(tran_prod_offer_cts),by=cust_id]

# attribute 2: category loyalty
# measurement: the skewness / variance of his category portfolio
cust_seg <- cust_seg[,cateskew:=skew(as.numeric(table(category_id)),na.rm = TRUE),by=cust_id]

# attribute 3: store loyalty
# measurement: skewness of his store portfolio

cust_seg <- cust_seg[,storeskew:=skew(as.numeric(table(store_id)),na.rm = TRUE),by=cust_id]

cust_seg_summary <- unique(cust_seg[,c("cust_id","dispercent","cateskew","storeskew")])

# combine to the info_summary, forming the complete customer data
cust <- left_join(cust_info_summary,cust_seg_summary)

fwrite(cust,"cust.csv")

cust_label <- fread("cust_label.csv")

cust_label[,revenue_mean:=mean(revenue_sum),by=label]
cust_label[,profit_mean:=mean(profit_sum),by=label]

cust_label_summary <- unique(cust_label[,c("label","revenue_mean","profit_mean")])


######################## Q6 ############################

# attribute 1: visited by cherry-pickers or loyal customers
# measurement: in FE for Q4, you have a column which measures the customer the attribute of being a cherry-picker
# so as a store, you can calculate the mean extend of your customers
store_seg <- cust_seg[,cherry_picker_skew:=skew(dispercent),by=store_id]
hist(store_seg$cherry_picker_skew)   #the larger the skewness, the less frequent it is visited by cherry-pickers

# attribute 2: loyalty
# measurement: the skewness / variance of his store portfolio
store_seg <- store_seg[,loyalty_customer:=skew(as.numeric(table(store_id))),by=cust_id]
store_seg <- store_seg[,loyalty_skew:=skew(loyalty_customer),by=store_id]

store_seg_summary <- unique(cust_seg[,c("store_id","cherry_picker_skew","loyalty_skew")])

fwrite(store_seg_summary,'store_seg.csv')
