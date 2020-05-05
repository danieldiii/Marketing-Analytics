### mkt pj
# EDA + FE

library(data.table)
library(dplyr)
library(psych)
options(scipen=999) # cancel Scientific notation
product <- read.csv("C:/Users/sidi/Downloads/product_table.csv")
transaction <- fread("C:/Users/sidi/Downloads/transaction_table.csv")

##### check out some weird stuff of the dataset ########
#describe(transaction)

head(transaction)

head(product)

nrow(unique(transaction,by=c("cust_id","tran_id","prod_id","store_id")))

length(unique(transaction$cust_id))

sum(transaction$tran_prod_sale_qty*transaction$prod_unit_price!=transaction$tran_prod_sale_amt)

head(transaction[transaction$tran_prod_sale_qty*transaction$prod_unit_price!=transaction$tran_prod_sale_amt])

sum(transaction$tran_prod_sale_amt+transaction$tran_prod_discount_amt!=transaction$tran_prod_paid_amt)

########### need awful lot of data cleaning! #########

# delete those with <=0 sales
transaction <- transaction[tran_prod_paid_amt>0]
# 2 times 1 product
transaction <- unique(transaction,by=c("cust_id","tran_id","prod_id","store_id"))
#  unit after-disc price
transaction <- transaction[,unitdiscountedprice:=tran_prod_paid_amt/tran_prod_sale_qty]

####################### Q1 #############################

# does not know about cost

# sol: check the dist of unit price; set a threshold: x dollars, below cost = min; above cost = min(0.7*price,min)

hist(transaction$prod_unit_price,breaks=1000)

nrow(transaction[prod_unit_price<=5])/nrow(transaction)

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


###

cust_info <- transaction[,revenue:=sum(tran_prod_paid_amt),by=cust_id]
cust_info <- cust_info[,visitcnt:=nrow(unique(.SD,by=c("tran_id","store_id"))),by=cust_id]
#cust_info <- cust_info[,revisitcnt:=length(unique(tran_id)),by=c("cust_id","store_id")]
cust_info <- cust_info[,prodcnteach:=length(prod_id),by=c("cust_id","tran_id","store_id")]
cust_info <- cust_info[,prodcnt:=mean(prodcnteach),by=cust_id]
cust_info_summary <- unique(cust_info[order(revenue,decreasing = TRUE),c("cust_id","revenue","visitcnt","prodcnt")])

####################### Q2 #############################

prod_tran <- left_join(transaction,product,by="prod_id")
prod_tran <- as.data.table(prod_tran)

# 2.1 best product?
prod_info <- prod_tran[,volume:=sum(tran_prod_sale_qty),by="prod_id"]
prod_info <- prod_info[,revenue:=sum(tran_prod_paid_amt),by="prod_id"]
prod_info <- prod_info[,storevisit:=nrow(unique(.SD,by=c("tran_id","store_id","cust_id"))),by="prod_id"]
prod_info <- prod_info[,customercnt:=length(unique(cust_id)),by="prod_id"]
prod_info_summary <- unique(prod_info[order(volume,decreasing = TRUE),c("prod_id","volume","revenue","storevisit","customercnt")])

# 2.2 best product group?
# what is group here? category? or segmentation?
cate_info <- prod_tran[,volume:=sum(tran_prod_sale_qty),by="category_id"]
cate_info <- cate_info[,revenue:=sum(tran_prod_paid_amt),by="category_id"]
cate_info <- cate_info[,storevisit:=nrow(unique(.SD,by=c("tran_id","store_id","cust_id"))),by="category_id"]
cate_info <- cate_info[,customercnt:=length(unique(cust_id)),by="category_id"]
cate_info_summary <- unique(cate_info[order(volume,decreasing = TRUE),c("category_id","volume","revenue","storevisit","customercnt")])

####################### Q3 #############################

store_info <- transaction[,volume:=sum(tran_prod_sale_qty),by="store_id"]
store_info <- store_info[,revenue:=sum(tran_prod_paid_amt),by="store_id"]
store_info <- store_info[,storevisit:=nrow(unique(.SD,by=c("tran_id","cust_id"))),by="store_id"]
store_info <- store_info[,customercnt:=length(unique(cust_id)),by="store_id"]
store_info_summary <- unique(store_info[order(volume,decreasing = TRUE),c("store_id","volume","revenue","storevisit","customercnt")])

#################### FE for Q4 #########################

# customer

# attribute 1: valuable or cherry-picker : do you give a shit to discount
# measurement: the percentage of discounted items out of all items he bought
cust_seg <- prod_tran[,dispercent:=mean(tran_prod_offer_cts),by=cust_id]

# attribute 2: certain categories?
# measurement: the skewness / variance of his category portfolio
cust_seg <- cust_seg[,cateskew:=skew(as.numeric(table(category_id))),by=cust_id]

cust_seg_summary <- unique(cust_seg[,c("cust_id","dispercent","cateskew")])


#################### FE for Q5 #########################

# product

# attribute 1: KVI1, KVI2
# measurement: revenue, profit

# attribute 2: traffic driver: you don't target at buy it, but every time you buy other stuff, you tend to take some this
# measurement: transaction times

# attribute 3: promotion
# measurement: average promotion count*day/ days=latest-earlist


#################### FE for Q6 #########################

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
