
########### import packages and data ###################

library(data.table)
library(dplyr)
library(psych)

options(scipen=999) # cancel Scientific notation

product <- read.csv("product_table.csv")
# 10767, 7
transaction <- fread("transaction_table.csv")
# 29617585, 12


################## data cleaning #######################

# check transaction amount
sum(transaction$tran_prod_paid_amt<0)
# 8 obs
sum(transaction$tran_prod_paid_amt==0)
# 3919 obs

# delete those with negative sales
transaction <- transaction[tran_prod_paid_amt>=0]
# 29617577, 12

# create new transaction id
transaction[, newid := .GRP, by = .(cust_id, tran_id, store_id)]
max(transaction$newid)
# 2830564 times of transaction
length(unique(transaction$cust_id))
# 7920 customers
length(unique(transaction$prod_id))
# 10770 products
length(unique(transaction$store_id))
# 421 stores

################## data preparation ####################
# create unit after-discount price
transaction <- transaction[, unitdiscountedprice:=tran_prod_paid_amt/tran_prod_sale_qty]

# estimate profit
# Assumption: Profit margin should be negatively correlated with sales volume (1%~6%)

test1 <- transaction %>%
  group_by(prod_unit, prod_id) %>%
  summarise(sale_qty = sum(tran_prod_sale_qty),paid_amt = sum(tran_prod_paid_amt))

# for unit in CT
summary(test1[test1$prod_unit=='CT',])  
# Qu - 1st: 354, 2st: 925, 3rd: 2731

# for unit in KG
summary(test1[test1$prod_unit=='KG',])  
# Qu - 1st: 218, 2st: 710, 3rd: 3444

# for sales volume below 25%, take 6% as profit margin
test1 <- as.data.table(test1)
test1 <- test1[((prod_unit=='CT'&sale_qty<354)|(prod_unit=='KG'&sale_qty<218)), unitcost:= 0.94*paid_amt/sale_qty]

# for sales volume belong to 25%~50%, take 4.5% as profit margin
test1 <- test1[((prod_unit=='CT'&345<=sale_qty&sale_qty<925)|(prod_unit=='KG'&218<=sale_qty&sale_qty<710)), unitcost:= 0.955*paid_amt/sale_qty]

# for sales volume belong to 50%~75%, take 2.5% as profit margin
test1 <- test1[((prod_unit=='CT'&925<=sale_qty&sale_qty<2731)|(prod_unit=='KG'&710<=sale_qty&sale_qty<3444)), unitcost:= 0.975*paid_amt/sale_qty]

# for sales volume above 75%, take 1% as profit margin
test1 <- test1[((prod_unit=='CT'&2731<=sale_qty)|(prod_unit=='KG'&3444<=sale_qty)), unitcost:= 0.99*paid_amt/sale_qty]


#################### merge dataset ########################
prod_tran <- left_join(transaction,product,by="prod_id")
prod_tran <- left_join(prod_tran, test1[,c('prod_id','unitcost')], by="prod_id")

#################### data prep #############################
# profits
prod_tran <- as.data.table(prod_tran)
prod_tran <- prod_tran[,profit:=tran_prod_paid_amt-unitcost*tran_prod_sale_qty]

# check bag data, include shopping bag, freezing bag, shopping cart....
bag <- prod_tran[category_desc_eng=="BAGS"] %>%
  group_by(subcategory_id, sub_category_desc) %>%
  summarise(price = mean(tran_prod_paid_amt/tran_prod_sale_qty), unit = sum(tran_prod_sale_qty))
  
# drop transaction with category_desc_eng == bags since customers don't go to the store to buy those things
prod_tran <- prod_tran[category_desc_eng!="BAGS"]
# 28852307 obs

fwrite(prod_tran,'all_transaction.csv')

############ check shampoo and hair conditioners product and brand ########
target_prod <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(brand_desc,category_id,category_desc_eng,prod_id,sub_category_desc,subcategory_id) %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            time_discount=sum(tran_prod_offer_cts>0)/n())

target_subcategory <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(brand_desc,category_id,category_desc_eng,sub_category_desc,subcategory_id) %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            prod_cnt=n_distinct(prod_id), time_discount=sum(tran_prod_offer_cts>0)/n())

target_category <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(brand_desc,category_id,category_desc_eng) %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            prod_cnt=n_distinct(prod_id), sub_category_cnt=n_distinct(sub_category_desc),
            time_discount=sum(tran_prod_offer_cts>0)/n(), brand_cnt=n_distinct(brand_desc))

target_brand <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(brand_desc) %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            prod_cnt=n_distinct(prod_id), sub_category_cnt=n_distinct(sub_category_desc),
            category_cnt=n_distinct(category_desc_eng),time_discount=sum(tran_prod_offer_cts>0)/n())

fwrite(target_prod,'target_prod.csv')
fwrite(target_subcategory,'target_subcategory.csv')
fwrite(target_category,'target_category.csv')
fwrite(target_brand,'target_brand.csv')

################# check time trend #########################
target_brand_time <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(brand_desc, tran_dt) %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)))

fwrite(target_brand_time,'target_brand_time.csv')


################ customer data aggregation #########################
cust_info <- prod_tran %>%
  group_by(cust_id) %>%
  summarise(revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid),
            prod_cnt=n_distinct(prod_id), store_cnt=n_distinct(store_id), brand_cnt=n_distinct(brand_desc),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            time_discount=sum(tran_prod_offer_cts>0)/n())
fwrite(cust_info,'cust_info.csv') 


cust_info_target <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(cust_id) %>%
  summarise(x_revenue=sum(tran_prod_paid_amt), x_profit=sum(profit),
            x_visit_cnt=n_distinct(newid), x_qty=sum(tran_prod_sale_qty),
            x_prod_cnt=n_distinct(prod_id), x_store_cnt=n_distinct(store_id), x_brand_cnt=n_distinct(brand_desc),
            x_totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            x_time_discount=sum(tran_prod_offer_cts>0)/n(),
            qty_private=sum(tran_prod_sale_qty[brand_desc=="PRIVATE LABEL"]),
            qty_nonprivate=sum(tran_prod_sale_qty[brand_desc!="PRIVATE LABEL"]))

fwrite(cust_info_target,'cust_info_target.csv') 


##################### customer to exclude ######################
# 8 subcategories 2 categories
# consider product in same category as equivalent
# if a customers purchase Pernalonga's PRIVATE LABEL equivalent to the promoted brand (larger quantity)
cust_info_target2 <- prod_tran[(category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO")] %>%
  group_by(cust_id,category_id,brand_desc) %>%
  summarise(qty=sum(tran_prod_sale_qty))

cust_info_target3 <- as.data.table(left_join(as.data.table(cust_info_target2)[brand_desc!="PRIVATE LABEL"],
                                             as.data.table(cust_info_target2)[brand_desc=="PRIVATE LABEL"],
                                             by=c("cust_id","category_id")))
cust_info_target3[is.na(qty.y)]$qty.y <- 0
cust_info_target3$ifinclude <- as.numeric(cust_info_target3$qty.x >= cust_info_target3$qty.y)
cust_info_target4 <- cust_info_target3[,c("cust_id","brand_desc.x","category_id","ifinclude")]

fwrite(cust_info_target4,'cust_exclude_brand.csv') 


# above data does not contain infomation for combination of customer and brand for:
# 1 who never buy shampoo or hair conditioner - should consider as potential customer?
# 2 only buy privite label - should exclude, refer to dataset cust_only_buy_private.csv
# 3 never buy privite label and never try some brands - keep
# 4 have bought privite label and never try some brands - exclude
# for 3/4 refer to dataset cust_ever_private.csv


# 3 never buy privite label and never try some brands - keep
# 4 have bought privite label and never try some brands - exclude
# for 3/4 refer to dataset cust_ever_private.csv

test2<-expand.grid(unique(cust_info_target2$cust_id),unique(cust_info_target2$category_id))
names(test2) <- c("cust_id","category_id")
cust_info_target5 <- as.data.table(left_join(as.data.table(test2),
                                             as.data.table(cust_info_target2)[brand_desc=="PRIVATE LABEL"],
                                             by=c("cust_id","category_id")))
cust_info_target5$ever_private <- as.numeric(!is.na(cust_info_target5$qty))
cust_info_target5 <- cust_info_target5[,c("cust_id","category_id","ever_private")]
fwrite(cust_info_target5,'cust_ever_private.csv') 


# 2 only buy privite label - should exclude, refer to dataset cust_only_buy_private.csv
cust_info_target7 <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(cust_id,category_id) %>%
  summarise(qty_private=sum(tran_prod_sale_qty[brand_desc=="PRIVATE LABEL"]),
            qty_nonprivate=sum(tran_prod_sale_qty[brand_desc!="PRIVATE LABEL"]))
cust_info_target7 <- as.data.table(cust_info_target7)[qty_private!=0&qty_nonprivate==0][,c("cust_id","category_id")]
fwrite(cust_info_target7,'cust_only_buy_private.csv')



summary <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            time_discount=sum(tran_prod_offer_cts>0)/n())

summary2 <- prod_tran %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            time_discount=sum(tran_prod_offer_cts>0)/n())



######################### clustering, to find target customer ##########################
cldata <- scale(cust_info_target[,-1])

############ decide the best k ##################
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
wss <- sapply(1:k.max, 
              function(k){kmeans(cldata, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Determine number of clusters
wss <- (nrow(cldata)-1)*sum(apply(cldata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cldata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# decide to go with k=4, clustering with 4 groups
k1 <- kmeans(cldata,4,25)
str(k1)

fviz_cluster(k1, data = cldata ,stand = FALSE,geom = "point",pointsize = 1)

cust_info_target$cluster <- k1$cluster

k1$centers

centers <- as.data.table(k1$centers)

library(reshape)

centers$id <- c(1,2,3,4)
centers <- centers[,c(1,2,3,4,5,6,8,9,12)]
names(centers) <- c("revenue","profit","visit count","product quantity","unique product count","store count","average discount","% discounted transaction","id")
centers_melt <- melt(centers,id=c("id"))

ggplot(centers_melt,aes(x=id,y=value,fill=variable))+geom_bar(stat='identity', position='dodge') +theme_bw() +labs(x = 'Cluster',y = 'Value', title = 'Features of the 4 centers') +theme(axis.title =element_text(size = 16),axis.text =element_text(size = 14, color = 'black'),plot.title =element_text(hjust = 0.5, size = 20))


# cluster 2 is the cluster that is attracted most by discount, we chose this cluster as our baseline target customers.

######## find the brand to invite ############

# for cluster 2, who is most attracted by discount of shampoo and hair conditioner
target_cunstomer <- cust_info_target[cluster==2]

prod_tran_tg_cl <- prod_tran[cust_id%in%target_cunstomer$cust_id]
#rm(prod_tran)

tg_cl_brand <- prod_tran_tg_cl[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(brand_desc) %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            prod_cnt=n_distinct(prod_id), sub_category_cnt=n_distinct(sub_category_desc),
            category_cnt=n_distinct(category_desc_eng),time_discount=sum(tran_prod_offer_cts>0)/n())

tg_cl_brand_1 <- as.data.table(tg_cl_brand)
brand_1 <- tg_cl_brand_1[brand_desc!="PRIVATE LABEL",c(1,2,3,4,5,6,7)]
brand_1 <- t(brand_1)
colnames(brand_1) <- brand_1[1,]
brand_1 <- brand_1[-1,]
na <- rownames(brand_1)
brand_1 <- as.data.table(brand_1)
brand_1$feature <- na

fwrite(brand_1,"brand_2.csv")

# consider halo effect

panteneid <- prod_tran_tg_cl[brand_desc=="PANTENE"]$newid
pantenehalo <- prod_tran_tg_cl[newid%in%panteneid] %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            prod_cnt=n_distinct(prod_id), sub_category_cnt=n_distinct(sub_category_desc),
            category_cnt=n_distinct(category_desc_eng),time_discount=sum(tran_prod_offer_cts>0)/n())
pantenehalo$brand <- "PANTENE"

# use history transaction to find which brand will receive halo effect
for (brand in target_brand$brand_desc) {
  panteneid <- prod_tran_tg_cl[brand_desc==brand]$newid
  brandhalo <- prod_tran_tg_cl[newid%in%panteneid] %>%
    summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
              visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
              totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
              prod_cnt=n_distinct(prod_id), sub_category_cnt=n_distinct(sub_category_desc),
              category_cnt=n_distinct(category_desc_eng),time_discount=sum(tran_prod_offer_cts>0)/n())
  brandhalo$brand <- brand
  pantenehalo<-rbind(pantenehalo,brandhalo)
}

brandhalo <- unique(pantenehalo)

tg_cl_brand_1 <- as.data.table(brandhalo)
brand_1 <- tg_cl_brand_1[brand!="PRIVATE LABEL",c(1,2,3,4,5,6,12)]
brand_1 <- t(brand_1)
colnames(brand_1) <- brand_1[7,]
brand_1 <- brand_1[-7,]
na <- rownames(brand_1)
brand_1 <- as.data.table(brand_1)
brand_1$feature <- na

fwrite(brand_1,"brandholo_2.csv")

# for cluster 1, who is shampoo lover

target_cunstomer <- cust_info_target[cluster==1]

prod_tran <- fread('all_transaction.csv')

prod_tran_tg_cl <- prod_tran[cust_id%in%target_cunstomer$cust_id]
rm(prod_tran)

tg_cl_brand_1 <- prod_tran_tg_cl[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO"] %>%
  group_by(brand_desc) %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            prod_cnt=n_distinct(prod_id), sub_category_cnt=n_distinct(sub_category_desc),
            category_cnt=n_distinct(category_desc_eng),time_discount=sum(tran_prod_offer_cts>0)/n())

tg_cl_brand_1 <- as.data.table(tg_cl_brand_1)
brand_1 <- tg_cl_brand_1[brand_desc!="PRIVATE LABEL",c(1,2,3,4,5,6,7)]
brand_1 <- t(brand_1)
colnames(brand_1) <- brand_1[1,]
brand_1 <- brand_1[-1,]
na <- rownames(brand_1)
brand_1 <- as.data.table(brand_1)
brand_1$feature <- na

fwrite(brand_1,"brand_1.csv")

# consider halo effect

panteneid <- prod_tran_tg_cl[brand_desc=="PANTENE"]$newid
pantenehalo <- prod_tran_tg_cl[newid%in%panteneid] %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            prod_cnt=n_distinct(prod_id), sub_category_cnt=n_distinct(sub_category_desc),
            category_cnt=n_distinct(category_desc_eng),time_discount=sum(tran_prod_offer_cts>0)/n())
pantenehalo$brand <- "PANTENE"

for (brand in target_brand$brand_desc) {
  panteneid <- prod_tran_tg_cl[brand_desc==brand]$newid
  brandhalo <- prod_tran_tg_cl[newid%in%panteneid] %>%
    summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), profit=sum(profit),
              visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
              totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
              prod_cnt=n_distinct(prod_id), sub_category_cnt=n_distinct(sub_category_desc),
              category_cnt=n_distinct(category_desc_eng),time_discount=sum(tran_prod_offer_cts>0)/n())
  brandhalo$brand <- brand
  pantenehalo<-rbind(pantenehalo,brandhalo)
}

brandhalo_1 <- unique(pantenehalo)
brandhalo_1 <- brandhalo_1[,c(12,1,2,3,4,5,6,7,11)]

brandhalo_1  <- as.data.table(brandhalo_1)
brandhalo_1 <- brandhalo_1[brand!="PRIVATE LABEL",c(1,2,3,4,5,6,7)]
brandhalo_1 <- t(brandhalo_1)
colnames(brandhalo_1) <- brandhalo_1[1,]
brandhalo_1 <- brandhalo_1[-1,]
na <- rownames(brandhalo_1)
brandhalo_1 <- as.data.table(brandhalo_1)
brandhalo_1$feature <- na

fwrite(brandhalo_1,"brandhalo_1.csv")

# exlude those private label lover from our target customers

private_shampoo <- prod_tran[brand_desc=="PRIVATE LABEL"&(category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO")]
exclude <- private_shampoo[,rev_private:=sum(tran_prod_paid_amt),by="cust_id"]
exclude <- unique(exclude[,c("cust_id","rev_private")],by=c("cust_id","rev_private"))

nivea_shampoo <- prod_tran[brand_desc=="NIVEA"&(category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO")]
include <- nivea_shampoo[,rev_private:=sum(tran_prod_paid_amt),by="cust_id"]
include <- unique(include[,c("cust_id","rev_private")],by=c("cust_id","rev_private"))

clude <- left_join(exclude,include,by="cust_id")
clude <- as.data.table(clude)
clude$whetherprivate <- clude$rev_private.x>clude$rev_private.y
excludecustid <- clude[whetherprivate!=FALSE,cust_id]

cust_info_target_withoutprivate <- cust_info_target[!cust_id%in%excludecustid,]

fwrite(cust_info_target_withoutprivate,"target_customers.csv")



################################ personalized recommendation system ##########################################
library(recommenderlab)

#read in the data
customer <- fread('target_customers.csv')
target_customer <- customer[customer$cluster==1 | customer$cluster==2,]

product <- fread('product_table.csv')
head(product)

transaction <- fread('transaction_table.csv')
tran <- transaction[,c('cust_id','prod_id','tran_prod_sale_qty')]
head(tran)

#join the datasets
cust_tran <- target_customer[tran,on='cust_id',nomatch=0]
cust_tran <- cust_tran[,c('cust_id','prod_id','tran_prod_sale_qty')]
head(cust_tran)

cust_tran_prod <- cust_tran[product,on='prod_id',nomatch=0]
head(cust_tran_prod)

#filter out transactions related to shampoo or hair conditioners products
nivea_shampoo <- cust_tran_prod[cust_tran_prod$category_desc_eng %in% c('SHAMPOO','HAIR CONDITIONERS')]
head(nivea_shampoo)

#pivot into matrix
install.packages('reshape')
library(reshape)

matrix_data <- nivea_shampoo[,c('cust_id','prod_id','tran_prod_sale_qty')]
matrix_data$cust_id <- as.factor(matrix_data$cust_id)
matrix_data$prod_id <- as.factor(matrix_data$prod_id)
matrix_data$tran_prod_sale_qty <- as.numeric(matrix_data$tran_prod_sale_qty)

matrix1 <- as(matrix_data, "realRatingMatrix")

#build the recommender model
recomModel <- Recommender(matrix1, method = "LIBMF")

predict <- predict(recomModel, matrix1, type='ratingMatrix')
result<-as(predict, "matrix")

#find out the list of NIVEA products
nivea<-product[product$brand_desc=='NIVEA',prod_id]

nivea_result<-result[,colnames(result) %in% nivea]

top_product<-apply(nivea_result, 1, max)
nivea_result[1,]==top_product[1]

#find out the top rated NIVEA product for each of our target customer
recommend_product<-apply(nivea_result, 1, function(t) colnames(nivea_result)[which.max(t)])
recommend_product<-as.data.frame(recommend_product)
recommend_product$prod_id<-recommend_product$recommend_product
recommend_product$cust_id<-rownames(recommend_product)
recommend_product<-recommend_product[c('cust_id','prod_id')]
View(recommend_product)
unique(recommend_product$prod_id)

fwrite(recommend_product,'recommend_product.csv')


############# cost-benefit analysis ##################

prod_tran <- fread('all_transaction.csv')
sandw <- prod_tran[category_desc_eng=="HAIR CONDITIONERS"|category_desc_eng=="SHAMPOO",]
dt <- fread("recommend_product.csv")
tc <- fread("target_customers.csv")
dt <- left_join(dt,tc[,c("cust_id","cluster")],by="cust_id")

# for cluster 1, shampoo lover, use min discount percentage since they don't care;
# for cluster 2, shampoo discount lover, use mean discount percentage since they have a standard in their mind.

sandw[unitdiscountedprice/prod_unit_price<1,maxpercentage:=max(unitdiscountedprice/prod_unit_price),by="cust_id"]
sandw[,meanpercentage:=mean(unitdiscountedprice/prod_unit_price),by="cust_id"]

dt <- left_join(dt,sandw[,c("cust_id","maxpercentage","meanpercentage")])
dt <- as.data.table(dt)
dt[,offer:=fifelse(cluster==1,maxpercentage,meanpercentage),by="cust_id"]
dt <- unique(dt)
dt <- dt[!is.na(maxpercentage),]

sandw[,averagecost:=mean(unitcost),by=prod_id]
sandw[,averageprice:=mean(tran_prod_sale_amt),by=prod_id]
dt <- left_join(dt,sandw[,c("prod_id","averagecost")],by="prod_id")
dt <- unique(dt)

dt <- left_join(dt,sandw[,c("prod_id","averageprice")],by="prod_id")
dt <- unique(dt)
dt <- as.data.table(dt)
dt[,discount_amt:=max(offer*averageprice,averagecost),by="cust_id"]
dt[,discount_amt:=averageprice-discount_amt,by="cust_id"]
dt[,profit:=averageprice-discount_amt-averagecost,by="cust_id"]
sum(dt$discount_amt) #$4326.637
# expected cost is $4326.637

dt[,product_incre_v:=sum(incre_v),by="prod_id"]

prod_incre <- unique(dt[,c("prod_id","product_incre_v")],by=c("prod_id","product_incre_v"))


# predict sales amount (expected amount for each customer)
# c1
c1 <- dt[cluster==1,cust_id]
tran_c1 <- sandw[cust_id%in%c1]
tran_c1$offer=tran_c1$unitdiscountedprice/tran_c1$prod_unit_price
glr <- glm(tran_prod_sale_qty~offer,data = tran_c1,family = "poisson")
summary(glr)

#lr <- lm(tran_prod_sale_qty~offer,data = tran_c1)
#summary(lr)

new <- data.frame(offer = dt[cluster==1]$offer)

expect =predict(glr, newdata = new,type = "response")

c11 <- data.frame(cust_id = dt[cluster==1,cust_id],expect_amt = expect)
dt <- left_join(dt,c11,by="cust_id")
dt <- as.data.table(dt)
# c2
c2 <- dt[cluster==2,cust_id]
tran_c2 <- sandw[cust_id%in%c2]
tran_c2$offer=tran_c2$unitdiscountedprice/tran_c2$prod_unit_price
glr2 <- glm(tran_prod_sale_qty~offer,data = tran_c2,family = "poisson")
summary(glr2)

new2 <- data.frame(offer = dt[cluster==2]$offer)

expect2 =predict(glr2, newdata = new2,type = "response")

c22 <- data.frame(cust_id = dt[cluster==2,cust_id],expect_amt = expect2)
dt[cluster==2,expect_amt:=c22$expect_amt]
sum(dt$expect_amt)
# will sell 3541 Nivea products with our promotion.

### compare to baseline. Only those customers focused on Nivea in the past will continue doing so.
sandw[,favbrand:=names(sort(table(brand_desc),decreasing = TRUE))[1],by="cust_id"]
sandw$whethernivea <- sandw$favbrand=="NIVEA"
sum(sandw$whethernivea) #4199
dt <- left_join(dt,sandw[,c("cust_id","whethernivea")])
dt <- unique(dt)
dt <- as.data.table(dt)
dt[whethernivea==FALSE,origin:=0]
dt$incre_v <- dt$expect_amt-dt$origin
sum(dt$incre_v) # 3385.098
sum(dt$expect_amt)-sum(dt$incre_v) #155.9
sum(dt$profit) # profit 1147.287

fwrite(dt,"cb2.csv")

### expected halo effect
#cluster 1
# NIVEA itself 189.34
# halo 13244.22
#cluster 2
# NIVEA itself -72
# halo 7933.51

# estimate halo effect would be 70 times
71*sum(dt$profit) #  total profit $81244.36

