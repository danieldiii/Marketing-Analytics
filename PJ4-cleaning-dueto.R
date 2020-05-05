### mkt pj
# EDA + FE


########### import packages and data ###################

library(data.table)
library(dplyr)
library(psych)

options(scipen=999) # cancel Scientific notation


transaction_origin <- fread("all_transaction.csv")

#check target product id
prod_list <- c(138936952,138936953,138936951)

transaction_origin <- transaction_origin[prod_id %in% prod_list]

#origin transaction data doesnt include any data we need

product <- read.csv("product_table_supp.csv")
# 3, 7
transaction <- fread("transaction_table_supp.csv")
# 8879, 12


################## data cleaning #######################

# check transaction amount
sum(transaction$tran_prod_paid_amt<0)
# 0 obs
sum(transaction$tran_prod_paid_amt==0)
# 0 obs

# create new transaction id
transaction[, newid := .GRP, by = .(cust_id, tran_id, store_id)]
max(transaction$newid)
# 8653 times of transaction
length(unique(transaction$cust_id))
# 5084 customers
length(unique(transaction$prod_id))
# 3 products
length(unique(transaction$store_id))
# 410 stores

################## data preparation ####################
# create unit after-discount price
transaction <- transaction[, unitdiscountedprice:=tran_prod_paid_amt/tran_prod_sale_qty]

# no need to estimate profit

# create discount per visit
transaction <- transaction[, discount:=1-tran_prod_paid_amt/tran_prod_sale_amt]

# create unit shelf price
transaction <- transaction[, shelfprice:=tran_prod_sale_amt/tran_prod_sale_qty]

transaction$tran_dt <- as.Date(transaction$tran_dt)
transaction <- transaction[, year:=year(tran_dt)]
transaction <- transaction[, week:=format(tran_dt,"%U")]

transaction <- transaction[, weekid:=paste(year,week,sep = "")]

fwrite(transaction,'clean_transaction.csv')




############ aggregate by product and week ########

weekdata <- transaction %>%
  group_by(prod_id,weekid) %>%
  summarise(volume=sum(tran_prod_sale_qty), revenue=sum(tran_prod_paid_amt), 
            visit_cnt=n_distinct(newid), customer_cnt=n_distinct(cust_id), store_cnt=n_distinct(store_id),
            totaldiscount=1-(sum(tran_prod_paid_amt)/sum(tran_prod_sale_amt)),
            avg_shelfprice = sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty))


fwrite(weekdata,'weekdata.csv')



#####calculate dueto with multiplicative models###########
modeldata1 <- fread("modeldata_138936951.csv")
modeldata2 <- fread("modeldata_138936952.csv")
modeldata3 <- fread("modeldata_138936953.csv")
modeldata1$whether_storedisplay <- 0
#colnames(modeldata1)

#modify this line of code to calculate dueto for each product with below code
modeldata <- modeldata3

#############code######
colnames(modeldata)[24] <-"storedisplay" 
modeldata$logvolume <- log(modeldata$volume)
model <- lm(logvolume ~ totaldiscount + avg_shelfprice + seas_index + factor(is_hol) + factor(is_imphol) + Email + factor(Flyer) + WebDisplay
            + PaidSearch + TVreach + Radioreach + factor(storedisplay)
            , data = modeldata)
modeldata$est <- exp(predict(model, newdata = modeldata))
#summary(model)

###calculate base
model_base <- modeldata
baseprice <- mean(modeldata$avg_shelfprice)
model_base[, c('totaldiscount', 'Email', 'Flyer', 'PaidSearch', 'WebDisplay', 'TVreach', 'Radioreach'
               ,'storedisplay'
               )] <- 0 
model_base[,'avg_shelfprice'] <- baseprice
model_base$base <- exp(predict(model, newdata = model_base))
modeldata$base <- model_base$base

##due to shelfprice
dueto <- modeldata
addon <- modeldata
addon[,'avg_shelfprice'] <- baseprice
dueto$dueto_shelfprice <- dueto$est - exp(predict(model, newdata = addon)) 

for (driver in c('totaldiscount', 'Email', 'Flyer', 'PaidSearch', 'WebDisplay', 'TVreach', 'Radioreach' ,'storedisplay'
                 )) {
  addon <- modeldata
  addon[,driver] <- 0
  col_name = paste('dueto', driver, sep = '_')
  dueto[,c(col_name)] <- dueto$est - exp(predict(model, newdata = addon))
}


#debias: adjust duetos by scaling
dueto[, duetosum := base + dueto_shelfprice + dueto_totaldiscount + dueto_Flyer + dueto_Email + dueto_WebDisplay + dueto_PaidSearch + dueto_TVreach + dueto_Radioreach + dueto_storedisplay
      ]
dueto_scaled <- dueto

for (col in c('base', 'dueto_shelfprice', 'dueto_totaldiscount', 'dueto_Flyer', 'dueto_Email', 'dueto_WebDisplay', 'dueto_PaidSearch', 'dueto_TVreach', 'dueto_Radioreach', 'dueto_storedisplay'
              )) {
  dueto_scaled[, c(col)] <- dueto_scaled[, .SD, .SDcols = c(col)] / dueto_scaled$duetosum * dueto_scaled$volume
}

dueto_scaled$year <- substr(as.character(dueto_scaled$yearweekid),1,4)

#modify output file name
fwrite(dueto_scaled[,c("prod_id","year","weekid","volume","base","dueto_shelfprice","dueto_totaldiscount","dueto_Email","dueto_Flyer","dueto_PaidSearch","dueto_WebDisplay",
                       "dueto_TVreach","dueto_Radioreach","dueto_storedisplay"
                       )],"dueto_138936953.csv")
#############code###########