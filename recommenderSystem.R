library(recommenderlab)
library(data.table)

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
