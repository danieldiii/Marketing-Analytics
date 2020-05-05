library(data.table)
library(car)
library(lmtest)

product1 = fread('modeldata_138936951.csv',header = TRUE)
product2 = fread('modeldata_138936952.csv',header = TRUE)
product3 = fread('modeldata_138936953.csv',header = TRUE)

mape <- function(pred,actual){
  mape <- mean(abs((actual - pred)/actual))
  return (mape)
}

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

###################PRODUCT 1##################
colnames(product1)

#additive model
out11 <- glm(volume~Flyer+Email+PaidSearch+WebDisplay+TVreach+
             Radioreach+is_hol+is_imphol+seas_index+avg_shelfprice+
             totaldiscount,data=product1)
summary(out11)
RMSE(predict(out11,product1),product1$volume) #34.56311
mape(predict(out11,product1),product1$volume) #0.2146937
dwtest(out11) #DW = 1.8296, p-value = 0.0693, Values below 2 indicate the possibility of positive autocorrelation
vif(out11) # Email, important_holidays, TV and Radio have VIF > 5

#multiplicative model
out12 <- glm(log(volume)~Flyer+Email+PaidSearch+WebDisplay+TVreach+
             Radioreach+is_hol+is_imphol+seas_index+avg_shelfprice+
             totaldiscount,data=product1)
summary(out12)
RMSE(exp(predict(out12,product1)),product1$volume) #37.74373
mape(exp(predict(out12,product1)),product1$volume) # 0.1955385
dwtest(out12) #DW = 1.468, p-value = 0.0003, Values below 2 indicate the possibility of positive autocorrelation
vif(out12) # Email, TV, Radio, important_holidays, TV and Radio have VIF > 5


#additive model with interaction
features<-c('Flyer','Email','PaidSearch','WebDisplay','TVreach','Radioreach',
            'is_hol','is_imphol','seas_index','avg_shelfprice','totaldiscount')
vars <- as.data.frame(combn(features,2))
interection <- c()
for(i in 1:ncol(vars)){
  c<-paste(vars[1,i],vars[2,i],sep = '*')
  interection<-append(interection,c)
}

print(paste(interection,collapse = '+'))

BigFm<-as.formula(paste('volume',paste(paste(features,collapse = '+'),paste(interection,collapse = '+'),sep = '+'), sep="~" ))
SmallFm<-as.formula(paste('volume',paste(features,collapse = '+'),sep='~'))

out_big<-glm(BigFm,data = product1)
summary(out_big)
vif(out_big)

sc<-list(lower=SmallFm,upper=BigFm)
out13<-step(out11,scope = sc,direction = 'both')
summary(out13)
RMSE(predict(out13,product1),product1$volume) #25.215
mape(predict(out13,product1),product1$volume) #0.0928
vif(out13)[vif(out13)>40]

ld.vars<-attributes(alias(out_big)$Complete)$dimnames[[1]]
formula.new <- as.formula(
  paste(
    paste(deparse(BigFm), collapse=""), 
    paste(ld.vars, collapse="-"),
    sep="-"
  )
)
out_big<-glm(formula.new,data = product1)
vif(out_big)


###################### WE PICK LOGIT MODEL AS THE DEPENDENT VARIABLE SHOULD BE BOUNDED ##################

# transform y
product1[,volume_trfm := log(volume/(max(volume)*1.1-volume))]
LogFm<-as.formula(paste('volume_trfm ~ ',paste(features,collapse = '+')))
out_log<-glm(LogFm,data = product1)
summary(out_log)

# transform it back to volume
product1$pred<-predict(out_log,data = product1)
product1[,pred := max(volume)*1.1*exp(pred)/(exp(pred)+1)]

RMSE(product1$pred,product1$volume)#34.83794
mape(product1$pred,product1$volume)#0.2083982
vif(out_log)
dwtest(out_log)#DW=1.7504, p-value=0.03

################# PRODUCT 2 #############
# transform y
product2[,volume_trfm := log(volume/(max(volume)*1.1-volume))]

#PRODUCT2 & PRODUCT3 have an extra feature indicating whether a product is displayed in a store or not
features<-c('Flyer','Email','PaidSearch','WebDisplay','TVreach','Radioreach',
            'is_hol','is_imphol','seas_index','avg_shelfprice','totaldiscount','whether_storedisplay')

LogFm<-as.formula(paste('volume_trfm ~ ',paste(features,collapse = '+')))
out_log2<-glm(LogFm,data = product2)
summary(out_log2)

# transform it back to volume
product2$pred<-predict(out_log2,data = product2)
product2[,pred := max(volume)*1.1*exp(pred)/(exp(pred)+1)]

RMSE(product2$pred,product2$volume)#18.69351
mape(product2$pred,product2$volume)#0.1441979
vif(out_log2)
dwtest(out_log2)#DW=2.0675, p-value=0.4


############# PRODUCT 3 #############
# transform y
product3[,volume_trfm := log(volume/(max(volume)*1.1-volume))]

out_log3<-glm(LogFm,data = product3)
summary(out_log3)

# transform it back to volume
product3$pred<-predict(out_log3,data = product3)
product3[,pred := max(volume)*1.1*exp(pred)/(exp(pred)+1)]

RMSE(product3$pred,product3$volume)#4.896241
mape(product3$pred,product3$volume)#0.4580704
vif(out_log3)
dwtest(out_log3)#DW=2.41, p-value=0.9374


