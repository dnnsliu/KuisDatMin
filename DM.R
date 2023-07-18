product <- read.csv("product_data.csv",fileEncoding="UTF-8-BOM")
tran <- read.csv("transaction_data.csv",fileEncoding="UTF-8-BOM")

dg<-merge(product,tran,by="product_name")
View(tran)
#a
#data visul
h1 <- table(tran$product_name)
h2 <- h1[h1 > 30]
?pie(h2,main="Product Distribution", labels = h2 ,col = c("red","lightgreen","lightblue","purple"))

#b
Rating<-product$rating
hist(Rating, col = "lightblue", main = "Product Ratings Frequency")

#c
cate <- for ( i in dg$product_name) {
  ifelse (dg$price >= 20000,"high",
      ifelse (dg$price >= 10000 & dg$price <20000,"Medium",
       ifelse (dg$price <10000,"Low",dg$price)))}
barplot(cate,main="Price Range of Products",xlab="Price Categories",ylab="Number of Product",col=c("red","green","blue"))


#2
#1 pre pro
dp <- dg$product_name[!duplicated(dg$product_name)]
dp <- dg$rating[dg$rating>=4.1]
dp <- dg$processor[dg$processor!="Helio G70 Processor"]
dp <- dg$warranty[dg$warranty!=""]

View(dp)
#2
apriori.data <- split(dg$product_name, dg$transaction_id)
#3
install.packages('arules')
library(arules)
freq <- apriori(apriori.data, parameter = list(support=0.2, target="frequent itemsets"))

inspect(freq)
#4
rule <- ruleInduction(freq, confidence=0.6)
inspect (rule)