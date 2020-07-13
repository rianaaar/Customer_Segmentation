#install.packages("RMySQL")
library(RMySQL)
#connect
con <- dbConnect(MySQL(), user="root", password="", host="localhost",
                 dbname="bikes_sales")
# SQL
sql <- "SELECT * FROM `bikes`"

#send query
req_sql <- tryCatch(dbSendQuery(con, sql), finally = print("query ok"))

#get data
data.bikes <- fetch(req_sql, n=-1)
######
#SQL
sql <- "SELECT * FROM `bikeshops`"

#send query
req_sql <- tryCatch(dbSendQuery(con, sql), finally = print("query ok"))

#get data
data.customer <- fetch(req_sql, n=-1)
######
#SQL
sql <- "SELECT * FROM `orders`"

#send query
req_sql <- tryCatch(dbSendQuery(con, sql), finally = print("query ok"))

#get data
data.order <- fetch(req_sql, n=-1)

#close connection
all_cons <- dbListConnections(MySQL())
for(con in all_cons) dbDisconnect(con)

str(data.bikes)
str(data.customer)
str(data.order)

data_sales_bikes <- merge(data.order,data.bikes, by.x = "product.id", by.y = "bike.id")
data_sales_bikes <- merge(data_sales_bikes, data.customer, by.x = "customer.id", by.y = "bikeshop.id")
str(data_sales_bikes)
data_sales_bikes$model <- as.factor(data_sales_bikes$model)
data_sales_bikes$category1 <- as.factor(data_sales_bikes$category1)
data_sales_bikes$category2 <- as.factor(data_sales_bikes$category2)
data_sales_bikes$frame <- as.factor(data_sales_bikes$frame)
##############
#descriptive analysis
#terdapat 97 model
library(ggplot2)
ggplot(data=data_sales_bikes, aes(x=category1,color=category1)) + geom_bar() 
a=table(data_sales_bikes$category1)
pct=round(a/sum(a)*100)
lbs=paste(c("Mountain","Road")," ",pct,"%",sep=" ")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Mountain and Road Category")
########
ggplot(data=data_sales_bikes, aes(x=category2,color=category2)) + geom_bar()
summary(data_sales_bikes$frame)
ggplot(data=data_sales_bikes, aes(x=frame,color=frame)) + geom_bar()
summary(data_sales_bikes$frame)

summary(data_sales_bikes$price)
hist(data_sales_bikes$price,
     col="blue",
     main="Histogram to Show Count of Price",
     xlab="Price",
     ylab="Frequency",
     labels=TRUE)
val <- boxplot(data_sales_bikes$price,
               col="red",
               main="Boxplot for Descriptive Analysis of Price")
summary(val$out)
plot(density(log10(data_sales_bikes$price)),
     col="yellow",
     main="Density Plot for Price",
     xlab="Price",
     ylab="Density")
polygon(density(data_sales_bikes$price),
        col="#ccff66")

data_sales_bikes$price.category <- ifelse(data_sales_bikes$price > median(data_sales_bikes$price),"expensive","cheap")
ggplot(data=data_sales_bikes, aes(x=price.category,color=price.category)) + geom_bar()
data_sales_bikes$price.category <- as.factor(data_sales_bikes$price.category)
summary(data_sales_bikes$price.category)
#############
library(dplyr)
customerTrends <- data_sales_bikes %>%
  group_by(bikeshop.name,model, category1, category2, frame, price, price.category) %>%
  summarise(total.qty = sum(quantity)) #%>%

which(is.na(customerTrends))
ggplot(customerTrends, aes(x = price ,y = total.qty, color=category1)) + geom_point() 
ggplot(customerTrends, aes(x = price ,y = total.qty, color=price.category)) + geom_point()
#############
sales_bikes <- as.data.frame(customerTrends)
sales_bikes$model <- as.factor(sales_bikes$model)
sales_bikes$category1 <- as.factor(sales_bikes$category1)
sales_bikes$category2 <- as.factor(sales_bikes$category2)
sales_bikes$frame <- as.factor(sales_bikes$frame)
sales_bikes$price.category <- as.factor(sales_bikes$price.category)
str(sales_bikes)
atribut_use <- c("model","category1","category2","frame","price","price.category")
###
library(cluster)
library(Rtsne) 
set.seed(200)
gower_dist <- daisy(sales_bikes[atribut_use],
                    metric = "gower",
                    type = list(logratio = 5))
summary(gower_dist)
########
data_gower <- as.matrix(gower_dist)
# Output most similar pair
sales_bikes[
  which(data_gower == min(data_gower[data_gower != min(data_gower)]), arr.ind = TRUE)[1, ], ]
########
# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
########
pam_fit <- pam(gower_dist, diss = TRUE, k = 6)
pam_results <- sales_bikes %>%
  dplyr::select(-bikeshop.name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
sales_bikes[pam_fit$medoids, ]
#######
sales_bikes$cluster <- pam_fit$clustering
######
