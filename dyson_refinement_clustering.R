rm(list=ls())
# R Project Template

# 1. Import
# a) Load libraries
library(skimr)
# b) Load dataset
source("C:/DATA/R model/00_Customers/00_dyson data/read_data_dyson.R", local=FALSE)
source("C:/DATA/theme_secun.R")

# a) Descriptive statistics
skim(data_sales_f)
skim(data_sales_s)
skim(data)
skim(data_sku)
skim(sku_sales)

#No missing data (0 means no sales)
#Transform sales
data_sales_f$fecha<- as.Date (data_sales_f$fecha, format = "%Y-%m-%d")
data_sales_s$fecha<- as.Date (data_sales_s$fecha, format = "%Y-%m-%d")
data$fecha<- as.Date (data$fecha, format = "%Y-%m-%d")

# c) Data visualizations
#Plot sales units over time for 4 categories
data.portfolio<- merge(data_sales_s,data_sku) %>%
  select(sku,range=product_range,margin=cash_margin,sales,fecha)

#remove those ones with no sales so far
remove.sku<- data.portfolio %>% 
  group_by(sku) %>%
  summarise(sales=sum(sales))%>%
  filter(sales<500)
data.portfolio <- data.portfolio[!data.portfolio$sku %in% remove.sku$sku,] 


ggplot(data.portfolio, aes(y=range,x=sales))+
  geom_boxplot()+
  labs(y = "Product Category", 
       x ="Sales (units)")+
  xlim(0,6000)

#Evolution of sales over time for the different categories
data.portfolio<- merge(data_sales_s,data_sku) %>%
  select(sku,range=product_range,margin=cash_margin,sales,fecha) %>%
  filter(sales !=0)

latest.months<-c("2018-11-01" ,"2018-10-01" ,"2018-09-01")
data.portfolio.latest <- data.portfolio %>%
  filter (fecha == "2018-11-01" |
          fecha == "2018-10-01" |
          fecha == "2018-09-01" ) 

#remove those ones with no sales so far
remove.sku<- data.portfolio %>% 
  group_by(sku) %>%
  summarise(sales=sum(sales))%>%
  filter(sales<500)
data.portfolio <- data.portfolio[!data.portfolio$sku %in% remove.sku$sku,] 

library(ggridges)
ggplot(data=data.portfolio, mapping=aes(y=range,x=sales))+
  geom_density_ridges()+
    labs(y = "Product category", 
       x ="Monthly sales (units)", 
       title ="Majority of sales across 4 different product categories are less than 250 units per month",
       subtitle= "plotted over a 2-year period ",
       caption = "Red points are sales points corresponding from the last 3 months")+
  geom_point(data.portfolio.latest,mapping=aes(y=range,x=sales), color="gray43")+
  theme_secun()+
  theme(axis.text.y = element_blank())+
  xlim(c=0,1000)

#Plot sales over time for all SKUs by category
data.portfolio<- merge(data_sales_s,data_sku) %>%
  select(sku,product,range=product_range,margin=cash_margin,sales,fecha) %>%
  group_by(range,sku) %>%
  arrange(fecha) %>%
  mutate(sales=cumsum(sales))
  
#remove those ones with no sales so far
remove.sku<- data.portfolio %>% 
  group_by(sku) %>%
  summarise(sales=sum(sales))%>%
  filter(sales<500)
data.portfolio <- data.portfolio[!data.portfolio$sku %in% remove.sku$sku,] 

#Order by sku and filter for top_n
data.portfolio <-left_join(data.portfolio, sku_sales) %>%
  arrange(total_sales,sku) %>%
  as.data.frame()%>%
  unique() 
sku.ordered<-unique(data.portfolio$sku)
data.portfolio$sku<-factor(data.portfolio$sku, level=sku.ordered)


aux="FS" #EC,FS,PC
ggplot(data=filter(data.portfolio,range==aux), aes(x=fecha,y=sales))+
geom_point(aes(color=range)) +
  geom_line()+
facet_wrap(sku~.,scales="free_y")

#Plot sales over time for top-20 sold SKUs
aux.ind<-data.portfolio$sku %in% sales_by_SKU$sku[1:20]

ggplot(data.portfolio[aux.ind,], 
       aes(x=fecha,y=sales))+
geom_line(aes(color=range)) +
facet_wrap(sku~.,scales="free_y")+
scale_color_discrete(
    labels = c("Category 1", "Category 2", "Category 3", "Category 4"))+
  labs(color="Legend")+
scale_x_discrete(labels="")+
  labs(x="Time", title = "Illustrative cummulative sales evolution for different SK")

#Plot products over time
data.portfolio<- merge(data_sales_s,data_sku, by ="sku") %>%
  select(range=product_range,product,fecha,sales) %>%
  group_by(range,product,fecha) %>%
  summarise(sales_product=sum(sales))

ggplot(data=data.portfolio, aes(x=fecha,y=sales_product))+
geom_col(aes(fill=range)) +
facet_wrap(product~.,scales="free_y")

# 3. Transform Data
data.cluster<-select(data.sales, sku, ends_with("_s"))

# 4. Model:Evaluate Algorithms
library(caret)
#prepare data
set.seed(123)
# a) Test options and evaluation metric
#k-means algorithm 
#Data 
##Preprocessing
kmeans_fit<-kmeans(scale(data.cluster[2:24]), centers=6, nstart=20, iter.max=50)
kmeans_fit$size

cluster <- c(1: length(unique(kmeans_fit$cluster)))
center_df <- data.frame(cluster, kmeans_fit$centers) #characterise cluster
center_reshape <- gather(center_df, features, values, X201701_s:X201811_s) 
#Plot data
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradient(low="brown", high="yellow")+
  theme_secun()+
  labs(y = "clusters", 
       x ="Time", 
       title ="4 clusters where almost 200 SKUs are encoded",
       subtitle= "plotted over a 2-year period ")

#Assignment of skus to different clusters
cluster_sku<- as.data.frame(cbind(cluster=kmeans_fit$cluster,sku=data.cluster[,1]))
table(cluster_sku$cluster)


data.portfolio<- merge(data_sales_s,data_sku) %>%
  select(sku,sales,fecha,product,range=product_range)

aux<-data.portfolio$sku %in% cluster_sku[cluster_sku$cluster=="6",2]
ggplot(data.portfolio[aux,], 
       aes(x=fecha,y=sales))+
geom_line(aes(color=range)) +
facet_wrap(sku~.,scales="free_y")




inTrain <- sample(seq(along = data.cluster[,1]), length(data.cluster[,1])*0.25)

training <- data.cluster[inTrain,]
test <- data.cluster[-inTrain,]

preProcValues <-  c("center", "scale")
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats=3,
                              verboseIter = TRUE)
knn_fit <- train(x=training[,1:24], 
               method = "knn",
               trControl = train.control,
               preProcess=preProcValues)


# b) Spot Check Algorithms
# c) Compare Algorithms
# c) Algorithm Tuning
# d) Ensembles

# 5. Visualize
# a) Predictions on validation dataset
# b) Create standalone model on entire training dataset
# c) Save model for later use
# 
# 
# 6. Communicate
