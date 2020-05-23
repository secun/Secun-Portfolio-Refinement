rm(list=ls())
# R Project Template

# 1. Import
# a) Load libraries
library(skimr)
library(ggplot2)
# b) Load dataset
source("C:/DATA/R model/00_Customers/00_dyson data/read_data_dyson.R", local=FALSE)
source("C:/DATA/theme_secun.R")

theme_secun <- function () { 
  theme_bw(base_size=12, base_family="Arial") + 
    theme(
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )}

#data ready
# Original data from the file 
# data.sales : sales units 
# data-margin : margisn by products 

# Modified data sets 
# data_sales_f: forecast of sales daa 
# data_sales_s: actual sales data 
# sales_by_sku : 14 most sold SKUs 
# data: contains forecasted data, sales data and the difference

# 2. Tidy data
# a) Descriptive statistics
skim(data_sales_f)
skim(data_sales_s)
skim(data)
#No missing data (0 means no sales)

# c) Data visualizations
#Sales,forecast and diff over time (general)
data.plot<- data %>%
  pivot_longer(names_to="type", cols=c("forecast","sales","f_s")) %>%
  group_by(fecha,type) %>%
  summarise(value=sum(value))

ggplot(filter(data.plot, type=="forecast" | type=="sales"),
       aes(x=fecha,y=value)) +
  geom_area(aes(group=type,fill=type), alpha=0.2,position = "identity")+
  geom_col( filter(data.plot, type=="f_s"), mapping=aes(x=fecha,y=value),fill="grey")+
  theme_secun()+
  theme(axis.text.x = element_text(angle = 45))
  

#Sales,forecast and diff over time (most sold SKUs)
data.plot<- data %>%
  pivot_longer(names_to="type", cols=c("forecast","sales","f_s")) %>%
  filter(sku %in% sales_by_SKU$sku) %>%
  group_by(fecha,type) %>%
  summarise(value=sum(value))

ggplot(filter(data.plot, type=="forecast" | type=="sales"),
       aes(x=fecha,y=value)) +
  geom_area(aes(group=type,fill=type), alpha=0.2,linetype= 2, position = "identity")+
  geom_col( filter(data.plot, type=="f_s"), mapping=aes(x=fecha,y=value),fill="grey", size=.2)+
  labs(x = "Month", 
       y ="Units",
       title = "Forecast and sales on different wave-lengths" ) +
  theme_secun()+
  theme(axis.text.x = element_text(angle = 45))

# 3. Transform Data 
# wider format
data.sales<-data_sales_s %>% 
  pivot_wider(names_from =fecha ,values_from = sales) %>%
  as.data.frame()
# b) duplication
setdiff(data.sales$sku, unique(data.sales$sku) )

rownames(data.sales) <- data.sales$sku
data.sales<-select(data.sales,-1)
#data.sales<- t(data.sales)   #We want variables as skus

# 4. Model:Evaluate Algorithms
library(FactoMineR)  # PCA algo

number_dimensions<-5
PCA.data.sales <- PCA(data.sales, 
    scale.unit= TRUE, #default value
    graph = FALSE, #provides graph, 
    ncp=number_dimensions)


#Visuals
library(factoextra)

#Plotting variance explained
fviz_screeplot(PCA.data.sales, addlabels = TRUE, ylim = c(0, 35))

#Plotting contribution of variables to first PCAs
fviz_pca_var(PCA.data.sales,
col.var = "contrib",
gradient.cols = c("#bb2e00","#002bbb"),
repel = TRUE)
            
##Plotting contribution of variables to PCAs
#10 Variables with the highest contributions to PC #1
fviz_contrib(PCA.data.sales, 
top = 10, 
choice = "var", 
axes = 1)
#10 Variables with the highest contributions to PC #2
fviz_contrib(PCA.data.sales, 
top = 10, 
choice = "var", 
axes = 2)

#Top 10 individuals with the highest contributions.
fviz_pca_ind(PCA.data.sales, 
select.ind = list(contrib = 10),
 repel = TRUE)


# a) Test options and evaluation metric
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
