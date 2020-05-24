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

# 2. Tidy data
# a) Descriptive statistics
skim(data_sales_f)
skim(data_sales_s)
skim(data)
skim(data_sku)

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
    scale_x_discrete(position = "top") +
    theme_secun()+
    theme(axis.text.x = element_text(angle = 45,size=6))
# 3. Transform Data 
# wider format for data_sales_s
data.sales<-data_sales_s %>% 
  pivot_wider(names_from =fecha ,values_from = sales) %>%
  as.data.frame()
# b) duplication
setdiff(data.sales$sku, unique(data.sales$sku) )

rownames(data.sales) <- data.sales$sku
data.sales<-select(data.sales,-1)
#data.sales<- t(data.sales)   #We want variables as skus

# wider format for data #forecasted, sales and difference
data.fs.wider<-data %>%
  select(-forecast,-sales) %>%
  pivot_wider(names_from =fecha ,values_from = f_s) %>%
  right_join(data_sku[,c(1,4)]) %>%
  as.data.frame()  %>%
  select(sku,product_range,starts_with("20"))


# b) duplication
setdiff(data.fs.wider$sku, unique(data.sales$sku) )

rownames(data.fs.wider) <- data.fs.wider$sku
#we have trasposed data - 
#Observations: months, Variables: SKUs
#data.fs.wider<-t(select(data.fs.wider,-1))

#we have NOT trasposed data - 
#Observations: SKUs, Variables: months
data.fs.wider<-select(data.fs.wider,-1)


# 4. Model:Evaluate Algorithms
library(FactoMineR)  # PCA algo

number_dimensions<-7
PCA.data.sales <- PCA(data.fs.wider, 
    quali.sup = 1,
    scale.unit= TRUE, #default value
    graph = FALSE, #provides graph, 
    ncp=number_dimensions)

summary(PCA.data.sales)

#Visuals
library(factoextra)

#Plotting variance explained
fviz_screeplot(PCA.data.sales, addlabels = TRUE, ylim = c(0, 35)) #4-5 dimensions
get_eigenvalue(PCA.data.sales) #Kaiser-Guttman rule (eignealues >1) - 8 dimension

#Identify observations with the highest contribution to first 2 PCAs
highest_ind_cont<-as.data.frame(cbind(PCA.data.sales$ind$contrib[,1],PCA.data.sales$ind$contrib[,2]))
colnames(highest_ind_cont) <-c("contrib_dim1","contrib_dim2")

obs.dim1<-head(highest_ind_cont[order(-highest_ind_cont$contrib_dim1),],7)
obs.dim2<-head(highest_ind_cont[order(-highest_ind_cont$contrib_dim2),],7)

rownames(obs.dim1)  
rownames(obs.dim2)  

#Identify vars with the highest contribution to first 2 PCAs
highest_var_cont<-as.data.frame(cbind(PCA.data.sales$var$contrib[,1],PCA.data.sales$var$contrib[,2]))
colnames(highest_var_cont) <-c("contrib_dim1","contrib_dim2")

var.dim1<-head(highest_var_cont[order(-highest_var_cont$contrib_dim1),],20)
var.dim2<-head(highest_var_cont[order(-highest_var_cont$contrib_dim2),],20)

rownames(var.dim1)  
rownames(var.dim2)  

###### Contribution of variables #####
#Plotting contribution of variables to first PCAs
fviz_pca_var(PCA.data.sales,
col.var = "contrib",
gradient.cols = c("moccasin", "grey20"),
repel = TRUE)
##Plotting contribution of variables to PCAs
#10 Variables with the highest contributions to PC #1
fviz_contrib(PCA.data.sales, 
top = 50, 
choice = "var", 
axes = 1)

#average contribution
mean(PCA.data.sales$var$contrib[,1])


#10 Variables with the highest contributions to PC #2
fviz_contrib(PCA.data.sales, 
top = 20, 
choice = "var", 
axes = 2)

#Create a barplot for the variables with the highest cos2 
#in the 1nd/2nd PC. Show the 10 highest
fviz_cos2(PCA.data.sales, 
choice = "var", 
axes = 1, 
top = 50)	

fviz_cos2(PCA.data.sales, 
choice = "var", 
axes = 2, 
top = 20)	

#Factor map  for variables with quality of representation >0.7
fviz_pca_var(PCA.data.sales, geom=c("point", "text"),
select.var = list(cos2 = 0.8), 
repel = TRUE)


###### Contribution of individuals #####
#Top 10 individuals with the highest contributions.
fviz_pca_ind(PCA.data.sales, 
select.ind = list(contrib = 10),
 repel = TRUE)

#Plotting cos2 for individuals
fviz_pca_ind(PCA.data.sales,
col.ind="cos2",
gradient.cols = c("#bb2e00", "#002bbb"),
repel = TRUE)

#Ellipsoids
aux <- as.factor(data.fs.wider$product_range)
fviz_pca_ind(PCA.data.sales, 
    label="var",
    habillage=aux,
    addEllipses=TRUE)

#Biplot with ellipsoids
fviz_pca_biplot(PCA.data.sales, 
habillage = aux, 
addEllipses = TRUE, 
alpha.var = "cos2")

#Individuals and PCAs
fviz_pca_biplot(PCA.data.sales)


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
