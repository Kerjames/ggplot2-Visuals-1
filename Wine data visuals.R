

### Data Analysis and EDA of Wine data### Assignment 1 

install.packages("tidyverse") #for data wrangling and EDA and ggplot
install.packages("lattice")   # for graphs 
install.packages("ggthemes")  # for graphs
install.packages("gridExtra") 
install.packages("corrplot") #for correlation matrix
install.packages("car")      
install.packages("digest")
install.packages("skimr")#for descriptive stats 
install.packages("plotly")
install.packages("Rmisc")

library(Rmisc)
library(plotly)
library(skimr)
library(digest)
library(gridExtra)
library(corrplot)
library(car)
library(tidyverse)
library(lattice)
library(ggthemes)


###importing the dataset

wine_raw <- read.csv("winequality.csv", head = TRUE, sep = ";") # data imported tabular form 
View(wine_raw)



### Exploring the data###

stats_export <- summary(wine_raw)

data <- wine_raw

skim_export <- skim_to_wide(data)
skim(data)

getwd()

write.csv(skim_export, file = "skim_export.csv")



#checking for correlation between predictor and target variables
#important variables will be analysed. (Gather makes data longer all variables as observations)
wine_raw %>%  
  gather(-quality, key = "key", value = "value") %>%  
  ggplot(aes(x = value, y = quality, color = key)) +
  geom_jitter(alpha = 0.3)+
  stat_smooth(method = "lm")+
  facet_wrap(~ key, scales = "free")+
  labs(title = "Relationship between Quality & Variables ")+
  theme_minimal()
  
view(wine_raw)


#analysis ofindenpendent & dependent variables
                      
table(wine_raw$quality) #quality of wine

p1 <- ggplot(data = wine_raw, aes(x = quality)) +    
  geom_bar(width = 1, color = 'black',fill = '#66CCCC') +
  theme_minimal()+
  labs(title = "Quality of Wine")

wine_quality

summary(wine_raw$quality)
  
 # Residual sugar 
p2 <- ggplot(data = wine_raw, aes(x = residual.sugar)) +
  geom_histogram(binwidth = 0.1, color = 'black',fill = ('#66CCCC')) +
  scale_x_continuous(lim = c(1,20))+
  theme_minimal()+
  labs(title = "Residual Sugar Content")+
  xlab("Residual Sugar (g)")

summary(wine_raw$residual.sugar)

wine_sugar

#alchohol
p3 <- ggplot(wine_raw, aes(x = alcohol))+
  geom_histogram(binwidth = 0.2, color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Alcohol Content")+
  xlab("Alcohol %")

# sulphates
p4 <- ggplot(wine_raw, aes(x = sulphates))+
  geom_histogram(binwidth = 0.01, color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Sulphate Concentration")+
  xlab("Sulphates")


#density

p5 <- ggplot(wine_raw, aes(x = density))+
  geom_histogram(color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Density")

#chlorides
p6 <- ggplot(wine_raw, aes(x = chlorides))+
  geom_histogram(color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Chlorides")


p7 <- ggplot(wine_raw, aes(x = citric.acid))+
  geom_histogram(color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Citric Acid")

p8 <- ggplot(wine_raw, aes(x = volatile.acidity))+
  geom_histogram(color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Volatile Acidity")
  
p9 <- ggplot(wine_raw, aes(x = pH))+
  geom_histogram(color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "pH")

p10 <- ggplot(wine_raw, aes(x = fixed.acidity))+
  geom_histogram(color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Fixed Acidity")

p11 <- ggplot(wine_raw, aes(x = total.sulfur.dioxide))+
  geom_histogram(color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Total Sulfur Dioxide")

p12 <- ggplot(wine_raw, aes(x = free.sulfur.dioxide))+
  geom_histogram(color = 'black', fill = '#66CCCC')+
  theme_minimal()+
  labs(title = "Free Sulfur Dioxide")


multiplot(p1, p2, p3, p4, p5, p6, ncol = 3 )
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, top = "Variable Distribution")
grid.arrange(p7, p8, p9, p10, p11, p12, ncol = 3)
 


# creating a correlation matrix 

wine_cor <- cor(wine_raw, method = "spearman")
corrplot(wine_cor, title = "Correlation Between the Variables")





# dividing the data into 3 groups (poor, average , good)

wine_categ <- wine_raw %>% 
  select(everything()) %>% 
   mutate(wine_standard = case_when(
     quality > 6 ~ "High Quality",
     quality > 4 ~ "Medium Quality",
     TRUE ~ "Low Quality"
   )) %>% 
  mutate(wine_standard = factor(wine_standard,
                                levels = c("Low Quality", "Medium Quality", "High Quality")))


                           
 # chlorides and density                        
ggplot(wine_categ, aes(x = chlorides))+
  geom_density(aes(fill = wine_standard), alpha = 0.6)+
  labs(title = "Density & Chlorides")+
  theme_minimal()

#Density and citcric acid

ggplot(wine_categ, aes(x = citric.acid))+
  geom_density(aes(fill = wine_standard), alpha = 0.6)+
  theme_minimal()+
  labs(title = "Density of Citric Acid")


ggplot(wine_categ, aes(x = residual.sugar))+
   geom_density(aes(fill = wine_standard), alpha = 0.6)+
   theme_minimal()+
  labs(title = "Density of Residual Sugar")



ggplot(wine_categ, aes(x = total.sulfur.dioxide))+
  geom_density(aes(fill = wine_standard), alpha = 0.6)+
  theme_minimal()



ggplot(wine_categ, aes( x = pH, y = fixed.acidity, color = wine_standard))+
  geom_point(alpha = 0.3)+
  stat_smooth(method = "lm")+
  facet_wrap(~ wine_standard)+
  theme_minimal()+
  labs(title = "pH & fixed acidity")

ggplot(wine_categ, aes(x = alcohol))+
    geom_density(aes(fill = wine_standard), alpha = 0.6)+
    theme_minimal()+
    labs(title = "Density of Alcohol Content")+
    xlab("alcohol %")


#3d Graph with 

plot_ly(wine_categ, x = ~alcohol, y = ~chlorides, z = ~density,
        type = "scatter3d", mode = "markers", color = ~wine_standard) %>% 
  layout(title = "Relationship between Alcohol, Chlorides and Density")

plot_ly(wine_categ, x = ~density, y = ~residual.sugar, z = ~fixed.acidity,
        type = "scatter3d", mode = "markers", color = ~wine_standard) %>% 
  layout(title = "redisual sugar & Fixed Acidity (High & Low Quality")


#Box plot of quality of wine

wine_raw$residual.sugar[wine_raw$residual.sugar > 20] <- NA #dropping the outliers


summary(data$residual.sugar)

ggplot(wine_raw, aes(x = as.factor(quality), y = residual.sugar, fill = as.factor(quality)))+
  geom_boxplot()+
  geom_jitter(alpha = 0.05)+
  theme_minimal()+
  scale_fill_brewer(palette="RdBu")+
  labs(title = "Quality & Residual sugar")








