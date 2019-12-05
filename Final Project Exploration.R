#######################################################
#Load Libraries
#######################################################
library(tidyverse)
library(readxl)
library(GGally)
library(forcats)
library(ggthemes)
library(car)
library(tree)
library(klaR)
library(data.table)
library(mclust)
library(broom)

#######################################################
#Data Wrangling--Create Final Dataset
#######################################################
regions<-read_excel("API_SI.POV.GINI_DS2_en_excel_v2_511456.xls", sheet="Metadata - Countries")
growth<-read_excel("API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_422103.xls")
gini<-read_excel("API_SI.POV.GINI_DS2_en_excel_v2_511456.xls")
happiness<-read_csv("happiness-cantril-ladder.csv")

##Tidy up the Regions Dataset
names(regions) <- c("Code", "Region", "", "", "")
regions<-regions[c("Code", "Region")]

##Tidy up the GDP Growth Dataset
growth_tidy<-growth %>%
  gather(-c(`Country Name`,`Country Code`,`Indicator Name`, `Indicator Code`), 
         key="Years", value="Growth")
names(growth_tidy) <- c("Country", "Code", "", "", "Year", "Growth")
growth_tidy <- growth_tidy[c("Country", "Code", "Year", "Growth")]
growth_tidy<-growth_tidy %>% filter(!is.na(Growth))

##Tidy up the Gini Coeff Dataset
gini_tidy<-gini%>%
  gather(-c(`Country Name`,`Country Code`,`Indicator Name`, `Indicator Code`),
         key="Years", value="Gini")
names(gini_tidy) <- c("Country", "Code", "", "", "Year", "Gini")
gini_tidy<-gini_tidy[c("Country", "Code", "Year", "Gini")]
gini_tidy<-gini_tidy %>% filter(!is.na(Gini))

##Tidy up Happiness Dataset
names(happiness) <- c("Country", "Code", "Year", "Happiness")

##Merge Into One Dataset
final_0<-merge(gini_tidy, growth_tidy, by=c("Country", "Code", "Year"))
final_1_t<-merge(final_0, happiness, by=c("Country", "Code", "Year"))
final_t<-merge(final_1_t, regions, by=c("Code"))

#######################################################
#1-var EDA
#######################################################
##Happiness
ggplot(data=final_t,mapping=aes(y=Happiness, x=fct_reorder(Region,Happiness, .desc=TRUE))) +
  geom_boxplot() +
  coord_flip()+
  theme_wsj()+
  theme(title=element_text(size=18),
        axis.title=element_text(size=16, face="bold"))+
  labs(title="Self-Reported Life Satisfaction by Country Region",
       caption="Source: Our World in Data",
       x="Country Region",
       y="Self-Reported Life Satisfaction (0-10: Lowest to Highest)")

##Growth
ggplot(data=final_t,mapping=aes(y=Growth, x=fct_reorder(Region,Growth, .desc=TRUE))) +
  geom_boxplot() +
  coord_flip()+
  theme_wsj()+
  theme(title=element_text(size=18),
        axis.title=element_text(size=16, face="bold"))+
  labs(title="Annual GDP Growth by Country Region",
       caption="Source: World Bank Databank",
       x="Country Region",
       y="Annual GDP Growth (%)")
##Gini
ggplot(data=final_t,mapping=aes(y=Gini, x=fct_reorder(Region,Gini, .desc=TRUE))) +
  geom_boxplot() +
  coord_flip()+
  theme_wsj()+
  theme(title=element_text(size=18),
        axis.title=element_text(size=16, face="bold"))+
  labs(title="Gini Coefficient by Country Region",
       caption="Source: World Bank Databank",
       x="Country Region",
       y="Gini Coefficient")

##Specific statistics for EDA
final_t %>%
  filter(Region == "Europe & Central Asia") %>%
  summary()
final_t %>%
  filter(Region == "Europe & Central Asia") %>%
  arrange(desc(Happiness)) %>%
  head()
final_t %>%
  filter(Region == "Middle East & North Africa") %>%
  summary()
final_t %>%
  filter(Region == "Latin America & Caribbean") %>%
  summary()
final_t %>%
  filter(Region == "Sub-Saharan Africa") %>%
  summary()
final_t %>%
  filter(Region == "North America") %>%
  summary()

##Tally and display number of countries and data points in each region
counts <- final_t %>%
  group_by(Region, Country) %>%
  summarize() %>%
  group_by(Region) %>%
  summarize(countryCount = n())
counts$dataCount <- (final_t %>%
                       group_by(Region) %>%
                       summarize(count = n()))$count

###Number of countries in each region
counts %>%
  ggplot(aes(reorder(Region, countryCount), countryCount)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_wsj() +
  theme(title=element_text(size=18),
        axis.title=element_text(size=16, face="bold"))+
  labs(title="Number of Countries by Region",
       x="Country Region",
       y="Number of Countries")

###Number of data points in each region
counts %>%
  ggplot(aes(reorder(Region, countryCount), dataCount)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_wsj() +
  theme(title=element_text(size=18),
        axis.title=element_text(size=16, face="bold"))+
  labs(title="Number of Data Points by Region",
       x="Country Region",
       y="Number of Data Points")

###Count of countries and data points
final_t %>%
  group_by(Country) %>%
  summarize() %>%
  nrow()
final_t %>% nrow()

#######################################################
#2-var EDA
#######################################################
ggpairs(data=final_t,columns=4:6) +
  labs(title="Relationship Between Gini Coefficient, GDP Growth, and Self-Reported Life Satisfaction",
       caption="Source: Our World in Data, World Bank Databank") +
  theme_stata()

#######################################################
#Multiple Linear Regression
#######################################################
##Regression Model
m1<-lm(Happiness~Gini+Growth+Region, data=final_t)
summary(m1)

##Coefficient Plot
m.tidy <- tidy(m1, conf.int = TRUE) 
m.tidy$term <- c("Intercept",
                 "Gini", 
                 "GDP Growth", 
                 "Region: Europe + Central Asia", 
                 "Region: Latin America + Caribbean", 
                 "Region: Middle East + North Africa", 
                 "Region: North America", 
                 "Region: South Asia", 
                 "Region: Sub-Saharan Africa") #this just cleans up the names for the following visualization
m.tidyf <- m.tidy[-1,]
view(m.tidyf)
ggplot(m.tidyf, aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height =.2) +
  geom_vline(xintercept = 0)+theme_wsj() +
  xlab("Coefficient Estimate") +
  ylab("Coefficient Term")+
  ggtitle("Coefficient Plot")+
  theme(title=element_text(size=18),
        axis.title=element_text(size=16, face="bold"))

##Predicting Happiness using Gini Coefficient with Graph
predicted_data <- tibble(
  Gini = final_t$Gini,
  Growth = final_t$Growth,
  Region = final_t$Region,
  Happiness = predict(m1)
)
ggplot(data = predicted_data, mapping = aes(x = Gini, y = Happiness, color = Region)) + 
  geom_smooth(method="lm", se=FALSE)+
  theme_wsj()+
  theme(title=element_text(size=18),
        axis.title=element_text(size=16, face="bold"))+
  labs(title = 'Predicting Happiness from Gini Coefficient',
       x = "Gini Coefficient",
       y = "Happiness",
       caption = 'Source: World Bank Databank and Our World in Data')

##Showing results in a table 
stargazer(m1, 
          type="text", 
          covariate.labels = c("Gini", 
                               "GDP Growth", 
                               "Region: Europe + Central Asia", 
                               "Region: Latin America + Caribbean", 
                               "Region: Middle East + North Africa", 
                               "Region: North America", 
                               "Region: South Asia", 
                               "Region: Sub-Saharan Africa", 
                               "Intercept"))

##Graphing Growth vs Happiness
ggplot(data = predicted_data, mapping = aes(x = Growth, y = Happiness, color = Region)) + 
  geom_smooth(method="lm",se=FALSE)+
  theme_wsj()+
  theme(title=element_text(size=18),
        axis.title=element_text(size=16, face="bold"))+
  labs(title = 'Predicting Happiness from Growth Rate',
       x = "Growth Rate",
       y = "Happiness",
       caption = 'Source: World Bank Databank and Our World in Data')

##Verify Error Assumptions and Influence
residualPlots(m1)
qqPlot(m1)
vif(m1)
cookthresh <-3*(8+1)/650
infIndexPlot(m1, id = T, vars=c("Cook",  "hat"))
final_t$hat <- hatvalues(m1)
final_t %>%
  filter(hat > cookthresh)

##Additional Verification--Predicting gini by growth rates 
m0 <- lm(Gini ~ Growth, data = final_t)
stargazer(m0, type = 'text')

#######################################################
#ML Model--Classification Tree 1
#######################################################
##Select Data we Want
tt <- table(final_t$Code)
happiness_ml<-subset(final_t, Code %in% names(tt[tt>1])) %>%arrange(Code,Year)
###Impute the Increased variable
happiness_ml$increased<-c(0L, happiness_ml$Happiness[-1]  > happiness_ml$Happiness[-nrow(happiness_ml)])
happiness_ml$increased <- shift(ifelse(happiness_ml$Code[2:(nrow(happiness_ml)+1)] != happiness_ml$Code[1:nrow(happiness_ml)],
                                       NA,
                                       shift(happiness_ml$increased, -1)))
happiness_ml_new<-happiness_ml #This will be used in our pruned tree
happiness_ml<-happiness_ml%>%filter(!is.na(increased))
happiness_ml %>%
  filter(Growth > -2.5) %>%
  summarize(avg_inc = mean(increased)) #used to visually verify our subsetting was correct 

##Random sample to build testing and training datasets
set.seed(4)
train_index <- sample(1:nrow(happiness_ml), 0.5 * nrow(happiness_ml))
test_index <- setdiff(1:nrow(happiness_ml), train_index)
hp_train <- happiness_ml[train_index,]
hp_test <- happiness_ml[test_index,]

##Build and visualize tree
hp.tree <- tree(factor(increased) ~ Gini+Growth+factor(Region),
                data = hp_train, method="class")
plot(hp.tree)
text(hp.tree)
hp.tree.pred <- predict(hp.tree,
                        as.data.frame(hp_test),
                        type="class")
table(hp.tree.pred, hp_test$increased)
(78+64)/(48+79+78+64) #error rate from above table

#######################################################
#ML Model--Classification Tree 2, with GiniChange and Prune
#######################################################
##Impute GiniChange variable
happiness_ml_new$GiniGrowth<-happiness_ml_new$Gini
happiness_ml_new<-happiness_ml_new %>%
  mutate(GiniGrowth=(Gini-lag(Gini))/lag(Gini))
happiness_ml_new<-happiness_ml_new%>%filter(!is.na(increased))

##Random sample to build testing and training datasets
set.seed(7)
traing_index <- sample(1:nrow(happiness_ml_new), 0.5 * nrow(happiness_ml_new))
testg_index <- setdiff(1:nrow(happiness_ml_new), traing_index)
hp_g_train <- happiness_ml_new[traing_index,]
hp_g_test <- happiness_ml_new[testg_index,]

##Build and visualize trees
hp.g.tree <- tree(factor(increased) ~ GiniGrowth+Growth+factor(Region),
                data = hp_g_train, method="class")
plot(hp.g.tree)
text(hp.g.tree)
hp.g.tree.pred <- predict(hp.g.tree,
                          as.data.frame(hp_g_test),
                          type="class")
table(hp.g.tree.pred, hp_g_test$increased)
(66+59)/(62+82+66+59) #error rate from above table

##Pruned tree
test1<-prune.tree(hp.g.tree, best=3)
plot(test1)
text(test1)
test1.pred <- predict(test1,
                      as.data.frame(hp_g_test),
                      type="class")
table(test1.pred, hp_g_test$increased)
(111+5)/(17+136+111+5) #error rate from above table

#######################################################
#Appendix--Additional Interesting Models that were Ultimately Kinda Trash
#######################################################
##QDA Model
pairs(hp_train[,c(4,5,6)],
      col=ifelse(hp_train$increased==0,"red","black"))
hp.qda <- qda(increased ~ Gini+Growth,
              data = hp_train)
hp.qda.pred <- predict(hp.qda,
                       as.data.frame(hp_test))
table(hp.qda.pred$class, hp_test$increased)
(70+52)/(56+91+70+52)

##Binary Logistic
hp.logit <- glm(factor(increased) ~ Gini+Growth+factor(Region),
                data = hp_train,
                family = binomial(link = "logit"))
hp.logit.prob <- predict(hp.logit,
                         as.data.frame(hp_test),
                         type = "response")
levels(factor(hp_test$increased))
hp.logit.pred <-ifelse(hp.logit.prob > 0.5,"1","0")
table(hp.logit.pred, hp_test$increased)
