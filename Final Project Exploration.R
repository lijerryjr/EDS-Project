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

#Data Wrangling
growth<-read_excel("API_NY.GDP.MKTP.KD.ZG_DS2_en_excel_v2_422103.xls")
gini<-read_excel("API_SI.POV.GINI_DS2_en_excel_v2_511456.xls")
happiness<-read_csv("share-of-people-who-say-they-are-happy.csv")
regions<-read_excel("API_SI.POV.GINI_DS2_en_excel_v2_511456.xls", sheet="Metadata - Countries")

names(regions) <- c("Code", "Region", "", "", "")
regions<-regions[c("Code", "Region")]

growth_tidy<-growth %>%
  gather(-c(`Country Name`,`Country Code`,`Indicator Name`, `Indicator Code`), 
         key="Years", value="Growth")


names(growth_tidy) <- c("Country", "Code", "", "", "Year", "Growth")
growth_tidy <- growth_tidy[c("Country", "Code", "Year", "Growth")]
growth_tidy<-growth_tidy %>% filter(!is.na(Growth))

gini_tidy<-gini%>%
  gather(-c(`Country Name`,`Country Code`,`Indicator Name`, `Indicator Code`),
         key="Years", value="Gini")

names(gini_tidy) <- c("Country", "Code", "", "", "Year", "Gini")
gini_tidy<-gini_tidy[c("Country", "Code", "Year", "Gini")]
gini_tidy<-gini_tidy %>% filter(!is.na(Gini))

names(happiness) <- c("Country", "Code", "Year", "Happiness")


final_0<-merge(gini_tidy, growth_tidy, by=c("Country", "Code", "Year"))
final_1<-merge(final_0, happiness, by=c("Country", "Code", "Year"))
final<-merge(final_1, regions, by=c("Code"))

happiness_2<-read_csv("happiness-cantril-ladder.csv")
names(happiness_2) <- c("Country", "Code", "Year", "Happiness")
final_1_t<-merge(final_0, happiness_2, by=c("Country", "Code", "Year"))
final_t<-merge(final_1_t, regions, by=c("Code"))


#1-var exploration
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

# Specific statistics for EDA
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

##2-var exploration
ggpairs(data=final_t,columns=4:6) +
  labs(title="Relationship Between Gini Coefficient, GDP Growth, and Self-Reported Life Satisfaction",
       caption="Source: Our World in Data, World Bank Databank") +
  theme_stata()


##Regression
m1<-lm(Happiness~Gini+Growth+Region, data=final_t)
summary(m1)
residualPlots(m1)
vif(m1)


m2<-lm(Happiness~Growth, data=final_t)
summary(m2)


#ML Model
##Select Data we Want
tt <- table(final_t$Code)
happiness_ml<-subset(final_t, Code %in% names(tt[tt>1])) %>%arrange(Code,Year)
happiness_ml$increased<-c(0L, happiness_ml$Happiness[-1]  > happiness_ml$Happiness[-nrow(happiness_ml)])

happiness_ml$increased <- shift(ifelse(happiness_ml$Code[2:(nrow(happiness_ml)+1)] != happiness_ml$Code[1:nrow(happiness_ml)],
                                       NA,
                                       shift(happiness_ml$increased, -1)))

happiness_ml_new<-happiness_ml

happiness_ml<-happiness_ml%>%filter(!is.na(increased))

happiness_ml %>%
  filter(Growth > -2.5) %>%
  summarize(avg_inc = mean(increased))

## Random sample indexes
set.seed(4)
train_index <- sample(1:nrow(happiness_ml), 0.5 * nrow(happiness_ml))
test_index <- setdiff(1:nrow(happiness_ml), train_index)

## Build training and testing datasets
hp_train <- happiness_ml[train_index,]

hp_test <- happiness_ml[test_index,]

##Build trees
hp.tree <- tree(factor(increased) ~ Gini+Growth+factor(Region),
                data = hp_train, method="class")
plot(hp.tree)
text(hp.tree)
hp.tree.pred <- predict(hp.tree,
                        as.data.frame(hp_test),
                        type="class")
table(hp.tree.pred, hp_test$increased)
(78+64)/(48+79+78+64)

##Pruned tree
test<-prune.tree(hp.tree, best=5)
plot(test)
text(test)
test.pred <- predict(test,
                     as.data.frame(hp_test),
                     type="class")
table(test.pred, hp_test$increased)
(104+26)/(22+117+104+26)

#Tree with Gini Growth
happiness_ml_new$GiniGrowth<-happiness_ml_new$Gini
happiness_ml_new<-happiness_ml_new %>%
  mutate(GiniGrowth=(Gini-lag(Gini))/lag(Gini))
happiness_ml_new<-happiness_ml_new%>%filter(!is.na(increased))

## Random sample indexes
set.seed(7)
traing_index <- sample(1:nrow(happiness_ml_new), 0.5 * nrow(happiness_ml_new))
testg_index <- setdiff(1:nrow(happiness_ml_new), traing_index)

## Build training and testing datasets
hp_g_train <- happiness_ml_new[traing_index,]
hp_g_test <- happiness_ml_new[testg_index,]

##Build trees
hp.g.tree <- tree(factor(increased) ~ GiniGrowth+Growth+factor(Region),
                data = hp_g_train, method="class")
plot(hp.g.tree)
text(hp.g.tree)
hp.g.tree.pred <- predict(hp.g.tree,
                          as.data.frame(hp_g_test),
                          type="class")
table(hp.g.tree.pred, hp_g_test$increased)
(66+59)/(62+82+66+59)

##Pruned tree
test1<-prune.tree(hp.g.tree, best=3)
plot(test1)
text(test1)
test1.pred <- predict(test1,
                      as.data.frame(hp_g_test),
                      type="class")
table(test1.pred, hp_g_test$increased)
(111+5)/(17+136+111+5)

#QDA Model--unused
pairs(hp_train[,c(4,5,6)],
      col=ifelse(hp_train$increased==0,"red","black"))
hp.qda <- qda(increased ~ Gini+Growth,
              data = hp_train)
hp.qda.pred <- predict(hp.qda,
                       as.data.frame(hp_test))
table(hp.qda.pred$class, hp_test$increased)
(70+52)/(56+91+70+52)

#Binary Logistic--unused
hp.logit <- glm(factor(increased) ~ Gini+Growth+factor(Region),
                data = hp_train,
                family = binomial(link = "logit"))
hp.logit.prob <- predict(hp.logit,
                         as.data.frame(hp_test),
                         type = "response")
levels(factor(hp_test$increased))
hp.logit.pred <-ifelse(hp.logit.prob > 0.5,"1","0")
table(hp.logit.pred, hp_test$increased)

