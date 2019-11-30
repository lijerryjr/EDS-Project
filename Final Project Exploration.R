###Data Wrangling

library(tidyverse)
library(readxl)
library(GGally)
library(forcats)
library(ggthemes)

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
  labs(title="Self-Reported Life Satisfaction by Country Region",
       caption="Source: Our World in Data",
       x="Country Region",
       y="Self-Reported Life Satisfaction (0-10: Lowest to Highest)")+
  theme_wsj()

##Growth
ggplot(data=final_t,mapping=aes(y=Growth, x=fct_reorder(Region,Growth, .desc=TRUE))) +
  geom_boxplot() +
  coord_flip()+
  labs(title="Annual GDP Growth by Country Region",
       caption="Source: World Bank Databank",
       x="Country Region",
       y="Annual GDP Growth (%)")+
  theme_wsj()

##Gini
ggplot(data=final_t,mapping=aes(y=Gini, x=fct_reorder(Region,Gini, .desc=TRUE))) +
  geom_boxplot() +
  coord_flip()+
  labs(title="Gini Coefficient by Country Region",
       caption="Source: World Bank Databank",
       x="Country Region",
       y="Gini Coefficient")+
  theme_wsj()


##2-var exploration
ggpairs(data=final_t,columns=4:6) +
  labs(title="Relationship Between Gini Coefficient, GDP Growth, and Self-Reported Life Satisfaction",
       caption="Source: Our World in Data, World Bank Databank") +
  theme_stata()


##Regression
m1<-lm(Happiness~Gini+Growth+Region, data=final_t)
summary(m1)

