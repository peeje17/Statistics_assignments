install.packages("tidyverse")
install.packages("haven")
install.packages("labelled")
install.packages("stargazer")
install.packages("car", exclude = "recode") 



library(tidyverse)
library(haven)
library(labelled)
library(stargazer)
library(car, exclude = "recode") 
library(MASS, exclude = "select") 
library(Hmisc)
library(psych)
library(ggplot2)      
library(effsize)      
library(sjPlot)       
library(sjmisc, exclude = "replace_na")
library(TOSTER)       
library(effects)

install.packages("fixest")
install.packages("broom")
install.packages("GGally")
install.packages("tidyr")

library(fixest) 
library(broom)
library(GGally)
library(tidyr)

#install.packages("jtools")
#install.packages("ggstance")
#install.packages("sandwich")
library(jtools)
library(ggstance)
library(sandwich)
#install.packages("fixest")
library(fixest)
#install.packages("modelsummary")
library(modelsummary)

                                ### --- Loading data --- ###

df = read_dta(file = "Sesame.dta")
describe(df)
View(df)


  
                              ### --- Recoding and descriptiv analysis --- ### 

df <- mutate(df, view_enc = recode(viewenc, `1`= 1, `2` = 0))
describeBy(df, df$viewenc)
describeBy(df, df$view_enc)


df <- mutate(df, sex_new = recode(sex, `1`= 0, `2` = 1))
describeBy(df, df$sex)
describeBy(df, df$sex_new)


df <- mutate(df, setting_new = recode(setting, `1`= 0, `2` = 1))
describeBy(df, df$view_enc)
describeBy(df, df$viewenc)
describeBy(df, df$site)

describe(df)
describeBy(df, df$setting_new)


                                ### --- 2. Balance Test --- ### 
# Anova test for distribution between groups

bal.test2 <- aov(sex_new~view_enc, data=df)
summary(bal.test2) # k?n ikke-signifikant 


bal.test3 <- aov(setting_new~view_enc, data=df)
summary(bal.test3) # setting er signifikant
describeBy(df, df$setting_new)


bal.test4 <- aov(site~view_enc, data=df)
summary(bal.test4) # site er ikke-signifikant 
describeBy(df, df$site)


bal.test6 <- aov(regular~view_enc, data=df)
summary(bal.test6) # regular er signifikant  
describeBy(df, df$regular)



# Making gender, setting, view_enc, site and regular to factorvariables
df$sex_new <- as.factor(df$sex_new)
df$setting_new <- as.factor(df$setting_new)
df$view_enc <- as.factor(df$view_enc)
df$site <- as.factor(df$site)
df$regular <- as.factor(df$regular)



                                          ### --- ATE for view_enc --- ###

# Firstly a ttest for the p value bewteen view encourgement and post knowledge of numbers
t.test(df$postnumb~df$view_enc, var.equal=TRUE)



# The average treatment effect of view_enc 
plot.data.view_enc <- df %>%
  group_by(view_enc)%>%
  summarise(
    mean = mean(postnumb),
    lci = t.test(postnumb, conf.level = 0.95)$conf.int[1],
    uci = t.test(postnumb, conf.level = 0.95)$conf.int[2])
plot.data.view_enc

# Visualization 
ggplot(plot.data.view_enc, aes(view_enc, mean)) +        
  geom_point() +
  geom_errorbar(aes(ymin = lci, ymax = uci)) +
  labs(x= "Opfordring 0=nej 1=ja", y = "Viden om tal")


# OLS regression with view_enc and interaction with site
reg_view_enc2 = lm(postnumb~view_enc*site+age+sex_new+setting_new+prenumb, data=df) 
summary(reg_view_enc2)

stargazer(reg_view_enc2,regular2)
stargazer(reg_view_enc2)




                              

                                    ### --- Regular B - using OLS --- ### 

reg_regular2 = lm(postnumb~regular*site+age+sex_new+prenumb+site+setting_new, data=df) 
summary(reg_regular2)

plot_summs(reg_regular2)

stargazer(reg_view_enc2, regular2)



                                    ### --- D cluster SE for view_enc --- ### 


# Cluster for view_enc with site as interaction  
reg_cluster1 <- feols(postnumb~view_enc*site+age+sex_new+setting_new+prenumb, 
                      cluster = ~ site,  data =  df)
summary(reg_cluster1)
plot_summs(reg_cluster1)

modelsummary(reg_cluster1)



                              ### --- D cluster SE for regular --- ### 


# Cluster for regular with site as interaction  
reg_cluster2 <- feols(postnumb~regular*site+age+sex_new+setting_new+site+prenumb, 
                      cluster = ~ site,  data =  df)
summary(reg_cluster2)
plot_summs(reg_cluster2)

etable(list(reg_cluster1, reg_cluster2.1), tex=TRUE)


