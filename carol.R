library(psych)
library(tidyverse)
carol <- read.csv("carol clean 1.csv")

#recode empathy group ---------------------------------

carol <- carol%>% # Save results back into data frame
  mutate( # Calculate new variables
    Empathy_Group = recode(Empathy1_no0, # Recode into new variable called gender
                              `1` = "Induced", # Coding 0s as 'male'
                              '0' = "Not Induced",
                              .default = "Other")) # Code everything else as 'Other'


carol$Empathy_Group %>% head()

#recode friend group ------------------------------------


carol <- carol%>% # Save results back into data frame
  mutate( # Calculate new variables
    Friend_Group = recode(friend1stranger0, # Recode into new variable called gender
                           `1` = "Friend", # Coding 0s as 'male'
                           '0' = "Stranger",
                           .default = "Other")) # Code everything else as 'Other'
carol$Friend_Group %>% head()

#reverse code SIS -------------------------------------

carol$SIS_2R<-6-carol$SIS_2
carol$SIS_10R<-6-carol$SIS_10
carol$SIS_18R<-6-carol$SIS_18
carol$SIS_2_2R<-6-carol$SIS_2_2
carol$SIS_2_4R<-6-carol$SIS_2_4
carol$SIS_2_6R<-6-carol$SIS_2_6
carol$SIS_7R<-6-carol$SIS_7
carol$SIS_15R<-6-carol$SIS_15
carol$SIS_23R<-6-carol$SIS_23
carol$SIS_1R<-6-carol$SIS_1
carol$SIS_9R<-6-carol$SIS_9
carol$SIS_17R<-6-carol$SIS_17
carol$SIS_4R<-6-carol$SIS_4
carol$SIS_12R<-6-carol$SIS_12
carol$SIS_20R<-6-carol$SIS_20


#make SIS scale -----------------------------

carol<-carol %>%
  mutate(SIS_scale = rowMeans(
    across(c(SIS_1R,SIS_2R,SIS_3,SIS_4R,SIS_5,SIS_6,SIS_7R,SIS_8,SIS_9R,SIS_10R,
             SIS_11,SIS_12R,SIS_13,SIS_14,SIS_15R,SIS_16,SIS_17R,SIS_18R,SIS_19,SIS_20R,
    ))))
    
carol$SIS_scale %>% head()
carol$SIS_scale %>% tail()

#check willingness to help ----------------------

carol$will_to_help %>% head()


#ANOVA for willingness to help -------------------------------------------

aov7 <- aov(will_to_help ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
summary(aov7)

# 
# Df Sum Sq Mean Sq F value Pr(>F)  
# Empathy_Group                1    5.4   5.429   1.254 0.2655  
# Friend_Group                 1   22.9  22.871   5.282 0.0236 *
#   Empathy_Group:Friend_Group   1    8.9   8.948   2.067 0.1537  
# Residuals                  100  433.0   4.330                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 1 observation deleted due to missingness

#ANOVA for SIS ----------------------

aov2 <- aov(SIS_scale ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
summary(aov2)
# 
# Df Sum Sq Mean Sq F value Pr(>F)
# Empathy_Group               1  0.046 0.04563   0.188  0.666
# Friend_Group                1  0.064 0.06359   0.262  0.610
# Empathy_Group:Friend_Group  1  0.058 0.05839   0.241  0.625
# Residuals                  75 18.176 0.24235               
# 26 observations deleted due to missingness
etaSquared(aov2)

#fix SIS scale

carol<-carol %>%
  mutate(SIS_scale1 = rowMeans(
    across(c(SIS_1R,SIS_2R,SIS_3,SIS_4R,SIS_5,SIS_6,SIS_7R,SIS_8,SIS_9R,SIS_10R,
             SIS_11,SIS_12R,SIS_13,SIS_14,SIS_15R,SIS_16,SIS_17R,SIS_18R,SIS_19,SIS_20R,
             SIS_21,SIS_22,SIS_23R,SIS_24)
    )))
    
carol$SIS_scale1 %>% head()

#ANOVA for first 24 of SIS

aov2 <- aov(SIS_scale1 ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
summary(aov2)


# Df Sum Sq Mean Sq F value Pr(>F)
# Empathy_Group               1  0.120 0.12039   1.054  0.307
# Friend_Group                1  0.179 0.17904   1.567  0.214
# Empathy_Group:Friend_Group  1  0.009 0.00858   0.075  0.785
# Residuals                  92 10.509 0.11423               
# 9 observations deleted due to missingness


#T test for empathy check ------------------------

t.test(empathy_check ~ Empathy_Group, data = carol)
# Welch Two Sample t-test
# 
# data:  empathy_check by Empathy_Group
# t = 0.30916, df = 102.65, p-value = 0.7578
# alternative hypothesis: true difference in means between group Induced and group Not Induced is not equal to 0
# 95 percent confidence interval:
#   -0.2018835  0.2764449
# sample estimates:
#   mean in group Induced mean in group Not Induced 
# 3.578947                  3.541667 


#Make SIS scale for last 5 questions-----------

carol<-carol %>%
  mutate(SIS_scale2 = rowMeans(
    across(c(SIS_2_1,SIS_2_2R,SIS_2_3,SIS_2_4R,SIS_2_5,SIS_2_6R)
    )))

carol$SIS_scale2 %>% head()
carol$SIS_scale2 %>% tail()

describe(carol$SIS_scale2)

# vars  n mean   sd median trimmed  mad min max range  skew kurtosis  se
# X1    1 84 3.74 1.87   3.75    3.83 2.35   0 6.5   6.5 -0.29    -1.02 0.2


#try ANOVA for last 5 of SIS

aov3 <- aov(SIS_scale2 ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
summary(aov3)

# Df Sum Sq Mean Sq F value Pr(>F)
# Empathy_Group               1   0.31   0.313   0.087  0.768
# Friend_Group                1   1.39   1.388   0.388  0.535
# Empathy_Group:Friend_Group  1   1.91   1.912   0.534  0.467
# Residuals                  80 286.24   3.578               
# 21 observations deleted due to missingness


-#see if empathy check and willingness to help are correlated------------

cor.test(x = carol$empathy_check,
         y = carol$will_to_help)


# 
# Pearsons product-moment correlation
# 
# data:  carol$empathy_check and carol$will_to_help
# t = 5.7453, df = 102, p-value = 9.561e-08
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.3336472 0.6273135
# sample estimates:
#       cor 
# 0.4944629 


#see if empathy check and SIS are correlated

cor.test(x = carol$empathy_check,
         y = carol$SIS_scale1)

#Pearson's product-moment correlation

            # data:  carol$empathy_check and carol$SIS_scale1
            # t = 3.079, df = 94, p-value = 0.002722
            # alternative hypothesis: true correlation is not equal to 0
            # 95 percent confidence interval:
            #  0.1087987 0.4743812
            # sample estimates:
            #       cor 
            # 0.3026824 
            
cor.test(x = carol$empathy_check,
          y = carol$SIS_scale2)

#Pearson's product-moment correlation

            # data:  carol$empathy_check and carol$SIS_scale2
            # t = 1.8909, df = 82, p-value = 0.06217
            # alternative hypothesis: true correlation is not equal to 0
            # 95 percent confidence interval:
            #  -0.01045085  0.40121511
            # sample estimates:
            #       cor 
            # 0.2044023 

#median split on empathy check -------------

carol <- carol %>% 
  mutate(empathy_msplit = cut_number(empathy_check, n = 2, labels = c("Low","High")))
  
median(carol$empathy_check) #median is 4
mean(carol$empathy_check) #mean is 3.561905

carol <- carol %>% 
  mutate(empathy_msplit = cut_number(empathy_check, n = 2, labels = c("Low","High")))

# it didn't work bc insufficient data to produce two bins


#see if age and willingness to help are correlated-----------------

cor.test(x = carol$age,
         y = carol$will_to_help)


# 
# Pearson's product-moment correlation
# 
# data:  carol$age and carol$will_to_help
# t = -0.83386, df = 96, p-value = 0.4064
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.2785329  0.1155666
# sample estimates:
#         cor 
# -0.08479867 '

#they are not

#see if gender difference-----------

#recode gender

carol <- carol%>% # Save results back into data frame
  mutate( # Calculate new variables
    gender = recode(gender, # Recode into new variable called gender
                          `1` = "Male", # Coding 0s as 'male'
                          '2' = "Female",
                          .default = "Other")) # Code everything else as 'Other'

carol %>% # Take our data frame
  filter(gender != "Other") %>% # Drop 'Other' from gender
  group_by(gender) %>% # Run analyses separately for male and female Ps
  summarise(
    wth_mean = mean(will_to_help, na.rm = T), # Get mean of depression scores
    wth_SD = sd(will_to_help, na.rm = T) # Get SD of depression scores
  )

# gender wth_mean wth_SD
# <chr>     <dbl>  <dbl>
#   1 Female     8.36   1.88
# 2 Male       7.63   2.66

t.test(will_to_help ~ gender, data = carol)

t.test(empathy_check ~ gender, data = carol)







#see if friend gender matters --------------------------


carol <- carol%>% # Save results back into data frame
  mutate( # Calculate new variables
    friend_gender = recode(friend_gender, 
                    `1` = "Male", # Coding 0s as 'male'
                    '2' = "Female",
                    .default = "Other")) # Code everything else as 'Other'

carol %>% # Take our data frame
  filter(friend_gender != "Other") %>% # Drop 'Other' from gender
  group_by(friend_gender) %>% # Run analyses separately for male and female Ps
  summarise(
    wth_mean = mean(will_to_help, na.rm = T), # Get mean of depression scores
    wth_SD = sd(will_to_help, na.rm = T) # Get SD of depression scores
  )
# 
# friend_gender wth_mean wth_SD
# <chr>            <dbl>  <dbl>
#   1 Female            8.89   1.74
# 2 Male              8      1.77

aov4<-aov(will_to_help ~ gender + friend_gender + gender:friend_gender, data=carol)
summary(aov4)

#                       Df Sum Sq Mean Sq F value Pr(>F)
# gender                2   9.97   4.986   1.568  0.219
# friend_gender         2   2.90   1.448   0.455  0.637
# gender:friend_gender  1   1.57   1.572   0.495  0.485
# Residuals            47 149.45   3.180               
# 52 observations deleted due to missingness
# 

aov5<-aov(SIS_scale1 ~ gender + friend_gender + gender:friend_gender, data=carol)
summary(aov5)
# Df Sum Sq Mean Sq F value Pr(>F)  
# gender                2  0.139  0.0694   0.631 0.5367  
# friend_gender         2  0.678  0.3391   3.088 0.0559 .
# gender:friend_gender  1  0.002  0.0023   0.021 0.8857  
# Residuals            43  4.723  0.1098                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 56 observations deleted due to missingness

#marginal results from friend gender on SIS and interaction---------

#check SIS subscales --------------

# Mutual Dependence 2R, 6, 10R, 14, 18R, 22
# Power 25, 26R, 27, 28R, 29, 30R
# Conflict 3, 7R, 11, 15R, 19, 23R
# Future Interdependence 1R, 5, 9R, 13, 17R, 21
# Information Certainty 4R, 8, 12R, 16, 20R, 24

#conflict subscale

carol<-carol %>%
  mutate(SIS_Conflict = rowMeans(
    across(c(SIS_3,SIS_7R,
             SIS_11,SIS_15R,SIS_19,SIS_23R)
    )))
aovconflict<-aov(SIS_Conflict ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data = carol)
summary(aovconflict)
# Df Sum Sq Mean Sq F value Pr(>F)
# Empathy_Group               1   0.07  0.0700   0.160  0.690
# Friend_Group                1   0.27  0.2745   0.626  0.431
# Empathy_Group:Friend_Group  1   0.31  0.3091   0.705  0.403
# Residuals                  96  42.07  0.4382               
# 5 observations deleted due to missingness

#mutual dependence subscale

carol<-carol %>%
  mutate(SIS_MutDependence = rowMeans(
    across(c(SIS_2R,SIS_6,
             SIS_10R,SIS_14,SIS_18R,SIS_22)
    )))

aovmutdepend<-aov(SIS_MutDependence ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data = carol)
summary(aovmutdepend)

# Df Sum Sq Mean Sq F value Pr(>F)
# Empathy_Group               1   0.46  0.4586   1.132  0.290
# Friend_Group                1   0.19  0.1915   0.473  0.493
# Empathy_Group:Friend_Group  1   0.14  0.1379   0.340  0.561
# Residuals                  97  39.30  0.4051               
# 4 observations deleted due to missingness

#future interdepenence subscale 1R, 5, 9R, 13, 17R, 21
carol<-carol %>%
  mutate(SIS_futint = rowMeans(
    across(c(SIS_1R,SIS_5,
             SIS_9R,SIS_13,SIS_17R,SIS_21)
    )))

aovfutint<-aov(SIS_futint ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data = carol)
summary(aovfutint)

# Df Sum Sq Mean Sq F value Pr(>F)
# Empathy_Group               1   0.08  0.0844   0.178  0.674
# Friend_Group                1   0.04  0.0368   0.078  0.781
# Empathy_Group:Friend_Group  1   0.24  0.2440   0.515  0.475
# Residuals                  95  45.04  0.4741               
# 6 observations deleted due to missingness


#Information Certainty 4R, 8, 12R, 16, 20R, 24

carol<-carol %>%
  mutate(SIS_infocert = rowMeans(
    across(c(SIS_4R,SIS_8,
             SIS_12R,SIS_16,SIS_20R,SIS_24)
    )))

aovinfocert<-aov(SIS_infocert ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data = carol)
summary(aovinfocert)

#significant difference for friend group
#                               Df Sum Sq Mean Sq F value Pr(>F)   
#   Empathy_Group               1   0.70   0.702   1.499 0.2238   
#   Friend_Group                1   4.28   4.285   9.148 0.0032 **
#   Empathy_Group:Friend_Group  1   0.80   0.799   1.707 0.1946   
#   Residuals                  95  44.50   0.468                  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
t.test(SIS_infocert ~ Friend_Group, data = carol)
# 
#           Welch Two Sample t-test
#           data:  SIS_infocert by Friend_Group
#           t = 3.2193, df = 96.994, p-value = 0.001748
#           alternative hypothesis: true difference in means between group Friend and group Stranger is not equal to 0
#           95 percent confidence interval:
#            0.1696593 0.7151446
#           sample estimates:
#           mean in group Friend mean in group Stranger 
#           3.754902               3.312500 
          
#check if our dependent variables were correlated------------------         
cor.test(x = carol$will_to_help, y = carol$SIS_scale)


        #Pearson's product-moment correlation
        
        # data:  carol$will_to_help and carol$SIS_scale
        # t = 2.0603, df = 77, p-value = 0.04275
        # alternative hypothesis: true correlation is not equal to 0
        # 95 percent confidence interval:
        #  0.007858903 0.428049061
        # sample estimates:
        #       cor 
        # 0.2285721 
        
        
        #try with first 24 questions
cor.test(x = carol$will_to_help, y = carol$SIS_scale1)
  #	Pearson's product-moment correlation

          data:  carol$will_to_help and carol$SIS_scale1
          t = 2.2176, df = 94, p-value = 0.02899
          alternative hypothesis: true correlation is not equal to 0
          95 percent confidence interval:
          0.02353773 0.40533775
          sample estimates:
          cor 
          0.2229715 
          
#get means--------------
          
          
#plot graphs------------------
          
t.test(will_to_help ~ Friend_Group, data = carol)         

          # 
          # Welch Two Sample t-test
          # 
          # data:  carol$will_to_help by carol$Friend_Group
          # t = 2.464, df = 92.714, p-value = 0.01558
          # alternative hypothesis: true difference in means between group Friend and group Stranger is not equal to 0
          # 95 percent confidence interval:
          #   0.1966137 1.8300234
          # sample estimates:
          #   mean in group Friend mean in group Stranger 
          # 8.660377               7.647059 
          # 



carol_friend <- carol %>% filter(Friend_Group == "Friend")
cor.test(carol_friend$empathy_check, carol_friend$will_to_help)

carol_stranger <- carol %>% filter(Friend_Group == "Stranger")
cor.test(carol_stranger$empathy_check, carol_stranger$will_to_help)

mod <- lm(will_to_help ~ Friend_Group*empathy_check, data = carol)
anova(mod)

# Analysis of Variance Table
# 
# Response: will_to_help
# Df Sum Sq Mean Sq F value    Pr(>F)    
# Friend_Group                 1  26.69  26.687  7.9436  0.005817 ** 
#   empathy_check                1 104.60 104.597 31.1337 2.064e-07 ***
#   Friend_Group:empathy_check   1   2.98   2.977  0.8862  0.348786    
# Residuals                  100 335.96   3.360                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


aov8<-aov(SIS_scale ~ Empathy_Group*Friend_Group, data=carol)
summary(aov8)

cor.test(x = carol$empathy_check,
         y = carol$SIS_scale)

# Pearson's product-moment correlation
# 
# data:  carol$empathy_check and carol$SIS_scale
# t = 3.1155, df = 77, p-value = 0.00258
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1225371 0.5174146
# sample estimates:
#       cor 
# 0.3345823 



ggplot(data = carol, aes(x = empathy_check, y = will_to_help)) +
  geom_point()



R version 4.3.0 (2023-04-21) -- "Already Tomorrow"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: aarch64-apple-darwin20 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

> library(psych)
> library(tidyverse)
── Attaching core tidyverse packages ───────────────────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.2     ✔ readr     2.1.4
✔ forcats   1.0.0     ✔ stringr   1.5.0
✔ ggplot2   3.4.2     ✔ tibble    3.2.1
✔ lubridate 1.9.2     ✔ tidyr     1.3.0
✔ purrr     1.0.1     
── Conflicts ─────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ ggplot2::%+%()   masks psych::%+%()
✖ ggplot2::alpha() masks psych::alpha()
✖ dplyr::filter()  masks stats::filter()
✖ dplyr::lag()     masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
> carol <- read.csv("carol clean 1.csv")
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
In file(file, "rt") :
  cannot open file 'carol clean 1.csv': No such file or directory
> setwd("/Users/shawnalarsen/Downloads")
> library(readr)
> carol_clean_1 <- read_csv("psy 120L/carol clean 1.csv")
Rows: 105 Columns: 37                                                                                      
── Column specification ─────────────────────────────────────────────────────────────────────────────────────
Delimiter: ","
dbl (37): age, gender, friend_gender, will_to_help, empathy_check, SIS_1, SIS_2, SIS_3, SIS_4, SIS_5, SIS...

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
> View(carol_clean_1)
> carol <- read.csv("carol clean 1.csv")
> #recode empathy group ---------------------------------
> 
> carol <- carol%>% # Save results back into data frame
+   mutate( # Calculate new variables
+     Empathy_Group = recode(Empathy1_no0, # Recode into new variable called gender
+                               `1` = "Induced", # Coding 0s as 'male'
+                               '0' = "Not Induced",
+                               .default = "Other")) # Code everything else as 'Other'
> 
> 
> carol$Empathy_Group %>% head()
[1] "Not Induced" "Induced"     "Induced"     "Not Induced" "Induced"     "Not Induced"
> 
> #recode friend group ------------------------------------
> 
> 
> carol <- carol%>% # Save results back into data frame
+   mutate( # Calculate new variables
+     Friend_Group = recode(friend1stranger0, # Recode into new variable called gender
+                            `1` = "Friend", # Coding 0s as 'male'
+                            '0' = "Stranger",
+                            .default = "Other")) # Code everything else as 'Other'
> carol$Friend_Group %>% head()
[1] "Stranger" "Friend"   "Friend"   "Stranger" "Friend"   "Stranger"
> 
> #reverse code SIS -------------------------------------
> ``
Error: attempt to use zero-length variable name
> library(psych)
> library(tidyverse)
> carol <- read.csv("carol clean 1.csv")
> 
> #recode empathy group ---------------------------------
> 
> carol <- carol%>% # Save results back into data frame
+   mutate( # Calculate new variables
+     Empathy_Group = recode(Empathy1_no0, # Recode into new variable called gender
+                               `1` = "Induced", # Coding 0s as 'male'
+                               '0' = "Not Induced",
+                               .default = "Other")) # Code everything else as 'Other'
> 
> 
> carol$Empathy_Group %>% head()
[1] "Not Induced" "Induced"     "Induced"     "Not Induced" "Induced"     "Not Induced"
> 
> #recode friend group ------------------------------------
> 
> 
> carol <- carol%>% # Save results back into data frame
+   mutate( # Calculate new variables
+     Friend_Group = recode(friend1stranger0, # Recode into new variable called gender
+                            `1` = "Friend", # Coding 0s as 'male'
+                            '0' = "Stranger",
+                            .default = "Other")) # Code everything else as 'Other'
> carol$Friend_Group %>% head()
[1] "Stranger" "Friend"   "Friend"   "Stranger" "Friend"   "Stranger"
> 
> #reverse code SIS -------------------------------------
> ``
Error: attempt to use zero-length variable name
> library(psych)
> library(tidyverse)
> carol <- read.csv("carol clean 1.csv")
> carol <- carol%>% # Save results back into data frame
+   mutate( # Calculate new variables
+     Empathy_Group = recode(Empathy1_no0, # Recode into new variable called gender
+                               `1` = "Induced", # Coding 0s as 'male'
+                               '0' = "Not Induced",
+                               .default = "Other")) # Code everything else as 'Other'
> carol$Empathy_Group %>% head()
[1] "Not Induced" "Induced"     "Induced"     "Not Induced" "Induced"     "Not Induced"
> carol <- carol%>% # Save results back into data frame
+   mutate( # Calculate new variables
+     Friend_Group = recode(friend1stranger0, # Recode into new variable called gender
+                            `1` = "Friend", # Coding 0s as 'male'
+                            '0' = "Stranger",
+                            .default = "Other")) # Code everything else as 'Other'
> carol$Friend_Group %>% head()
[1] "Stranger" "Friend"   "Friend"   "Stranger" "Friend"   "Stranger"
> #reverse code SIS -------------------------------------
> ``
Error: attempt to use zero-length variable name
> carol$SIS_2R<-6-carol$SIS_2
> carol$SIS_10R<-6-carol$SIS_10
> carol$SIS_18R<-6-carol$SIS_18
> carol$SIS_2_2R<-6-carol$SIS_2_2
> carol$SIS_2_4R<-6-carol$SIS_2_4
> carol$SIS_2_6R<-6-carol$SIS_2_6
> carol$SIS_7R<-6-carol$SIS_7
> carol$SIS_15R<-6-carol$SIS_15
> carol$SIS_23R<-6-carol$SIS_23
> carol$SIS_1R<-6-carol$SIS_1
> carol$SIS_9R<-6-carol$SIS_9
> carol$SIS_17R<-6-carol$SIS_17
> carol$SIS_4R<-6-carol$SIS_4
> carol$SIS_12R<-6-carol$SIS_12
> carol$SIS_20R<-6-carol$SIS_20
> #make SIS scale -----------------------------
> carol<-carol %>%
+     across(c(SIS_1R,SIS_2R,SIS_3,SIS_4R,SIS_5,SIS_6,SIS_7R,SIS_8,SIS_9R,SIS_10R,
+              SIS_21,SIS_22,SIS_23R,SIS_24,SIS_2_1,SIS_2_2R,SIS_2_3,SIS_2_4R,SIS_2_5,SIS_2_6R)
+ carol$SIS_scale %>% head()
Error: unexpected symbol in:
"             SIS_21,SIS_22,SIS_23R,SIS_24,SIS_2_1,SIS_2_2R,SIS_2_3,SIS_2_4R,SIS_2_5,SIS_2_6R)
carol"
> carol$SIS_scale %>% tail()
NULL
> carol$will_to_help %>% head()
[1] 10 10 10  7 10 10
> aov2 <- aov(will_to_help ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
> summary(aov2)
                            Df Sum Sq Mean Sq F value Pr(>F)  
Empathy_Group                1    5.4   5.429   1.254 0.2655  
Friend_Group                 1   22.9  22.871   5.282 0.0236 *
Empathy_Group:Friend_Group   1    8.9   8.948   2.067 0.1537  
Residuals                  100  433.0   4.330                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1 observation deleted due to missingness
> aov1 <- aov(SIS_scale ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
Error in eval(predvars, data, env) : object 'SIS_scale' not found
> summary(aov2)
                            Df Sum Sq Mean Sq F value Pr(>F)  
Empathy_Group                1    5.4   5.429   1.254 0.2655  
Friend_Group                 1   22.9  22.871   5.282 0.0236 *
Empathy_Group:Friend_Group   1    8.9   8.948   2.067 0.1537  
Residuals                  100  433.0   4.330                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1 observation deleted due to missingness
> # 
> # 
> # Df Sum Sq Mean Sq F value Pr(>F)
> # Empathy_Group               1  0.046 0.04563   0.188  0.666
> # 
> # Df Sum Sq Mean Sq F value Pr(>F)
> # Empathy_Group               1  0.046 0.04563   0.188  0.666
> # Friend_Group                1  0.064 0.06359   0.262  0.610
> # Empathy_Group:Friend_Group  1  0.058 0.05839   0.241  0.625
> # 
> # Df Sum Sq Mean Sq F value Pr(>F)
> # Empathy_Group               1  0.046 0.04563   0.188  0.666
> # Friend_Group                1  0.064 0.06359   0.262  0.610
> # Empathy_Group:Friend_Group  1  0.058 0.05839   0.241  0.625
> # Residuals                  75 18.176 0.24235               
> # 26 observations deleted due to missingness
> #fix SIS scale
> carol<-carol %>%
+     across(c(SIS_1R,SIS_2R,SIS_3,SIS_4R,SIS_5,SIS_6,SIS_7R,SIS_8,SIS_9R,SIS_10R,
+              SIS_21,SIS_22,SIS_23R,SIS_24)
+ carol$SIS_scale1 %>% head()
Error: unexpected symbol in:
"             SIS_21,SIS_22,SIS_23R,SIS_24)
carol"
> aov2 <- aov(SIS_scale1 ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
Error in eval(predvars, data, env) : object 'SIS_scale1' not found
> summary(aov2)
                            Df Sum Sq Mean Sq F value Pr(>F)  
Empathy_Group                1    5.4   5.429   1.254 0.2655  
Friend_Group                 1   22.9  22.871   5.282 0.0236 *
Empathy_Group:Friend_Group   1    8.9   8.948   2.067 0.1537  
Residuals                  100  433.0   4.330                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
1 observation deleted due to missingness
> t.test(empathy_check ~ Empathy_Group, data = carol)

	Welch Two Sample t-test

data:  empathy_check by Empathy_Group
t = 0.30916, df = 102.65, p-value = 0.7578
alternative hypothesis: true difference in means between group Induced and group Not Induced is not equal to 0
95 percent confidence interval:
 -0.2018835  0.2764449
sample estimates:
    mean in group Induced mean in group Not Induced 
                 3.578947                  3.541667 

> # Welch Two Sample t-test
> # Welch Two Sample t-test
> # 
> # data:  empathy_check by Empathy_Group
> # Welch Two Sample t-test
> # 
> # data:  empathy_check by Empathy_Group
> # t = 0.30916, df = 102.65, p-value = 0.7578
> # alternative hypothesis: true difference in means between group Induced and group Not Induced is not equal to 0
> # Welch Two Sample t-test
> # 
> # data:  empathy_check by Empathy_Group
> # t = 0.30916, df = 102.65, p-value = 0.7578
> # alternative hypothesis: true difference in means between group Induced and group Not Induced is not equal to 0
> # 95 percent confidence interval:
> #   -0.2018835  0.2764449
> # Welch Two Sample t-test
> # 
> # data:  empathy_check by Empathy_Group
> # t = 0.30916, df = 102.65, p-value = 0.7578
> # alternative hypothesis: true difference in means between group Induced and group Not Induced is not equal to 0
> # 95 percent confidence interval:
> #   -0.2018835  0.2764449
> # sample estimates:
> #   mean in group Induced mean in group Not Induced 
> #Make SIS scale for last 5 questions-----------
> carol<-carol %>%
+     across(c(SIS_2_1,SIS_2_2R,SIS_2_3,SIS_2_4R,SIS_2_5,SIS_2_6R)
+ carol$SIS_scale2 %>% head()
Error: unexpected symbol in:
"    across(c(SIS_2_1,SIS_2_2R,SIS_2_3,SIS_2_4R,SIS_2_5,SIS_2_6R)
carol"
> carol$SIS_scale2 %>% tail()
NULL
> describe(carol$SIS_scale2)
Error in stats[1, 3] <- median(x, na.rm = na.rm) : 
  number of items to replace is not a multiple of replacement length
In addition: Warning message:
In mean.default(x, na.rm = na.rm) :
  argument is not numeric or logical: returning NA
> aov3 <- aov(SIS_scale2 ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
Error in eval(predvars, data, env) : object 'SIS_scale2' not found
> summary(aov3)
Error: object 'aov3' not found
> cor.test(x = carol$empathy_check,
+          y = carol$will_to_help)

	Pearson's product-moment correlation

data:  carol$empathy_check and carol$will_to_help
t = 5.7453, df = 102, p-value = 9.561e-08
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.3336472 0.6273135
sample estimates:
      cor 
0.4944629 

> cor.test(x = carol$empathy_check,
+          y = carol$SIS_scale1)
Error in cor.test.default(x = carol$empathy_check, y = carol$SIS_scale1) : 
  'y' must be a numeric vector
> cor.test(x = carol$empathy_check,
+           y = carol$SIS_scale2)
Error in cor.test.default(x = carol$empathy_check, y = carol$SIS_scale2) : 
  'y' must be a numeric vector
> carol <- carol %>% 
+   mutate(empathy_msplit = cut_number(empathy_check, n = 2, labels = c("Low","High")))
Error in `mutate()`:
ℹ In argument: `empathy_msplit = cut_number(empathy_check, n = 2, labels = c("Low", "High"))`.
Caused by error in `cut_number()`:
! Insufficient data values to produce 2 bins.
Run `rlang::last_trace()` to see where the error occurred.
> median(carol$empathy_check) #median is 4
[1] 4
> mean(carol$empathy_check) #mean is 3.561905
[1] 3.561905
> carol <- carol %>% 
+   mutate(empathy_msplit = cut_number(empathy_check, n = 2, labels = c("Low","High")))
Error in `mutate()`:
ℹ In argument: `empathy_msplit = cut_number(empathy_check, n = 2, labels = c("Low", "High"))`.
Caused by error in `cut_number()`:
! Insufficient data values to produce 2 bins.
Run `rlang::last_trace()` to see where the error occurred.
> cor.test(x = carol$age,
+          y = carol$will_to_help)

	Pearson's product-moment correlation

data:  carol$age and carol$will_to_help
t = -0.83386, df = 96, p-value = 0.4064
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.2785329  0.1155666
sample estimates:
        cor 
-0.08479867 

> carol <- carol%>% # Save results back into data frame
+   mutate( # Calculate new variables
+     gender = recode(gender, # Recode into new variable called gender
+                           `1` = "Male", # Coding 0s as 'male'
+                           '2' = "Female",
+                           .default = "Other")) # Code everything else as 'Other'
> carol %>% # Take our data frame
+   filter(gender != "Other") %>% # Drop 'Other' from gender
+   group_by(gender) %>% # Run analyses separately for male and female Ps
+   summarise(
+     wth_mean = mean(will_to_help, na.rm = T), # Get mean of depression scores
+     wth_SD = sd(will_to_help, na.rm = T) # Get SD of depression scores
+   )
# A tibble: 2 × 3
  gender wth_mean wth_SD
  <chr>     <dbl>  <dbl>
1 Female     8.36   1.88
2 Male       7.63   2.66
> t.test(will_to_help ~ gender, data = carol)
Error in t.test.formula(will_to_help ~ gender, data = carol) : 
  grouping factor must have exactly 2 levels
> t.test(empathy_check ~ gender, data = carol)
Error in t.test.formula(empathy_check ~ gender, data = carol) : 
  grouping factor must have exactly 2 levels
> carol <- carol%>% # Save results back into data frame
+   mutate( # Calculate new variables
+     friend_gender = recode(friend_gender, 
+                     `1` = "Male", # Coding 0s as 'male'
+                     '2' = "Female",
+                     .default = "Other")) # Code everything else as 'Other'
> carol %>% # Take our data frame
+   filter(friend_gender != "Other") %>% # Drop 'Other' from gender
+   group_by(friend_gender) %>% # Run analyses separately for male and female Ps
+   summarise(
+     wth_mean = mean(will_to_help, na.rm = T), # Get mean of depression scores
+     wth_SD = sd(will_to_help, na.rm = T) # Get SD of depression scores
+   )
# A tibble: 2 × 3
  friend_gender wth_mean wth_SD
  <chr>            <dbl>  <dbl>
1 Female            8.89   1.74
2 Male              8      1.77
> aov4<-aov(will_to_help ~ gender + friend_gender + gender:friend_gender, data=carol)
> summary(aov4)
                     Df Sum Sq Mean Sq F value Pr(>F)
gender                2   9.97   4.986   1.568  0.219
friend_gender         2   2.90   1.448   0.455  0.637
gender:friend_gender  1   1.57   1.572   0.495  0.485
Residuals            47 149.45   3.180               
52 observations deleted due to missingness
> aov5<-aov(SIS_scale1 ~ gender + friend_gender + gender:friend_gender, data=carol)
Error in eval(predvars, data, env) : object 'SIS_scale1' not found
> summary(aov5)
Error: object 'aov5' not found
> carol<-carol %>%
+   mutate(SIS_Conflict = rowMeans(
+     across(c(SIS_3,SIS_7R,
+              SIS_11,SIS_15R,SIS_19,SIS_23R)
+     )))
> aovconflict<-aov(SIS_Conflict ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data = carol)
> summary(aovconflict)
                           Df Sum Sq Mean Sq F value Pr(>F)
Empathy_Group               1   0.07  0.0700   0.160  0.690
Friend_Group                1   0.27  0.2745   0.626  0.431
Empathy_Group:Friend_Group  1   0.31  0.3091   0.705  0.403
Residuals                  96  42.07  0.4382               
5 observations deleted due to missingness
> carol<-carol %>%
+   mutate(SIS_MutDependence = rowMeans(
+     across(c(SIS_2R,SIS_6,
+              SIS_10R,SIS_14,SIS_18R,SIS_22)
+     )))
> aovmutdepend<-aov(SIS_MutDependence ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data = carol)
> summary(aovmutdepend)
                           Df Sum Sq Mean Sq F value Pr(>F)
Empathy_Group               1   0.46  0.4586   1.132  0.290
Friend_Group                1   0.19  0.1915   0.473  0.493
Empathy_Group:Friend_Group  1   0.14  0.1379   0.340  0.561
Residuals                  97  39.30  0.4051               
4 observations deleted due to missingness
> #future interdepenence subscale 1R, 5, 9R, 13, 17R, 21
> carol<-carol %>%
+   mutate(SIS_futint = rowMeans(
+     across(c(SIS_1R,SIS_5,
+              SIS_9R,SIS_13,SIS_17R,SIS_21)
+     )))
> aovfutint<-aov(SIS_futint ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data = carol)
> summary(aovfutint)
                           Df Sum Sq Mean Sq F value Pr(>F)
Empathy_Group               1   0.08  0.0844   0.178  0.674
Friend_Group                1   0.04  0.0368   0.078  0.781
Empathy_Group:Friend_Group  1   0.24  0.2440   0.515  0.475
Residuals                  95  45.04  0.4741               
6 observations deleted due to missingness
> carol<-carol %>%
+   mutate(SIS_infocert = rowMeans(
+     across(c(SIS_4R,SIS_8,
+              SIS_12R,SIS_16,SIS_20R,SIS_24)
+     )))
> aovinfocert<-aov(SIS_infocert ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data = carol)
> summary(aovinfocert)
                           Df Sum Sq Mean Sq F value Pr(>F)   
Empathy_Group               1   0.70   0.702   1.499 0.2238   
Friend_Group                1   4.28   4.285   9.148 0.0032 **
Empathy_Group:Friend_Group  1   0.80   0.799   1.707 0.1946   
Residuals                  95  44.50   0.468                  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
6 observations deleted due to missingness
> t.test(SIS_infocert ~ Friend_Group, data = carol)

	Welch Two Sample t-test

data:  SIS_infocert by Friend_Group
t = 3.2193, df = 96.994, p-value = 0.001748
alternative hypothesis: true difference in means between group Friend and group Stranger is not equal to 0
95 percent confidence interval:
 0.1696593 0.7151446
sample estimates:
  mean in group Friend mean in group Stranger 
              3.754902               3.312500 

> #check if our dependent variables were correlated------------------         
> cor.test(x = carol$will_to_help, y = carol$SIS_scale)
Error in cor.test.default(x = carol$will_to_help, y = carol$SIS_scale) : 
  'y' must be a numeric vector
>         #try with first 24 questions
> cor.test(x = carol$will_to_help, y = carol$SIS_scale1)
Error in cor.test.default(x = carol$will_to_help, y = carol$SIS_scale1) : 
  'y' must be a numeric vector
>           data:  carol$will_to_help and carol$SIS_scale1
Error: unexpected symbol in "          data:  carol$will_to_help and"
>           t = 2.2176, df = 94, p-value = 0.02899
Error: unexpected ',' in "          t = 2.2176,"
>           alternative hypothesis: true correlation is not equal to 0
Error: unexpected symbol in "          alternative hypothesis"
>           95 percent confidence interval:
Error: unexpected symbol in "          95 percent"
>           sample estimates:
Error: unexpected symbol in "          sample estimates"
>           0.2229715 
[1] 0.2229715
 t.test(carol$will_to_help ~ carol$Friend_Group)         

	Welch Two Sample t-test

data:  carol$will_to_help by carol$Friend_Group
t = 2.464, df = 92.714, p-value = 0.01558
alternative hypothesis: true difference in means between group Friend and group Stranger is not equal to 0
95 percent confidence interval:
 0.1966137 1.8300234
sample estimates:
  mean in group Friend mean in group Stranger 
              8.660377               7.647059 

>           # 
>           # data:  carol$will_to_help by carol$Friend_Group
>           # t = 2.464, df = 92.714, p-value = 0.01558
>           # alternative hypothesis: true difference in means between group Friend and group Stranger is not equal to 0
>           # 95 percent confidence interval:
>           #   0.1966137 1.8300234
>           # sample estimates:
>           #   mean in group Friend mean in group Stranger 
>           # 8.660377               7.647059 
>           # 
> library(psych)
> library(tidyverse)
> ggplot(data = , aes(x = will_to_help, y = Empathy_check)) +
+       geom_point()
Error in `fortify()`:
! `data` must be a <data.frame>, or an object coercible by `fortify()`, not a <uneval> object.
ℹ Did you accidentally pass `aes()` to the `data` argument?
Run `rlang::last_trace()` to see where the error occurred.
> setwd(carol)
Error in setwd(carol) : character argument expected
> ggplot(data =  carol, aes(x = will_to_help, y = SIS_scale1)) + geom_point()
Error in `geom_point()`:
! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
! object 'SIS_scale1' not found
Run `rlang::last_trace()` to see where the error occurred.
> barplot(5,carol,var=carol$will_to_help,grp=Friend_Group,horiz=TRUE,label="Willingness to Help",zero=FALSE)
Error in width/2 : non-numeric argument to binary operator
In addition: Warning message:
In mean.default(width) : argument is not numeric or logical: returning NA
> barplot.default(5,width=1,space=NULL,)
> barplot(SIS_scale1 ~ Friend_Group, data = carol)
Error in eval(predvars, data, env) : object 'SIS_scale1' not found
> barplot(Friend_Group ~ will_to_help, data = carol)
Error in barplot.formula(Friend_Group ~ will_to_help, data = carol) : 
  duplicated categorical values - try another formula or subset
> save.image("~/Downloads/CarolResults.RData")
> carol_friend <- carol %>% filter(Friend_Group == "Friend")
> cor.test(carol_friend$empathy_check, carol_friend$will_to_help)

	Pearson's product-moment correlation

data:  carol_friend$empathy_check and carol_friend$will_to_help
t = 5.6469, df = 51, p-value = 7.291e-07
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.4204407 0.7626792
sample estimates:
     cor 
0.620248 

> carol_stranger <- carol %>% filter(Stranger_Group == "Stranger")
Error in `filter()`:
ℹ In argument: `Stranger_Group == "Stranger"`.
Caused by error:
! object 'Stranger_Group' not found
Run `rlang::last_trace()` to see where the error occurred.
> carol_stranger <- carol %>% filter(Friend_Group == "Stranger")
> cor.test(carol_stranger$empathy_check, carol_stranger$will_to_help)

	Pearson's product-moment correlation

data:  carol_stranger$empathy_check and carol_stranger$will_to_help
t = 3.0462, df = 49, p-value = 0.003726
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.1386918 0.6077743
sample estimates:
      cor 
0.3990243 

> mod <- lm(will_to_help ~ Friend_Group*empathy_check, data = carol)
> anova(mod)
Analysis of Variance Table

Response: will_to_help
                            Df Sum Sq Mean Sq F value    Pr(>F)    
Friend_Group                 1  26.69  26.687  7.9436  0.005817 ** 
empathy_check                1 104.60 104.597 31.1337 2.064e-07 ***
Friend_Group:empathy_check   1   2.98   2.977  0.8862  0.348786    
Residuals                  100 335.96   3.360                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> aov1 <- aov(SIS_scale ~ Empathy_Group*Friend_Group, data=carol)
Error in eval(predvars, data, env) : object 'SIS_scale' not found
> aov(SIS_scale ~ Empathy_Group*Friend_Group, data=carol)
Error in eval(predvars, data, env) : object 'SIS_scale' not found
> carol<-carol %>%
+   mutate(SIS_scale = rowMeans(
+     across(c(SIS_1R,SIS_2R,SIS_3,SIS_4R,SIS_5,SIS_6,SIS_7R,SIS_8,SIS_9R,SIS_10R,
+              SIS_11,SIS_12R,SIS_13,SIS_14,SIS_15R,SIS_16,SIS_17R,SIS_18R,SIS_19,SIS_20R,
+              SIS_21,SIS_22,SIS_23R,SIS_24,SIS_2_1,SIS_2_2R,SIS_2_3,SIS_2_4R,SIS_2_5,SIS_2_6R)
+     ))
+ )
> aov8<-aov(SIS_scale ~ Empathy_Group*Friend_Group, data=carol)
> summary(aov8)
                           Df Sum Sq Mean Sq F value Pr(>F)
Empathy_Group               1  0.046 0.04563   0.188  0.666
Friend_Group                1  0.064 0.06359   0.262  0.610
Empathy_Group:Friend_Group  1  0.058 0.05839   0.241  0.625
Residuals                  75 18.176 0.24235               
26 observations deleted due to missingness
> aov8<-aov(SIS_scale ~ Empathy_Group*Friend_Group, data=carol)
> summary(aov8)
                           Df Sum Sq Mean Sq F value Pr(>F)
Empathy_Group               1  0.046 0.04563   0.188  0.666
Friend_Group                1  0.064 0.06359   0.262  0.610
Empathy_Group:Friend_Group  1  0.058 0.05839   0.241  0.625
Residuals                  75 18.176 0.24235               
26 observations deleted due to missingness
> View(aovfutint)
> View(aovconflict)
> View(carol)
> View(carol_friend)
> View(carol_clean_1)
> View(carol)
> cor.test(x = carol$empathy_check,
+          y = carol$SIS_scale)

# 	Pearson's product-moment correlation
# 
# data:  carol$empathy_check and carol$SIS_scale
# t = 3.1155, df = 77, p-value = 0.00258
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1225371 0.5174146
# sample estimates:
#       cor 
# 0.3345823 
# 
# > ggplot(data = carol, aes(x = empathy_check, y = will_to_help)) +
# +   geom_point()
# Warning message:
# Removed 1 rows containing missing values (`geom_point()`). 
# > hist(carol$will_to_help)
# > table(carol$will_to_help)
# 
#  0  2  3  5  6  7  8  9 10 
#  2  1  2  4 10 12 19 15 39 
# > table(carol$will_to_help, carol$empathy_check)
#     
#       2  3  4
#   0   1  0  1
#   2   0  1  0
#   3   0  1  1
#   5   1  2  1
#   6   0  9  1
#   7   4  5  3
#   8   1  8 10
#   9   0  2 13
#   10  0  4 35
# > table(carol$empathy_check)
# 
#  2  3  4 
#  7 32 66 
 

carol <- carol%>% # Save results back into data frame
  mutate( # Calculate new variables
    EmpathyHighLow= recode(empathy_check, 
                           `4` = "High", 
                           '2' = "Low",
                           '3' = "Low",
                           .default = "Other")) # Code everything else as 'Other'
library(psych)
library(tidyverse)

library(emmeans)

emmeans(carol, ~ EmpathyHighLow*Friend_Group)
summary(carol$EmpathyHighLow)

t.test(will_to_help ~ EmpathyHighLow, data = carol)

head(carol$EmpathyHighLow)
# set up the x-axis with the first factor (two bars for each of the two conditions) 
condition.x <- c(rep("High", 2), rep("Low", 2)) 

# set up the second factor (two conditions are differentiated by two colors)
condition.color <- rep(c("Friend", "Stranger"), 2)

# create a new variable with the means of four conditions (make sure you make it with the correct order)
means.4Group <- c(2.61, 2.65, 3.23, 3.14) #the order is disgust + negative, disgust + positive, fear + negative, fear + positive

# create a new variable with the standard error (SE) of four conditions (keep the correct order)
se.4Group <- c(0.177, 0.177, 0.172, 0.177) #same order as the means

# create a new dataframe and add all the new variables in it, so that we will not mess up our original dataset
data.plot <- data.frame(condition.x, condition.color, means.4Group, se.4Group)

data.plot #check whether the conditions and data are matching



t.test(will_to_help ~ Friend_Group, data = carol)

aov9 <- aov(will_to_help ~ EmpathyHighLow + Friend_Group + EmpathyHighLow:Friend_Group, data=carol)
summary(aov9)

# Df Sum Sq Mean Sq F value   Pr(>F)    
# EmpathyHighLow                1  108.3  108.28  31.716 1.64e-07 ***
#   Friend_Group                  1   19.4   19.39   5.678   0.0191 *  
#   EmpathyHighLow:Friend_Group   1    1.1    1.14   0.333   0.5651    
# Residuals                   100  341.4    3.41                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 1 observation deleted due to missingness\

emmeans(aov9, ~ EmpathyHighLow*Friend_Group)

EmpathyHighLow Friend_Group emmean    SE  df lower.CL upper.CL
High           Friend         9.43 0.312 100     8.81    10.05
Low            Friend         7.17 0.436 100     6.30     8.03
High           Stranger       8.40 0.337 100     7.73     9.07
Low            Stranger       6.57 0.403 100     5.77     7.37

Confidence level used: 0.95 


#time to graph
emmeans(aov9, ~ EmpathyHighLow*Friend_Group)
condition.x <- c(rep("High Empathy", 2), rep("Low Empathy", 2))
condition.color <- rep(c("Friend", "Stranger"), 2)
means.4Group <- c(9.43, 7.17,8.40, 6.57)
se.4Group <- c(0.312, 0.436, 0.337, 0.403)
data.plot <- data.frame(condition.x, condition.color, means.4Group, se.4Group)
data.plot

plot1 <- ggplot(data.plot, aes(fill=condition.color, y=means.4Group, x=condition.x)) + geom_bar(position="dodge", color="black", stat="identity") + geom_errorbar(aes(ymin=means.4Group - se.4Group, ymax=means.4Group + se.4Group), width=.1, position=position_dodge(.9)) + labs(fill="color", title="Reported Empathy x Relationship Type on Willingness to Help", x = "Reported Empathy", y= "Willingness to Help") 
plot1

library(psych)
library(tidyverse)
library(lsr)
install.packages("lsr")

#willingness to help eta squares-----------
etaSquared(aov2)
etaSquared( aov2 ) 
summary(aov2)
etaSquared(aov2)
describe(carol$Friend_Group)
# eta.sq  eta.sq.part
# Empathy_Group              0.0058565274 0.0059920508
# Friend_Group               0.0165513023 0.0167510149
# Empathy_Group:Friend_Group 0.0007933264 0.0008159111

summary(aov3)
summary(aov4)
summary(aov5)
summary(aov9)
etaSquared(aov9)

# eta.sq eta.sq.part
# EmpathyHighLow              0.214754408 0.228261524
# Friend_Group                0.041228847 0.053732329
# EmpathyHighLow:Friend_Group 0.002419074 0.003320666

summary(aov7)
etaSquared(aov7)

t.test(SIS_scale ~ carol$EmpathyHighLow, data = carol)

describe(carol$EmpathyHighLow)
summary(carol$EmpathyHighLow)
describe(carol$empathy_check)
table(carol$empathy_check)
describe(carol$age)
table(carol$gender)
table(carol$age)
aov(carol$will_to_help ~ carol$Empathy_Group*carol$Friend_Group)


aovSIS <- aov(SIS_scale ~ Empathy_Group + Friend_Group + Empathy_Group:Friend_Group, data=carol)
summary(aovSIS)

emmeans(carol$will_to_help ~ carol$Friend_Group)

summary(carol$Friend_Group)
means.4Group(carol$will_to_help~carol$Friend_Group)
carol_clean_1 %>%
  group_by(Empathy1_no0, friend1stranger0) %>% summarise(
    mean_will_to_help = 
      mean(will_to_help, na.rm = T)
  )
carol <- carol_clean_1
t.test(will_to_help ~ friend1stranger0, data = carol_clean_1)
t.test(will_to_help ~ EmpathyHi, data = carol)   
table(carol$EmpathyHighLow)
t.test(carol$SIS_scale ~ carol$EmpathyHighLow)


aov11 <- aov(SIS_scale ~ EmpathyHighLow + Friend_Group + EmpathyHighLow:Friend_Group, data=carol)
summary(aov11)

barplot(SIS_scale ~ EmpathyHighLow, data = carol)
emmeans(aov11, ~ EmpathyHighLow*Friend_Group)

# EmpathyHighLow Friend_Group emmean     SE df lower.CL upper.CL
# High           Friend         3.43 0.0588 94     3.32     3.55
# Low            Friend         3.12 0.0819 94     2.96     3.29
# High           Stranger       3.28 0.0638 94     3.16     3.41
# Low            Stranger       3.17 0.0755 94     3.02     3.31
# 
# Confidence level used: 0.95 

condition.x <- c(rep("High Empathy", 2), rep("Low Empathy", 2))
condition.color <- rep(c("Friend", "Stranger"), 2)
means.4Group <- c(3.43, 3.12,3.28, 3.17)
se.4Group <- c(0.0588, 0.0819, 0.0638, 0.0755)
data.plot <- data.frame(condition.x, condition.color, means.4Group, se.4Group)
data.plot

plot3 <- ggplot(data.plot, aes(fill=condition.color, y=means.4Group, x=condition.x)) + geom_bar(position="dodge", color="black", stat="identity") + geom_errorbar(aes(ymin=means.4Group - se.4Group, ymax=means.4Group + se.4Group), width=.1, position=position_dodge(.9)) + labs(fill="color", title="Reported Empathy x Relationship Type on Situational Interdependence", x = "Reported Empathy", y= "Willingness to Help") 
plot3

ggplot(data = carol, aes(x = SIS_scale, y = will_to_help)) + 
  geom_point() + labs(fill="color", title="Situational Interdependence x Willingness to Hehlp ", x = "Situational Interdependence", y= "Willingness to Help") 

etaSquared(aov2)
etaSquared(aov7)
summary(aov7)


emmeans(carol$will_to_help ~ carol$Empathy_Group)
