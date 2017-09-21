# mutation n recode

x = c(1:5)
y= c("a","b","c","d","e")

dat = data.frame(x,y)

summary(dat)

summary(dat$y)
summary(as.factor(dat$y))

# R

library(dplyr)
dat$y = recode(dat$y, a = "Apple", .default = levels(dat$y))

#dplyr

# Replace characters/Factors with recode

dat %>% mutate(y= recode(y, b = "Bat", .default = levels(y)))

dat %>% mutate(y= recode(y, b = "Bat", .default = levels(y)))

dat %>% mutate(y= recode(y, c = "Cat", .default = levels(y)))


# you can use dplyr to replace integers , but better to use base
dat %>% mutate(x= recode(x, `3` =6L))

dat %>% mutate(x= recode(x, 4 =6L)) # always put ``  in left hand side or it wouldnt work and L for integer before 3 or 4 

# Base R
dat$x[dat$x==5] = 5.2 


# Mosaic derived factor, multiple nested if else

library(mosaic)

dat %>% mutate(z = derivedFactor( g=x==2, h= x>3,.ordered=TRUE))


dat %>% mutate(z = ifelse(x<=2,"Low",
                          ifelse(x>=4,"High","Medium")))



dat %>% mutate(z= derivedFactor(Low = x<=2 ,High = x>=4 ,Medium = !(x<=2|x>=4)))


# dplyr case_when 
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.3/topics/case_when

dat %>% mutate(z1 = case_when(
  x<=2~"Low",
  x>=4~"High",
  TRUE~"Medium"
))

# For changing factors


library(forcats)

mutate(dat, z2 = fct_recode(y, "b" = "f"))
