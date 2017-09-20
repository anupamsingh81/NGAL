# mutation n recode

x = c(1:5)
y= c("a","b","c","d","e")

dat = data.frame(x,y)

summary(dat)

summary(dat$y)
summary(as.factor(dat$y))

dat$y = recode(dat$y, a = "Apple", .default = levels(dat$y))


dat %>% mutate(y= recode(y, b = "Bat", .default = levels(y)))
