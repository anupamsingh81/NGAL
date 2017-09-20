# misceelaneous

# correlation of Fena, NGAL

ungl %>% filter(Creat<=1)

ungl %>% filter(diagnoses=="CKD")


library(ggplot2)

ggplot(ungl2,aes(y=urine.NGAL,x=diagnoses))+geom_boxplot()

ggplot(ungl2,aes(y=feNa,x=diagnoses))+geom_boxplot()

ggplot(ungl2,aes(y=CTP,x=diagnoses))+geom_boxplot()

ggplot(ungl2,aes(y=MELD,x=diagnoses))+geom_boxplot()


ggplot(ungl2,aes(y=Creat,x=diagnoses))+geom_boxplot()

ggplot(ungl2,aes(y=Urine.na,x=diagnoses))+geom_boxplot()


ggplot(ungl2,aes(feNa,urine.NGAL))+geom_point()+geom_smooth()


ggplot(ungl2,aes(y=urine.NGAL,x=SBP))+geom_boxplot()

ggplot(ungl2,aes(y=urine.NGAL,x=hepatic.encephalopathy))+geom_boxplot()



ggplot(ungl30,aes(y=NGAL,x=outcome30))+geom_boxplot()+xlab("Outcome at 30 Days")+ylab("NGAL")


ggplot(ungl,aes(y=NGAL,x=outcome90))+geom_boxplot()+xlab("Outcome at 90 Days")+ylab("NGAL")











