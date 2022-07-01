library(readr)
britishSeatBeltStudy <- read_csv("britishSeatBeltStudy.csv", 
                                 col_types = cols(law = col_factor(levels = c("0", 
                                                                              "1")), Date = col_date(format = "%Y-%m-%d")))
#View(britishSeatBeltStudy)

britishSeatBeltStudy$FractionDriversKilled = britishSeatBeltStudy$DriversKilled /britishSeatBeltStudy$drivers

summary(britishSeatBeltStudy$FractionDriversKilled)

#boxplot(FractionDriversKilled ~ law, data = britishSeatBeltStudy)
#boxplot(DriversKilled ~ law, data = britishSeatBeltStudy)

t.test(britishSeatBeltStudy$DriversKilled [which(britishSeatBeltStudy$law %in% c(1))], 
       britishSeatBeltStudy$DriversKilled [which(britishSeatBeltStudy$law %in% c(0))], 
       var.equal = F, paired = F
)

t.test(britishSeatBeltStudy$FractionDriversKilled [which(britishSeatBeltStudy$law %in% c(1))], 
       britishSeatBeltStudy$FractionDriversKilled [which(britishSeatBeltStudy$law %in% c(0))], 
       var.equal = F, paired = F
)

# Linear Analysis: Logistic Regression [(Classification)]

fit <- glm (formula = law ~ DriversKilled + FractionDriversKilled + kms,
            data = britishSeatBeltStudy, family = 'binomial')
summary(fit)


# Linear Analysis: linear Regression to predict the number of drivers that died based on kms driven, petrol price and 
#law being in effect
#Continuous Response 

fit2 <- lm(formula = DriversKilled ~ law + kms + PetrolPrice , data = britishSeatBeltStudy, family = 'binomial')
summary(fit2)
