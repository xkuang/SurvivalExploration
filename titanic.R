library(readr)
library(corrplot)
library(ggplot2)
library(rpart)

titanic_data <- read_csv("~/SurvivalExploration/titanic_data.csv")
View(titanic_data)

D1 <- titanic_data
D1$gender <-ifelse(D1$Sex=="male",0,1)

D2 <- dplyr::select(D1, 2:3, 6:8, 10, 13)

D2 <- tidyr::gather(D2)

names(D2) <- c("variable","value")
D2 <- na.omit(D2)
g <- ggplot(D2,aes(x=value))

g + geom_histogram(data = D2[D2$variable == "Age",], binwidth=1) +
  geom_histogram(data = D2[D2$variable == "Fare",], binwidth=0.1) +  
  geom_histogram(data = D2[D2$variable == "gender",], binwidth=1) +
  geom_histogram(data = D2[D2$variable == "Parch",], binwidth=1) + 
  geom_histogram(data = D2[D2$variable == "Pclass",], binwidth=1) +
  geom_histogram(data = D2[D2$variable == "SibSp",], binwidth=1) + 
  geom_histogram(data = D2[D2$variable == "Survived",], binwidth=1) + 
  facet_wrap(~variable, scales = "free")

D3 <- dplyr::select(D1, 2:3, 6:8, 10, 13)
D3 <- na.omit(D3)
COR <- cor(D3)

corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.01, insig = "blank")

male <- dplyr::filter(D1, Sex != "female")
male <- dplyr::filter(male, Age > 10)
male <- dplyr::select(male, 2:3, 6:8, 10, 13)
#male <- na.omit(male)
c.tree <- rpart(Survived ~ Pclass + Age , method="class", data=male)
post(c.tree, file = "tree.ps", title = "tree")
printcp(c.tree)


c.treeA <- prune(c.tree, cp = 0.018)
post(c.treeA, file = "treeA.ps", title = "treeA")
printcp(c.treeA)

female <- dplyr::filter(D1, Sex != "male")
female <- dplyr::filter(female, Pclass == 3)
