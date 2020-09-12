library(readr)
pkdf <- read.csv("Pokemon_for_session.csv")
head(pkdf)

pkdf
summary(pkdf)
### try with all the dependent variables
pk_logit_model <- glm(Legendary~ Attack+Defense+Sp_Atk+Sp_Def +HP + Speed, data=pkdf, family="binomial")
summary(pk_logit_model)


#Try deleting 
pk_logit_model <- glm(Legendary~ Attack+Sp_Atk+Sp_Def +HP + Speed, data=pkdf, family="binomial")
summary(pk_logit_model)

#making it more significance using attack
exp(0.032132)-1

library(rpart.plot)
#putting the same variables from tree
pk_tree_model<- rpart(Legendary~ Attack+Sp_Atk+Sp_Def +HP, data= pkdf, method= "class")
rpart.plot(pk_tree_model, type= 1 , extra= 1, box.palette=c("pink", "green"), branch.lty=3, shadow.col="gray")
summary(pk_tree_model)

#plotting cp
plotcp(pk_tree_model)

library(plotly)
library(ggplot2)
z <- ggplot(pkdf, aes(x=HP, y=Sp_Atk))+
  geom_jitter(aes(color=Type_1))

ggplotly(z) #converting a ggplot to plotly


#Compare ! logistic regression vs tree
library(ROCR)

pk_predict_logit<- prediction(pk_predict_logit, pkdf$Legendary)
pk_tree_prediction<- prediction(pk_tree_prediction[,2],pkdf$Legendary)

#lets get 
pk_performance_logit <-performance(pk_logit_prediction, "tpr", "fpr")
pk_performance_tree <-performance(pk_tree_prediction, "tpr", "fpr")

#
plot(pk_performance_logit, col = "blue", lty= 3, lwd=3)
plot(pk_performance_tree, col = "black", lty = 3, add = TRUE)