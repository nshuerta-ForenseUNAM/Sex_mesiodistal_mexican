xlsx<-read_excel(dir,sheet=1,col_names=TRUE)
data<-as.data.frame(xlsx)
head(data)

data$Sex<-as.factor(data$Sex)
mylogit <- glm(Sex~MD_13, data = data, family = "binomial")
summary(mylogit)
AIC(mylogit)
pre<-predict(mylogit,type='response')
fitted<-ifelse(pre > 0.5,"Female","Male")
fitted<-as.factor(fitted)
length(fitted)

Sas<-as.factor(data1$Sex)

require(caret)    
cm<-confusionMatrix(fitted,Sas)
cm

mylogit <- glm(Sex~MD_23, data = data, family = "binomial")
summary(mylogit)
AIC(mylogit)
predict(mylogit)

pre<-predict(mylogit,type='response')
fitted<-ifelse(pre > 0.5,"Female","Male")
fitted<-as.factor(fitted)
length(fitted)

Sas<-as.factor(data1$Sex)

require(caret)    
cm<-confusionMatrix(fitted,Sas)
cm

mylogit <- glm(Sex~MD_43, data = data, family = "binomial")
summary(mylogit)
AIC(mylogit)
predict(mylogit)

pre<-predict(mylogit,type='response')
fitted<-ifelse(pre > 0.5,"Female","Male")
fitted<-as.factor(fitted)
length(fitted)

Sas<-as.factor(data1$Sex)

require(caret)    
cm<-confusionMatrix(fitted,Sas)
cm

mylogit <- glm(Sex~MD_33, data = data, family = "binomial")
summary(mylogit)
AIC(mylogit)
predict(mylogit)

pre<-predict(mylogit,type='response')
fitted<-ifelse(pre > 0.5,"Female","Male")
fitted<-as.factor(fitted)
length(fitted)

Sas<-as.factor(data1$Sex)

require(caret)    
cm<-confusionMatrix(fitted,Sas)
cm


ggplot(data1, aes(x=MD_13, fill=Sex)) + geom_density(alpha=.3)
ggplot(data1, aes(x=MD_23, fill=Sex)) + geom_density(alpha=.3)
ggplot(data1, aes(x=MD_43, fill=Sex)) + geom_density(alpha=.3)
ggplot(data1, aes(x=MD_33, fill=Sex)) + geom_density(alpha=.3)
