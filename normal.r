library(readxl)
dir<-file.choose()

xlsx<-read_excel(dir,sheet=1,col_names=TRUE)
data<-as.data.frame(xlsx)
head(data)


tiff(file="/Users/statistics/Desktop/Proyectos/Odontología  Forense/Paper dimorfismo/Supplementary_3.tiff",width=12, height=10, units="in", res=100)
par(mfrow=c(2,2))

nombre<-names(data[4])
s<-shapiro.test(data[,4])
w<-round(s$statistic,digits=3)
p<-round(s$p.value,digits=3)
sub<-paste0("Shapiro-Wilk normality test (W = ",w," p-value = ",p,")")
qqnorm(data[,4],xlab="Theoretical Quantiles",ylab="Sample Quantiles",main=paste("Normal Q-Q Plot:",nombre),sub=sub,font.main=2, font.lab=2, font.sub=4,cex.main=1.4,cex.lab=1.1)
qqline(data[,4])

nombre<-names(data[5])
s<-shapiro.test(data[,5])
w<-round(s$statistic,digits=3)
p<-round(s$p.value,digits=3)
sub<-paste0("Shapiro-Wilk normality test (W = ",w," p-value = ",p,")")
qqnorm(data[,5],xlab="Theoretical Quantiles",ylab="Sample Quantiles",main=paste("Normal Q-Q Plot:",nombre),sub=sub,font.main=2, font.lab=2, font.sub=4,cex.main=1.4,cex.lab=1.1)
qqline(data[,5])

nombre<-names(data[6])
s<-shapiro.test(data[,6])
w<-round(s$statistic,digits=3)
p<-round(s$p.value,digits=3)
sub<-paste0("Shapiro-Wilk normality test (W = ",w," p-value = ",p,")")
qqnorm(data[,6],xlab="Theoretical Quantiles",ylab="Sample Quantiles",main=paste("Normal Q-Q Plot:",nombre),sub=sub,font.main=2, font.lab=2, font.sub=4,cex.main=1.4,cex.lab=1.1)
qqline(data[,6])

nombre<-names(data[7])
s<-shapiro.test(data[,7])
w<-round(s$statistic,digits=3)
p<-round(s$p.value,digits=3)
sub<-paste0("Shapiro-Wilk normality test (W = ",w," p-value = ",p,")")
qqnorm(data[,7],xlab="Theoretical Quantiles",ylab="Sample Quantiles",main=paste("Normal Q-Q Plot:",nombre),sub=sub,font.main=2, font.lab=2, font.sub=4,cex.main=1.4,cex.lab=1.1)
qqline(data[,7])

dev.off()
