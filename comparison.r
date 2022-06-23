library(ggplot2)
library(viridis)
library(ggpubr) 

#par(mfrow=c(2,2))
T13<-t.test(data[,4]~data[,3])
nombre<-names(data[4])
p <- ggplot(data, aes(x=Sex, y=data[,4],fill=Sex)) + 
  geom_violin(trim=FALSE,alpha=0.7)
#p+ scale_fill_viridis_b(discrete = TRUE,option = "H")+ theme_classic()+ylab("Mesiodistal measure (mm)")+theme(legend.position="top")+xlab("")+stat_summary(fun.y=mean,color="black")+labs(title=paste("Canine:",nombre))

p1<-p+ scale_fill_viridis(discrete = TRUE,begin = 0,end = .5)+ theme_classic()+theme(axis.text = element_text(size = 12),text = element_text(size = 15))+ylab("Mesiodistal measure (mm)")+theme(legend.position="top")+xlab("")+stat_summary(fun.y=mean,color="black")+labs(title=paste("Canine:",nombre))+ylim(5, 10.5)

my_comparisons <- list(c("Female", "Male"))

PP1<-p1+stat_compare_means(comparisons = my_comparisons,method = "t.test",label.y = 10.3,size = 5,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("p-value < 0.0001", "p-value < 0.001", "p-value < 0.01", "p-value < 0.05","1"))) 



nombre<-names(data[5])
p <- ggplot(data, aes(x=Sex, y=data[,5],fill=Sex)) + 
  geom_violin(trim=FALSE,alpha=0.7)
#p+ scale_fill_viridis_b(discrete = TRUE,option = "H")+ theme_classic()+ylab("Mesiodistal measure (mm)")+theme(legend.position="top")+xlab("")+stat_summary(fun.y=mean,color="black")+labs(title=paste("Canine:",nombre))

p1<-p+ scale_fill_viridis(discrete = TRUE,begin = 0,end = .5)+ theme_classic()+theme(axis.text = element_text(size = 12),text = element_text(size = 15))+ylab("Mesiodistal measure (mm)")+theme(legend.position="top")+xlab("")+stat_summary(fun.y=mean,color="black")+labs(title=paste("Canine:",nombre))+ylim(5, 10.5)

my_comparisons <- list(c("Female", "Male"))

PP2<-p1+stat_compare_means(comparisons = my_comparisons,method = "t.test",label.y = 10.3,size = 5,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("p-value < 0.0001", "p-value < 0.001", "p-value < 0.01", "p-value < 0.05","1"))) 



nombre<-names(data[6])
p <- ggplot(data, aes(x=Sex, y=data[,6],fill=Sex)) + 
  geom_violin(trim=FALSE,alpha=0.7)
#p+ scale_fill_viridis_b(discrete = TRUE,option = "H")+ theme_classic()+ylab("Mesiodistal measure (mm)")+theme(legend.position="top")+xlab("")+stat_summary(fun.y=mean,color="black")+labs(title=paste("Canine:",nombre))

p1<-p+ scale_fill_viridis(discrete = TRUE,begin = 0,end = .5)+ theme_classic()+theme(axis.text = element_text(size = 12),text = element_text(size = 15))+ylab("Mesiodistal measure (mm)")+theme(legend.position="top")+xlab("")+stat_summary(fun.y=mean,color="black")+labs(title=paste("Canine:",nombre))+ylim(5, 10.5)

my_comparisons <- list(c("Female", "Male"))

PP3<-p1+stat_compare_means(comparisons = my_comparisons,method = "t.test",label.y = 10.3,size = 5,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("p-value < 0.0001", "p-value < 0.001", "p-value < 0.01", "p-value < 0.05","1"))) 



nombre<-names(data[7])
p <- ggplot(data, aes(x=Sex, y=data[,7],fill=Sex)) + 
  geom_violin(trim=FALSE,alpha=0.7)
#p+ scale_fill_viridis_b(discrete = TRUE,option = "H")+ theme_classic()+ylab("Mesiodistal measure (mm)")+theme(legend.position="top")+xlab("")+stat_summary(fun.y=mean,color="black")+labs(title=paste("Canine:",nombre))

p1<-p+ scale_fill_viridis(discrete = TRUE,begin = 0,end = .5)+ theme_classic()+theme(axis.text = element_text(size = 12),text = element_text(size = 15))+ylab("Mesiodistal measure (mm)")+theme(legend.position="top")+xlab("")+stat_summary(fun.y=mean,color="black")+labs(title=paste("Canine:",nombre))+ylim(5, 10.5)

my_comparisons <- list(c("Female", "Male"))

PP4<-p1+stat_compare_means(comparisons = my_comparisons,method = "t.test",label.y = 10.3,size = 5,symnum.args=list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("p-value < 0.0001", "p-value < 0.001", "p-value < 0.01", "p-value < 0.05","1"))) 


require(gridExtra)
plot<-grid.arrange(PP1,PP2,PP3,PP4, ncol=2)
setwd("/Users/statistics/Desktop")

g <- arrangeGrob(PP1, PP2,PP3,PP4, ncol=2) 
ggsave(file="Figure_4.tiff", g,dpi = 320,width = 35, height = 30, units = "cm")
