library(readxl)
dir<-file.choose()

xlsx<-read_excel(dir,sheet=3,col_names=TRUE)
data<-as.data.frame(xlsx)
head(data)

datc<-data[,c(2,4:7)]

library(corrplot)
library(RColorBrewer)
cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
    }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

tiff(file="/Users/statistics/Desktop/Proyectos/Odontología  Forense/Paper dimorfismo/Figure_3.tiff",width=12, height=10, units="in", res=100)
p.mat <- cor.mtest(datc)
corrplot(M, method="color", type="upper",addCoef.col = "black",number.cex=0.8,tl.col="black",tl.cex=0.8,col= colorRampPalette(c("gray","white", "darkslategray3"))(10),p.mat = p.mat, sig.level = 1, insig = "blank",diag=FALSE)
dev.off()
