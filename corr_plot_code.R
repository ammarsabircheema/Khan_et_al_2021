# loading libraries
library(readxl)
library(corrplot)
library(RColorBrewer)

# Reading the input files
df_plants_info <- as.data.frame(read_excel("processed_petunia2.xlsx"))

# function for calculating p_values
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


p.mat <- cor.mtest(df_plants_info[,-1])

# Performing correlation
cor <- cor(df_plants_info[,-1],method = "pearson", use = "complete.obs")

# Making plot
corrplot(cor, type="upper",p.mat = p.mat, sig.level = 0.005, insig = "blank",col = brewer.pal(n = 8, name = "RdYlBu"),tl.srt = 45)
