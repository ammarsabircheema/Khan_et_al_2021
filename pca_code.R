# loading libraries
library(readxl)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)

# reading the input file
df_plants_info <- as.data.frame(read_excel("processed_nico.xlsx"))


# scaling the data
scaled_df <- apply(df_plants_info[,-1], 2, scale)

# performing PCA
arrests.cov <- cov(scaled_df)
arrests.eigen <- eigen(arrests.cov)

(phi <- arrests.eigen$vectors[,1:2])

phi <- -phi
row.names(phi) <- c(colnames(df_plants_info[,-1]))
colnames(phi) <- c("PC1", "PC2")

PC1 <- as.matrix(scaled_df) %*% phi[,1]
PC2 <- as.matrix(scaled_df) %*% phi[,2]

# Create data frame with Principal Components scores
PC <- data.frame(State = colnames(df_plants_info[,-1]), PC1, PC2)


# plotting the results of PCA
p <- ggplot(PC, aes(PC1, PC2,label = State)) + 
  modelr::geom_ref_line(h = 0) +
  modelr::geom_ref_line(v = 0) +
  geom_point(aes(color= factor(PC1)),show.legend=F) +
  xlab("First Principal Component") +
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components of Nicotiana Data")

p2 <- p + geom_text_repel(segment.color = NA) 
