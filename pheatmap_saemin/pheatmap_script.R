# Package

library(pheatmap)


# Data

data_set = read.csv("pheatmap-Zscore01.csv", h = T, sep = ",", row.names = 1)


# Make a matrix

data_matrix = as.matrix(data_set)


# Scale the data


data_matrix_scaled = scale(data_matrix)


# Render the heatmap

pheatmap(data_matrix)




pheatmap(data_matrix, 
         cluster_rows =  FALSE, 
         cluster_cols = FALSE, 
         show_rownames = FALSE,
         scale = "row",
         angle_col = "0")









