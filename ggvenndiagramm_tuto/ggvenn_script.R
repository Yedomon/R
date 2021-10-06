# package loading

library(ggVennDiagram)
library(ggplot2)

# load data

dataframe = read.csv("test_file.csv", sep = ",", h =T )

View(dataframe)

# Make list

list_file = list(Root=dataframe$root, Stem=dataframe$stem, Leaf=dataframe$leaf, Shoot=dataframe$shoot)


# Check if it is a list object

str(list_file)


# Plot a basic venn plot

ggVennDiagram(list_file)

ggVennDiagram(list_file, edge_size= 0.1, edge_lty = 4) + 
  scale_fill_gradient(low="red",high = "pink") 


# option 2

attach(dataframe)

list_new = list(Root = root, Stem = stem, Leaf = leaf, Shoot = shoot )

ggVennDiagram(list_new)

ggVennDiagram(list_new, edge_size= 0.1, edge_lty = 4) + 
  scale_fill_gradient(low="red",high = "pink") 


sessionInfo()              
