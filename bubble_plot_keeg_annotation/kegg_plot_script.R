## library

library(ggplot2)
library(patchwork)


## data importation


j17_kegg = read.csv("j17_kegg_default.csv", sep = ",", h = T)


## plot


j17_k = ggplot(j17_kegg, aes(x = reorder(term, gene_number), y = enrich_ratio, color = `p_value` )) + 
  geom_point(data=j17_kegg,aes(x = reorder(term, gene_number), y = enrich_ratio, size = `gene_number`), alpha=.7)+
  scale_size(range = c(.1, 10), name="Gene number") +
  coord_flip()+
  theme_bw()+
  theme(axis.ticks.length=unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(5,5,0,5,"pt")),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt")),
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        legend.title.align=0.5)+
  xlab("Term")+
  ylab("Enrich ratio")+
  labs(color="p-value", size="Gene number", title="KEGG")+ #Replace by your variable names; \n allow a new line for text
  scale_color_gradient(low="#884EA0",high="#7FB3D5",limits=c(0, NA)) +
  guides(colour = guide_colourbar(order = 1),
         size = guide_legend(order=2))

j17_k




#### for pc2

########## KEGG
pc2_kegg = read.csv("pc2_kegg.csv", sep = ",", h = T)

pc2_k = ggplot(pc2_kegg, aes(x = reorder(term, gene_number), y = enrich_ratio, color = `p_value` )) + 
  geom_point(data=pc2_kegg,aes(x = reorder(term, gene_number), y = enrich_ratio, size = `gene_number`), alpha=.7)+
  scale_size(range = c(.1, 10), name="Gene number") +
  coord_flip()+
  theme_bw()+
  theme(axis.ticks.length=unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(5,5,0,5,"pt")),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt")),
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        legend.title.align=0.5)+
  xlab("Term")+
  ylab("Enrich ratio")+
  labs(color="p-value", size="Gene number", title = "(A) Perilla citriodora PC002")+ #Replace by your variable names; \n allow a new line for text
  scale_color_gradient(low="#884EA0",high="#7FB3D5",limits=c(0, NA)) +
  guides(colour = guide_colourbar(order = 1),
         size = guide_legend(order=2))

pc2_k


###### pc9


pc9_kegg = read.csv("pc9_kegg.csv", sep = ",", h = T)

pc9_k = ggplot(pc9_kegg, aes(x = reorder(term, gene_number), y = enrich_ratio, color = `p_value` )) + 
  geom_point(data=pc9_kegg,aes(x = reorder(term, gene_number), y = enrich_ratio, size = `gene_number`), alpha=.7)+
  scale_size(range = c(.1, 10), name="Gene number") +
  coord_flip()+
  theme_bw()+
  theme(axis.ticks.length=unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(5,5,0,5,"pt")),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt")),
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        legend.title.align=0.5)+
  xlab("Term")+
  ylab("Enrich ratio")+
  labs(color="p-value", size="Gene number", title = "(B) Perilla citriodora PC09")+ #Replace by your variable names; \n allow a new line for text
  scale_color_gradient(low="#884EA0",high="#7FB3D5",limits=c(0, NA)) +
  guides(colour = guide_colourbar(order = 1),
         size = guide_legend(order=2))
  

pc9_k



### PF40


pf40_kegg = read.csv("pf40_kegg.csv", sep = ",", h = T)

pf40_k = ggplot(pf40_kegg, aes(x = reorder(term, gene_number), y = enrich_ratio, color = `p_value` )) + 
  geom_point(data=pf40_kegg,aes(x = reorder(term, gene_number), y = enrich_ratio, size = `gene_number`), alpha=.7)+
  scale_size(range = c(.1, 10), name="Gene number") +
  coord_flip()+
  theme_bw()+
  theme(axis.ticks.length=unit(-0.1, "cm"),
        axis.text.x = element_text(margin=margin(5,5,0,5,"pt")),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt")),
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        legend.title.align=0.5)+
  xlab("Term")+
  ylab("Enrich ratio")+
  labs(color="p-value", size="Gene number", title="(C) Perilla frutescense PF40")+ #Replace by your variable names; \n allow a new line for text
  scale_color_gradient(low="#884EA0",high="#7FB3D5",limits=c(0, NA)) +
  guides(colour = guide_colourbar(order = 1),
         size = guide_legend(order=2))

pf40_k



## KEGG together

pc2_k + pc9_k + pf40_k
