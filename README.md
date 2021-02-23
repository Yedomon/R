# R shiny

- #### [Shiny Apps: Development and Deployment](https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/shiny-apps/)
- #### [Shiny Apps: Development and Deployment Video](https://www.youtube.com/watch?v=QT3WUQu99pM)
- #### [R Recipe: Shiny Tables from Excel Templates](https://ljupcho.com/blog/shiny-excel-table-templates)



# R


- #### ggstatsplot | [raison d'etre](https://indrajeetpatil.github.io/ggstatsplot/index.html) |[PPT](https://indrajeetpatil.github.io/ggstatsplot_slides/slides/ggstatsplot_presentation.html#57)



summary of tests
The central tendency measure displayed will depend on the statistics:




| Type           | Measure      | Function used                                                                                                      |
| -------------- | ------------ | ------------------------------------------------------------------------------------------------------------------ |
| Parametric     | mean         | `[parameters::describe_distribution](https://easystats.github.io/parameters/reference/describe_distribution.html)` |
| Non-parametric | median       | `[parameters::describe_distribution](https://easystats.github.io/parameters/reference/describe_distribution.html)` |
| Robust         | trimmed mean | `[parameters::describe_distribution](https://easystats.github.io/parameters/reference/describe_distribution.html)` |
| Bayesian       | MAP estimate | `[parameters::describe_distribution](https://easystats.github.io/parameters/reference/describe_distribution.html)` |





Following (between-subjects) tests are carried out for each type of analyses-



| Type           | No. of groups | Test                                            | Function used                                                              |
| -------------- | ------------- | ----------------------------------------------- | -------------------------------------------------------------------------- |
| Parametric     | \> 2          | Fisher’s or Welch’s one-way ANOVA               | `[stats::oneway.test](https://rdrr.io/r/stats/oneway.test.html)`           |
| Non-parametric | \> 2          | Kruskal–Wallis one-way ANOVA                    | `[stats::kruskal.test](https://rdrr.io/r/stats/kruskal.test.html)`         |
| Robust         | \> 2          | Heteroscedastic one-way ANOVA for trimmed means | `[WRS2::t1way](https://rdrr.io/pkg/WRS2/man/t1way.html)`                   |
| Bayes Factor   | \> 2          | Fisher’s ANOVA                                  | `[BayesFactor::anovaBF](https://rdrr.io/pkg/BayesFactor/man/anovaBF.html)` |
| Parametric     | 2             | Student’s or Welch’s _t_\-test                  | `[stats::t.test](https://rdrr.io/r/stats/t.test.html)`                     |
| Non-parametric | 2             | Mann–Whitney _U_ test                           | `[stats::wilcox.test](https://rdrr.io/r/stats/wilcox.test.html)`           |
| Robust         | 2             | Yuen’s test for trimmed means                   | `[WRS2::yuen](https://rdrr.io/pkg/WRS2/man/yuen.html)`                     |
| Bayesian       | 2             | Student’s _t_\-test                             | `[BayesFactor::ttestBF](https://rdrr.io/pkg/BayesFactor/man/ttestBF.html)` |



[ggbetweenstats R documentation](https://www.rdocumentation.org/packages/ggstatsplot/versions/0.6.6/topics/ggbetweenstats)


```python

ggbetweenstats(
  data,
  x,
  y,
  plot.type = "boxviolin",
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  p.adjust.method = "holm",
  effsize.type = "unbiased",
  bf.prior = 0.707,
  bf.message = TRUE,
  results.subtitle = TRUE,
  xlab = NULL,
  ylab = NULL,
  caption = NULL,
  title = NULL,
  subtitle = NULL,
  sample.size.label = TRUE,
  k = 2L,
  var.equal = FALSE,
  conf.level = 0.95,
  nboot = 100L,
  tr = 0.1,
  mean.plotting = TRUE,
  mean.ci = FALSE,
  mean.point.args = list(size = 5, color = "darkred"),
  mean.label.args = list(size = 3),
  notch = FALSE,
  notchwidth = 0.5,
  outlier.tagging = FALSE,
  outlier.label = NULL,
  outlier.coef = 1.5,
  outlier.shape = 19,
  outlier.color = "black",
  outlier.label.args = list(size = 3),
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha
    = 0.4, size = 3, stroke = 0),
  violin.args = list(width = 0.5, alpha = 0.2),
  ggsignif.args = list(textsize = 3, tip_length = 0.01),
  ggtheme = ggplot2::theme_bw(),
  ggstatsplot.layer = TRUE,
  package = "RColorBrewer",
  palette = "Dark2",
  ggplot.component = NULL,
  output = "plot",
  ...
)

```





- #### very nice youtube channel from Dr Yury Zablotski for data exploration, cleaning and so on. Very nice tutorial for [dlookr](https://youtu.be/M7eNYbd4n1Y)



- #### theme blog r [here](https://rfortherestofus.com/2019/08/themes-to-improve-your-ggplot-figures/)

- #### a nice example of theme [ggthemr](https://github.com/Mikata-Project/ggthemr#usage) option  fresh [artyfarty option scientific ou empty](https://datarootsio.github.io/artyfarty/articles/introduction.html) 


- #### tldr: Thomas Vroylandt and I have developed a package called pagedreport to help you make beautiful PDF reports from RMarkdown. Full documentation is available [here](https://pagedreport.rfortherestofus.com/)   


- #### [Announcing pagedreport](https://rfortherestofus.com/2021/01/announcing-pagedreport/)


I found the blog r for the rest of us  [here](https://rfortherestofus.com/blog/) simply wonderfull!!!!  Amazing tutorials



[How to make beatifull table in R](https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/)




- #### [Lesson 7. Add Citations and Cross References to an R Markdown Report with Bookdown](https://www.earthdatascience.org/courses/earth-analytics/document-your-science/add-citations-to-rmarkdown-report/)

- #### COLOR SELECTION MAKE EASY [HERE](https://www.w3schools.com/colors/colors_picker.asp) AND [HERE](https://www.color-hex.com/)


- #### facetwap with coloring option of title [here](https://themockup.blog/posts/2020-05-01-tidy-long-models/) and [here](https://www.opensourcefootball.com/posts/2020-09-07-estimating-runpass-tendencies-with-tidymodels-and-nflfastr/)

- #### Setting up a Distill Blog with Netlify [here](https://www.shamindras.com/posts/2019-07-11-shrotriya2019distillpt1/)

- #### Do nice PPT and share it with xarigan and distill package  [here](https://rstudio-education.github.io/teaching-in-production/#prework)


- #### [How to create a blog or a website in R with {Distill} & continuously deploy via Github & Netlify](https://youtu.be/WZt4H-ogH3s)
- #### Creating a Blog with distill [here](https://rstudio.github.io/distill/blog.html) | [Example](https://pkgs.rstudio.com/distill/articles/examples.html)

- #### Building a website using R {distill} [here](https://www.youtube.com/watch?v=Fm3bsYCilEU)

- #### How to create a blog or a website in R with {Distill} package and manually deploy with {Netlify} [here](https://www.youtube.com/watch?v=p9nuRKaF4nM)

- #### Heatmap 


```

library(pheatmap); library(gplots); library(ggplot2); library(colorspace); library(wesanderson)

tpm = delim("Senna_tora_CHS-L.txt", header=T, row.names='gene')
bk = unique(c(seq(0, 0, length=10), 0, seq(0, 13, length=100)))
colors = colorRampPalette(c("white", "red"))(100)
pheatmap(tpm, 
         color = colors,
         breaks = bk,
         cluster_cols=F,
         cluster_rows=F,
         fontsize_col=9,
         fontsize_row=7,
         cellwidth = 260/ncol(tpm), 
         cellheight = 200/nrow(tpm), 
         border_color = "grey",
         show_rownames=T,
         gaps_col = c(1,2,3,4,5,6),
         labels_row = rownames(tpm),
         main=paste(c(nrow(tpm), " CHS-L genes"),collapse=""))
         
 
  ```
 
 
 - #### Ks plot
 
 
 ```
 
 library(ggplot2); library(ggrepel); library(gridExtra)
args = commandArgs(trailingOnly=TRUE)

library(limma)
Cats = c()
KaKs = data.frame()
for (i in args){
  Data = data.frame(category = strsplit2(i, '\\.')[1,1], read.table(i, sep = '', header = T, quote = '', fill = T))
  KaKs = rbind(KaKs, Data)
  Label = strsplit2(i, '\\/')
  Cats = c(Cats, strsplit2(Label[1, ncol(Label)], '\\.')[1,1])
}

KaKs$category = factor(KaKs$category, levels = Cats)
NumOrg = length(Cats)


Ks = ggplot(KaKs) + 
  geom_density(aes(Ks, fill = factor(category)), alpha = 0.3, colour = NA) + 
  scale_fill_manual(values = rainbow(NumOrg)) +
  scale_x_continuous(expand = c(0,0), limits = c(0, 4)) + 
  ggtitle('Ks') +  
  theme_bw(base_size = 6) +
  xlab('Ks') +
  theme(legend.position = 'bottom',legend.title = element_blank(),plot.title = element_text(vjust = 0.5, hjust = 0.5, size = 20))

pdf(paste(c('./Plot_Ks.', Cats, '.pdf'), collapse = ''))
grid.arrange(Ks)
dev.off()

```



`

To calculate the synonymous-substitution Ks values, we selected the orthologous gene pairs between species and the paralogous pairs within a species from the orthology analysis. The selected proteins were further subjected to multiple-sequence alignment with MAFFT v7.305b93 and corrected with Gblocks v0.91b94. The corresponding genomic regions of conserved proteins, which were observed from the corrected multiple alignments, were subjected to Ks calculation using ParaAT v2.0 (ref. 98) with the Yang–Nielsen approach implemented in PAML99. The Ks distribution plot (Supplementary Fig. 27) was drawn using in-house Python and R scripts.

`





[kaks-calculator](https://code.google.com/archive/p/kaks-calculator/downloads)




 - #### [r color inpiration selection](https://htmlcolorcodes.com/)


- #### [R for biologist](http://omgenomics.com/plotting-in-r-for-biologists/)

- color palette   [inspiration](https://www.google.co.kr/imgres?imgurl=https%3A%2F%2Fplastid.readthedocs.io%2Fen%2Flatest%2F_images%2Fstackedbar.png&imgrefurl=https%3A%2F%2Fplastid.readthedocs.io%2Fen%2Flatest%2Fexamples%2Fz_plotting.html&tbnid=qxkqTSGPS4TqXM&vet=12ahUKEwj6p6Od6dntAhVD5ZQKHdO2C5gQMyhUegQIARBx..i&docid=Zi5Bizo5SpBWDM&w=1675&h=1324&q=genome%20plot%20python&hl=fr&ved=2ahUKEwj6p6Od6dntAhVD5ZQKHdO2C5gQMyhUegQIARBx) | [awespme](https://plastid.readthedocs.io/en/latest/examples/z_plotting.html)

- ### Principal Component Analysis | [STHDA Tuto](http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/) | [Visualization](http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining) | [factoextra tuto by Kassambara](https://rpkgs.datanovia.com/factoextra/reference/fviz_pca.html)

- #### [Tidytuesdays Tutorial](https://github.com/jkaupp/tidytuesdays)


- #### [A ggplot2 Tutorial for Beautiful Plotting in R](https://cedricscherer.netlify.app/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/)

- #### [ggplot2 teaching material : Collection of teaching materials for R](https://github.com/Z3tt/ggplot-courses) 
- #### [ggplot2-tutorial](https://github.com/jennybc/ggplot2-tutorial)


- #### [super-and-sub-script-labels.R](https://gist.github.com/benmarwick/5cfeda1d20ff9ab3231a5de8fac36213)
- #### [polyshing a ggplot2 graph](https://ggplot2-book.org/polishing.html)


- #### [Remove 'a' from legend when using aesthetics and geom_text: Reponse 9](https://stackoverflow.com/questions/18337653/remove-a-from-legend-when-using-aesthetics-and-geom-text)


- #### [Find easily color hexa code](https://www.colorhexa.com/) | [website2](https://www.color-hex.com/)
- #### [Showing data values on stacked bar chart in ggplot2](https://intellipaat.com/community/12428/showing-data-values-on-stacked-bar-chart-in-ggplot2)


- #### [flextable](https://davidgohel.github.io/flextable/articles/overview.html)

- #### [y-axis on the right side · Len Kiefer](http://lenkiefer.com/2020/11/20/y-axis-on-the-right-side/)

- ### [acm](http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/75-acm-analyse-des-correspondances-multiples-avec-r-l-essentiel/)

- #### [How to export high quality images in R](https://aebou.rbind.io/posts/2020/11/how-to-export-high-quality-images-in-r/)

- #### [Comment exporter des images de haute qualité avec R](https://aebou.rbind.io/fr/posts/2020/11/comment-exporter-des-images-de-haute-qualit%C3%A9-avec-r/)

- #### [New Version of Patchwork is Here: Inset a plot inside a plot](https://cmdlinetips.com/2020/11/insetting-with-patchwork/) [tuto2](https://www.data-imaginist.com/2020/insetting-a-new-patchwork-version/) | [tuto3](https://www.r-bloggers.com/2020/11/insetting-a-new-patchwork-version/) 

- #### [tips-to-combine-multiple-ggplots-using-patchwork](https://cmdlinetips.com/2020/01/tips-to-combine-multiple-ggplots-using-patchwork/)

- #### [Grouped Stacked bar plot with ggplot](https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html)  | [Dealing with colors in ggplot2](https://www.r-graph-gallery.com/ggplot2-color.html) | [Paletteer package](https://github.com/EmilHvitfeldt/r-color-palettes)

- #### [Supprimer la legende](https://www.datanovia.com/en/fr/blog/gpplot-comment-supprimer-la-legende/)

- #### [Label bar chart ](https://thomasadventure.blog/posts/labels-ggplot2-bar-chart/)
- #### [inspi charles bodet ggplot2](https://www.charlesbordet.com/en/make-beautiful-charts-ggplot2/#)
- #### [BBC](https://bbc.github.io/rcookbook/)  [Exampple](http://www.interhacktives.com/2020/03/09/how-to-create-bbc-style-graphics-with-r-a-bbplot-tutorial/)
- #### [R graph galery ggplot2](https://www.r-graph-gallery.com/ggplot2-package.html)
- #### [Data to viz](https://www.data-to-viz.com/)

- #### [A ggplot2 Tutorial for Beautiful Plotting in R](https://cedricscherer.netlify.app/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/)
- #### [Tidytuesday 2020](https://github.com/jwatzek/tidytuesday)

- #### inspi geom area en haut (plant height par example) et barre pourcentage en bas (pour le nombre de feuille vert et jaune et mort) , vrainspiration [ici](https://twitter.com/watzoever/status/1199508300959752192?ref_src=twsrc%5Etfw%7Ctwcamp%5Etweetembed%7Ctwterm%5E1199508300959752192%7Ctwgr%5Eshare_3%2Ccontainerclick_0&ref_url=https%3A%2F%2Fcedricscherer.netlify.app%2F2019%2F12%2F30%2Fbest-tidytuesday-2019%2F) ... le code dur github est [ici](https://github.com/jwatzek/tidytuesday)

- #### [inspi](https://cedricscherer.netlify.app/2020/01/09/australian-bushfires-comparison-europe-world/)

- #### [Inspi](https://frontpagedata.com/examples/)
- #### [axis label with ggplot2](http://environmentalcomputing.net/plotting-with-ggplot-adding-titles-and-axis-names/#:~:text=To%20alter%20the%20labels%20on,line%20of%20basic%20ggplot%20code.&text=Note%3A%20You%20can%20also%20use,which%20is%20equivalent%20to%20ggtitle%20.)
- ##### [annotation ggplot2](https://viz-ggplot2.rsquaredacademy.com/textann.html)
- #### [Ajouter les pvalues sur un ggplot, manuellement](https://statistique-et-logiciel-r.com/ajouter-pvalues-sur-ggplot/)

- #### [order x axis following a recise axix](https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html)

- #### [Modifier les axes color face et autres](http://www.sthda.com/french/wiki/ggplot2-graduation-des-axes-guide-pour-personnaliser-les-etiquettes-des-graduations-logiciel-r-et-visualisation-de-donnees)


- #### [barplot with error bar](https://www.r-graph-gallery.com/4-barplot-with-error-bar.html) or [here](http://www.sthda.com/french/wiki/ggplot2-barres-d-erreur-guide-de-demarrage-rapide-logiciel-r-et-visualisation-de-donnees)
- #### [Chicklet Stacked Bar Chart](https://www.mikelee.co/posts/2020-02-08-recreate-fivethirtyeight-chicklet-stacked-bar-chart-in-ggplot2/)

- #### [Distill for R Markdown: Creating a Website](https://rstudio.github.io/distill/website.html)

- #### [A Maize Color Palette Generator](https://github.com/AndiKur4/MaizePal)

- #### [Exporting editable ggplot graphics to PowerPoint with officer and purrr](https://www.pipinghotdata.com/posts/2020-09-22-exporting-editable-ggplot-graphics-to-powerpoint-with-officer-and-purrr/)
- #### [Learning Statistics with R](https://learningstatisticswithr.com/)
- #### [Becoming An rvest Magician](https://zachbogart.com/post/rvest-wizardry/)
- #### [How to zoom in on a plot in R](https://datavizpyr.com/how-to-zoom-in-on-a-plot-in-r/)

- #### [Top 50 Rggplot2 visualization items](http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html)

- #### [Rboot Camp: A free online R course](https://r-bootcamp.netlify.app/)
- #### [The best books on Computer Science for Data Scientists recommended by Hadley Wickham](https://fivebooks.com/best-books/computer-science-data-science-hadley-wickham/)
- #### [flextable R package](https://davidgohel.github.io/flextable/)
- #### [PLOTTING SIGNIFICANCE ON THE TOP A BAR CHART OPTION1](https://nakedstatistics.wordpress.com/2017/05/11/plotting-significance/)
- #### [PLOTTING SIGNIFICANCE ON THE TOP A BAR CHART OPTION2](http://131.111.177.41/statistics/R/graphs2.html)
- #### [T TEST](http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r)
- #### [R studio data sheet](https://rstudio.cloud/learn/cheat-sheets)
- #### [R Cookbook](http://www.cookbook-r.com/)


- #### [ggupset](https://github.com/const-ae/ggupset)

- #### [How to Change X and Y Axis Values from Real to Integers in ggplot2?](https://datavizpyr.com/change-x-and-y-axis-values-to-integers-ggplot2/)




# R Map

- #### [ggplot2 official guide for map](https://ggplot2-book.org/maps.html)


- #### [How to get terrain data and draw spatial maps in R using ggplot2? | StatswithR | Arnab Hazra](https://youtu.be/tX8RaTp7jL8)

- #### [Choropleth maps with geom_sf() | Professional dataviz with ggplot2 | R](https://youtu.be/nlcZcWJiyX8)

- #### [Raster maps with geom_raster() | Professional dataviz with ggplot2 | R](https://youtu.be/NQZNpyEgVss)

- #### [Create a map of a country with the neighbouring countries and an inset map](https://aebou.rbind.io/posts/2020/11/create-a-map-of-a-country-with-the-neighbouring-countries-and-an-inset-map/)

- #### [Interactive map with R](https://thinkr.fr/cartographie-interactive-avec-r-la-suite/)
- #### [Interactive map with R and Leaflet](https://thinkr.fr/cartographie-interactive-comment-visualiser-mes-donnees-spatiales-de-maniere-dynamique-avec-leaflet/)
- #### [Geocomputation with R](https://geocompr.robinlovelace.net/)
- #### [R as GIS, part 1: vector](https://datascienceplus.com/r-as-gis-part-1-vector/)

# R Markdown

- #### [Remedy provides addins to facilitate writing in markdown with RStudio](https://github.com/ThinkR-open/remedy) 
