
[Jakub Nowosad](https://nowosad.github.io/)  | [PPT](https://nowosad.github.io/SIGR2021/lecture/lecture.html#64)





![img](https://pbs.twimg.com/media/E5DKj-3X0AMFxnW?format=jpg&name=4096x4096)



code is [here](https://github.com/GuillemSalazar/tidytuesday/blob/main/2021/week_27/code/tidytuesday.R)







[get discrete palette by package and name](https://rdrr.io/cran/paletteer/man/paletteer_d.html)







raster file download [1](https://rspatial.org/raster/sdm/4_sdm_envdata.html) | [2](https://rspatial.org/raster/spatial/8-rastermanip.html)

```r

filename <- system.file("external/rlogo.grd", package="raster")

brick(filename)



nlayers(b)


```











Today 23 june 2021


Inkscape


Make a curve : Use Bezier to draw and Shit + A to make a curve at a specific point

Then fill with a color of your choice by selecting Fill bounded area  

Then harmonize the area and stroke color 

Then move the filled shape to the place you want

Then Click End key to put in in backfront












Dr Tovignan...Please check this answer


**Introductive note**

A geographic information system (GIS) is a system that creates, manages, analyzes, and maps all types of data. GIS connects data to a map, integrating location data (where things are) with all types of descriptive information (what things are like there). Numerous data format are available in GIS. An exhaustive list can be found [here](https://gisgeography.com/gis-formats/). 

**First session**

In the first session, we dealt with the ESRI Shapefile, a widely used GIS data format. The shapefile format is a geospatial vector data format for geographic information system (GIS) software. Using this vector format, we learnt how to manipulate this data, how to add additionnal information and visualize it.

**Second session**

In the second session, we will dive into another GIS file format called Raster format. Raster data is made up of pixels (also referred to as grid cells). They are usually regularly spaced and square but they don’t have to be. Rasters have pixels that are associated with a value (continuous) or class (discrete). The main objective of this second session is to introduce the raster data manipulation in R and how to unravel the information that it contains. 

**Summary**

In summary, we introduced how to handle a vector data in the last session. We will introduce the raster data type in the next one.

**Application note**

Rater data are frequently used to store environmental and climatic data including: rainfall, forest distribution and/or coverage, water, agriculture, land, vegetation, wild animal tracking and management, marine species management, iceberg evolution tracking etc... 


Thank you.







[Make a website easily with wowchemy hugo theme](https://wowchemy.com/templates/)









[Ridgeline Plots in R (3 Examples)](https://statisticsglobe.com/ridgeline-plots-in-r)


![img](https://statisticsglobe.com/wp-content/uploads/2021/06/figure-3-ridgeline-plots-in-r.png)





[Bubble Chart in R-ggplot & Plotly](https://www.r-bloggers.com/2021/06/bubble-chart-in-r-ggplot-plotly/)

![img](https://i2.wp.com/finnstats.com/wp-content/uploads/2021/06/bubble-chart-in-R.png?zoom=1.25&w=450&ssl=1)














xarigan [tuto 1](http://arm.rbind.io/slides/xaringan.html#1)  | [tuto 2](https://annakrystalli.me/talks/xaringan/xaringan.html#1) | [tuto 3](https://alison.rbind.io/project/rladies-xaringan/) | [tuto 4](https://www.katiejolly.io/blog/2021-03-16/designing-slides) | [Code couleur](https://htmlcolorcodes.com/fr/)


https://datacarpentry.org/organization-geospatial/01-intro-raster-data/index.html


https://desktop.arcgis.com/en/arcmap/10.3/manage-data/raster-and-images/what-is-raster-data.htm

http://www.worldclim.com/bioclim


https://worldclim.org/data/v1.4/formats.html


https://worldclim.org/data/bioclim.html

https://emilypiche.github.io/BIO381/raster.html



```r

# downloading the bioclimatic variables from worldclim at a resolution of 30 seconds (.5 minutes)
r <- getData("worldclim", var="bio", res=0.5, lon=-72, lat=44)
# lets also get the elevational data associated with the climate data
alt <- getData("worldclim", var="alt", res=.5, lon=-72, lat=44)

```

















[Introduce yourself online using blogdown and Hugo Apèro](https://youtu.be/RksaNh5Ywbo)





Using Tidyverse for Genomics - Workshop animated by Dr. Janani Ravi [Video](https://youtu.be/m-7knbiSf9A) | [Blog](https://jananiravi.github.io/workshop-tidyverse/genomics/workshop-tidyverse-genomics.html)














[15 Tips to Customize lines in ggplot2 with element_line()](https://cmdlinetips.com/2021/05/tips-to-customize-lines-in-ggplot2-with-element_line/)

![img](https://i1.wp.com/cmdlinetips.com/wp-content/uploads/2021/05/ggplot2_element_line_tips.png?w=660&ssl=1)





















- ####   Water map in African countries

![img](https://github.com/Yedomon/R/blob/main/code.jpg)


[Code](https://github.com/elidom/my_TidyTues/blob/master/week_19/water.Rmd)




- #### Diverging stacked barplot with ggplot2



The [blog](https://luisdva.github.io/rstats/Diverging-bar-plots/) 

Main code


```r

library(dplyr)
library(ggplot2)
library(extrafont)
devtools::install_github('bart6114/artyfarty')
library(artyfarty)

vegSurvey <- vegSurvey %>%  mutate(sppInv= ifelse(veg_Type =="native",spp,spp*-1))

# plot for only the North slope

vegSurvey %>% filter(slope=="North") %>% 
ggplot(aes(x=sampling_point, y=sppInv, fill=veg_Type))+
  geom_bar(stat="identity",position="identity")+
  xlab("sampling point")+ylab("number of species")+
  scale_fill_manual(name="Plant type",values = c("#FFA373","#50486D"))+
  coord_flip()+ggtitle("North slope")+
  geom_hline(yintercept=0)+
  xlab("Sampling Points")+
  ylab("Species number")+
  scale_y_continuous(breaks = pretty(vegSurvey$sppInv),labels = abs(pretty(vegSurvey$sppInv)))+
  theme_scientific()

```



Output




![img](https://luisdva.github.io/assets/images/northslope.png)




A second more details tutorials


The blog [post](https://www.onceupondata.com/2019/01/25/ggplot2-divergent-bars/)



The code:


```r
## calculate breaks values
breaks_values <- pretty(starwars_chars$average_height)


## create plot
starwars_chars %>%
  ggplot(aes(x = homeworld, y = average_height, fill = gender))+
  geom_hline(yintercept = 0)+
  geom_bar(stat = "identity")+
  coord_flip()+
  scale_y_continuous(breaks = breaks_values,
                     labels = abs(breaks_values))+
  theme_minimal()+
  scale_fill_manual(values = c("#bf812d", "#35978f"))


```


Output


![img](https://www.onceupondata.com/post/2019-01-25-ggplot2-divergent-bars_files/figure-html/starwars3-1.png)






- #### Multiple views on how to choose a visualization | [Medium post](https://medium.com/multiple-views-visualization-research-explained/multiple-views-on-how-to-choose-a-visualization-b3ffc99fcddc) | [Indrajeet Patil POST](https://twitter.com/patilindrajeets/status/1380437894859530240) 


![img](https://pbs.twimg.com/media/EyhM8lxW8AEnuyL?format=jpg&name=large)

- #### [Lesson 5: Spatial Data Analysis in R](https://youtu.be/nQVFqkifeSE)
- #### [Plotting a ggtree and ggplots side by side](https://www.r-bloggers.com/2018/10/plotting-a-ggtree-and-ggplots-side-by-side/)


- #### [vhs palette](https://github.com/cj-holmes/vhs)

- #### [Machine learning with R](https://github.com/Yedomon/R/blob/main/lmlcr.pdf)

- #### [Which color scale to use when visualizing data](https://blog.datawrapper.de/which-color-scale-to-use-in-data-vis/)



- #### [Making Extra Great Slides](https://www.garrickadenbuie.com/talk/extra-great-slides-nyhackr/)


- #### [ggplot Wizardry: My Favorite Tricks and Secrets for Beautiful Plots in R ](https://github.com/Z3tt/OutlierConf2021) | [PPT](https://www.cedricscherer.com/slides/OutlierConf2021_ggplot-wizardry.pdf)



![g](https://raw.githubusercontent.com/Z3tt/OutlierConf2021/main/img/2021_outlier.png)

- #### [mfiz](http://mfviz.com/)

- #### [Plagiarism checker at jbnu](https://iei.jbnu.ac.kr) | [LOAD](https://jbnu.copykiller.com/myspace/upload)

- #### Graph color choice example from [2020 | The citation advantage of linking publications to research data](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0230416)


![figure](https://journals.plos.org/plosone/article/figure/image?size=large&id=10.1371/journal.pone.0230416.g002)

# R shiny

- #### [Shiny Apps: Development and Deployment](https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/shiny-apps/)
- #### [Shiny Apps: Development and Deployment Video](https://www.youtube.com/watch?v=QT3WUQu99pM)
- #### [R Recipe: Shiny Tables from Excel Templates](https://ljupcho.com/blog/shiny-excel-table-templates)


Hi Dear Yury Zablotski. Thanks again for this amazing tutorial. I was able to set up and deploy my blog website. However I encountered this issue when writing a blog today.


See:

```

Error in read_xml.character(rss_path) : 
  Input is not proper UTF-8, indicate encoding !
Bytes: 0xE9 0x76 0x72 0x2E [9]
Calls: <Anonymous> ... write_feed_xml -> <Anonymous> -> read_xml.character
Furthermore : Warning messages:
1: In (function (category = "LC_ALL", locale = "")  :
  the OS request to specify the location to "en_US.UTF-8" could not be honored
2: In (function (category = "LC_ALL", locale = "")  :
  the OS request to specify the location to "en_US.UTF-8" could not be honored
3: In (function (category = "LC_ALL", locale = "")  :
  the OS request to specify the location to "en_US.UTF-8" could not be honored
Execution stopped

```


Here is the github link for the site ---> https://github.com/Yedomon/My-website


Here is the deployed website ---> https://yedomon.netlify.app/

Thank you.



# R

```r

sma analysis to get slope and elevation p value

#####

install.packages("smatr")


### load

library(smatr)
b = sma(longev~lma+rain, type="shift", data=leaf.low.soilp) # change type shift to elevation or elev.test=1 or slope.test=1 I do not know
summary(b)



#ggplot2 style
fit1=lm(longev~lma+rain,data=leaf.low.soilp)
summary(fit1)

equation1=function(x){coef(fit1)[2]*x+coef(fit1)[1]}
equation2=function(x){coef(fit1)[2]*x+coef(fit1)[1]+coef(fit1)[3]}

ggplot(leaf.low.soilp,aes(y=longev,x=lma,color=rain))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])



## ggPredict style

ggPredict(fit1,se=TRUE,interactive=FALSE) + 
  theme_minimal()+
  theme(legend.position = "top")+
  labs(x="lma in cm", y = "longev in cm") +
  annotate('text', x = 70, y = 4, label = 'p-value < 0.01)') +
  scale_fill_manual(values = c("#D16103", "#C3D7A4", "#52854C"))+
  scale_color_manual(values = c("#D16103", "#C3D7A4", "#52854C"))

# color selection [here](https://www.datanovia.com/en/fr/blog/couleurs-ggplot-meilleures-astuces-que-vous-allez-adorer/)

```


- #### [Assessing Clustering Tendency](https://www.datanovia.com/en/lessons/assessing-clustering-tendency/)

- #### [Cluster Validation Statistics: Must Know Methods](https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/)


- #### [good tuto simple analysis in R](https://biostats.w.uib.no/9-pastecs/)

- #### [setup distill](https://www.shamindras.com/posts/2019-07-31-shrotriya2019distillpt2/)

- #### [smatr 3– an R package for estimation and inference about allometric lines | sma](https://www.rdocumentation.org/packages/smatr/versions/3.4-8/topics/sma)

- #### [An introduction to repeatability estimation with rptR](https://cran.r-project.org/web/packages/rptR/vignettes/rptR.html) | [Installation](https://www.rdocumentation.org/packages/rptR/versions/0.9.22)

- #### ggiraphExtra | Multiple regression model without interaction | ggPredict | [Link](https://cran.r-project.org/web/packages/ggiraphExtra/vignettes/ggPredict.html)

- ##### a great alternative [here](https://rpkgs.datanovia.com/ggpubr/reference/stat_regline_equation.html) with ggpubr


- #### correlation with ggstatplot [r documentation](https://www.rdocumentation.org/packages/ggstatsplot/versions/0.2.0/topics/ggcorrmat) | [PPT](https://indrajeetpatil.github.io/ggstatsplot_slides/slides/ggstatsplot_presentation.html#34)


- #### [ANOVA tutorial and adding an icon on r plot](https://ourcodingclub.github.io/tutorials/anova/)

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


```r

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





- #### very nice youtube channel from Dr Yury Zablotski for data exploration, cleaning and so on. Very nice tutorial for [dlookr](https://youtu.be/M7eNYbd4n1Y) | [the blog](https://yuzar-blog.netlify.app/posts/2021-01-30-r-package-reviews-dlookr-diagnose-explore-and-transform-your-data/)



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


```r

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
 
 
 ```r
 
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




A new script for ka ks plot | data is [here](https://github.com/Yedomon/R/blob/main/data_th.csv)



```r


ggplot(data_th, aes(x=ks, color=species)) + 
  geom_density(size = 0.75)  +
  scale_x_continuous(name = "Substitutions per synonymous site (Ks)", limits =c(0,4)) + 
  scale_y_continuous(name="Density") + 
  scale_color_brewer(palette ="Paired") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50")) +
  theme(legend.position = c(0.8, 0.7))


```





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

- #### [1](https://www.r-bloggers.com/2019/04/zooming-in-on-maps-with-sf-and-ggplot2/) | [2](https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html) | [3](http://search.r-project.org/library/ggplot2/html/ggsf.html) | [4](https://geanders.github.io/navy_public_health/3-2-basic-mapping.html#basic-mapping) | [5](https://semba-blog.netlify.app/01/26/2020/world-map-and-map-projections/) [7](https://bookdown.org/robinlovelace/geocompr/spatial-class.html)



In cases when a coordinate reference system (CRS) is missing or the wrong CRS is set, the st_set_crs() function can be used:

```
new_vector = st_set_crs(new_vector, 4326) # set CRS
#> Warning: st_crs<- : replacing crs does not reproject data; use st_transform
#> for that
The warning message informs us that the st_set_crs() function does not transform data from one CRS to another.

```

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
