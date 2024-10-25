# MoBC-Net

<!-- badges: start -->
<!-- badges: end -->

The goal of MoBC-Net is designed to quantify the distance between distinct modules-of-interest (MOIs) within a network and identify key link nodes using a novel metric termed module-betweenness centrality (MoBC).

<img width="347" alt="image" src="https://github.com/user-attachments/assets/0bcf86aa-eb73-4648-8177-56732a365a13">
![figure1](https://github.com/user-attachments/assets/467a0240-94f5-4fff-a4de-a873b436af35)

## Installation

You can install the development version of MoBCnet from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hanbi-99/MoBC-Net")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(MoBCnet)
library(dplyr)

#--- load file
module.file <- system.file("data","comm.breast.csv", package='MoBCnet')
ppi.file <- system.file("data","human.ppi.network.csv", package='MoBCnet')

modules <- read.csv(module.file)
modules <- split(modules[,1], modules[,2])
ppi <- read.csv(ppi.file)

#--- calculate distance
mobc.res <- CommDistFunction(ppi,modules, show.binning=FALSE)
mobc.res@MoBCresults$p.adj <- p.adjust(mobc.res@MoBCresults$pvalue, "BH")
mobc.res@MoBCresults %>% arrange(p.adj) %>% head

#--- get the top link genes
link.gene.res <- MoBC.genes(MoBC.result=mobc.res, module1.name='M2', module2.name="M3",
                                random=1000, ratio=0.1, cal.p=FALSE)
link.gene.res %>% head

plotMoBC_genes(MoBC.result=mobc.res, module1.name='M2', module2.name='M3', 
                    top=5, module1.color='lightblue1',module2.color='lightpink')

plotDist(MoBC.result <- mobc.res, pval=0.005)


```

