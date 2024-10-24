
# MoBCnet

<!-- badges: start -->
<!-- badges: end -->

The goal of MoBCnet is to ...

## Installation

You can install the development version of MoBCnet from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hanbi-99/MoBC-net")
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

