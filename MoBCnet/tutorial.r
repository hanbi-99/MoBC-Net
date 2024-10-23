


#-------------------------------------------------------------------------------------
#--------------- run

setwd('/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/hanbi/Script/Project/MoBC-net/MoBCnet')
# devtools::document()

dir.set='/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/hanbi/Script/Project/MoBC-net/MoBCnet/R/'
ff = list.files(dir.set)


for(ii in ff) source(paste0(dir.set,ii))

library(magrittr)

user.id <- getwd()
user.id <- gsub('/.*','',gsub('/Users/','',user.id))


if(user.id=='hanbilee'){
    fdir.set = paste0('/Users/',user.id,'/Library/CloudStorage/OneDrive-GCCORP/문서 - AI & BI 연구팀 - Community distance/Community distance/Package')
}else if(user.id=='yoomibaek'){
    dir.set = paste0('/Users/',user.id,'/Library/CloudStorage/OneDrive-GCCORP/Community distance/Package')
}






#-------------------------------------------------------------------------------------
#--------------- run

library(dplyr)
library(MoBCnet)
# tutorial

#--- load file
module.file = system.file("data","comm.breast.csv", package='MoBCnet')
ppi.file = system.file("data","human.ppi.network.csv", package='MoBCnet')

modules <- read.csv(module.file)
modules = split(modules[,1], modules[,2])
ppi <- read.csv(ppi.file)

mobc.res = CommDistFunction(ppi,modules, show.binning=FALSE)
mobc.res@MoBCresults$p.adj = p.adjust(mobc.res@MoBCresults$pvalue, "BH")
mobc.res@MoBCresults %>% arrange(p.adj) %>% head

link.gene.res = MoBC.genes(MoBC.result=mobc.res, module1.name='M2', module2.name="M3",
                                random=1000, ratio=0.1, cal.p=FALSE)
link.gene.res %>% head

plot.MoBC.genes(MoBC.result=mobc.res, module1.name='M2', module2.name='M3', 
                    top=5, module1.color='lightblue1',module2.color='lightpink')

plot.Dist(MoBC.result = mobc.res, pval=0.005)


