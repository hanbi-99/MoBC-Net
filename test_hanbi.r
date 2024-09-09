



library(devtools)
library(usethis)


#--------- https://usethis.r-lib.org/

# Create a new package -------------------------------------------------
path <- file.path("/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/hanbi/Script/Project/MoBC-net/", "MoBCnet")
create_package(path)
# only needed since this session isn't interactive
proj_activate(path)


# Modify the description ----------------------------------------------
use_mit_license("Testname")

use_package("igraph", 'imports')
use_package("magrittr",'imports')


# Set up other files -------------------------------------------------
use_readme_md()

use_news_md()

use_test("my-test")

x <- 1
y <- 2
use_data(x, y)

# Use git ------------------------------------------------------------
use_git()



#--------------- TEST


# https://r-pkgs.org/man.html


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


#-------- Run
# toy network csv 파일 및 community gene list 파일 받기
toy_network<- read.csv(paste0(fdir.set,"/toy_example/toy_network.csv"))
comm.genelist.final <- readRDS(paste0(fdir.set,"/toy_example/comm.genelist.rds"))

# cancer pathway genes2.csv : RTK, RAS 합쳐서
# cancer pathway genes.csv : RTK, RAS (이걸로 tp53 vs cell cycle)
# --> gene list toupper



# human.ppi.network.csv : test network


# GetDistance.r
# community distance measure, method에서 distance measure 방식 설정
dist.results <- CommDistFunction(
                 toy_network,
                 comm.genelist.final,
                 random= 100,
                 overlap_filtering = TRUE
                #  method= 'closest'
                 )

# network=toy_network
# community.genelist = comm.genelist.final

class(dist.results)
names(dist.results)

re1= Get.ConnectingGene(dist.results, 'union.C6_16','union.C2_62')
re2= Get.ConnectingGene(dist.results, 'Stat3','union.C2_62')
re3 = Get.Centrality(dist.results, 'union.C6_16','union.C2_62')


head(re1)
head(re3)



cor(re3[rownames(re1),'hub.score'] , re1$normalized.freq, method='spearman')


re1= Get.ConnectingGene(dist.results, 'comm1','union.C2_62')
re2= Get.ConnectingGene(dist.results, 'g1','union.C2_62')
re3 = Get.Centrality(dist.results, 'a','union.C2_62')
head(re3)

g = dist.results$graph
community1=dist.results$filtered.community[['union.C1_83']]
community2=dist.results$filtered.community[['union.C2_62']]

plotDist(dist.results)

#-------- other network (Hanbi)

load(file=paste0(DIR_RDATA,"/Cachexia/CAD_DEG_updateInfo.Rdata")) 
# fsinfo1, cp.degl, cp.degl.up, cp.degl.dn, cp.allg,
#             degl.up, degl.dn, degl.all,col.list, 
#             all.cls, savestring, kegg.pml

load(file=paste0(DIR_RDATA,"/Cachexia/CAD_DEG_v2.Rdata")) 
# ginfol.all, eml, all.fcl, all.fcm, all.qm,
# 	degl.up, degl.dn, fsinfo, fgid1, ginfo_hs, 

# load(file=paste0(DIR_scRDATA, "/all_sc_data_and_deg_vSS.Rdata"))
# # obj, deg.rel, 

# sc.degl.up = lapply(deg.rel, function(xx) subset(xx, avg_log2FC > log2(1.5) & p_val_adj <0.05)$gene_name)



# GetDistance.r
# community distance measure, method에서 distance measure 방식 설정
dist.results <- CommDistFunction(
                 savestring,
                 all.cls[grep('cancer',names(all.cls))],
                 random= 1000,
                 method= 'closest')


class(dist.results)
dist.results@MoBCresults %>% dplyr::arrange(z_score)

re1= MoBC.genes(dist.results, 'cancer_3','cancer_8') # 76905 : 'Lrg1'
re1$gene_name = fgid1[match(re1$gene, fgid1$EntrezID),'gene_name']
head(re1,20)

plotDist(dist.results)


#---------------------- validation set
# cancer pathway genes2.csv : RTK, RAS 합쳐서
# cancer pathway genes.csv : RTK, RAS (이걸로 tp53 vs cell cycle)
# --> gene list toupper

# toy network csv 파일 및 community gene list 파일 받기

gl<- read.csv(paste0(fdir.set,"/toy_example/cancer pathway genes.csv"))
toy_network <- read.csv(paste0(fdir.set,"/toy_example/human.ppi.network.csv"), row.names=1)

gl1 = strsplit(gl$Genes, ', ') %>% 'names<-'(gl$Pathway) %>% lapply(toupper)




# human.ppi.network.csv : test network


# GetDistance.r
# community distance measure, method에서 distance measure 방식 설정
dist.results <- CommDistFunction(
                 toy_network,
                 gl1,
                 random= 100,
                 overlap_filtering = TRUE
                #  method= 'closest'
                 )

# network=toy_network
# community.genelist = comm.genelist.final

class(dist.results)
names(dist.results)

plotDist(dist.results)
re1 = MoBC.genes(dist.results, 'TP53','Cell Cycle')

head(re1)

