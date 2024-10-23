library(magrittr)




user.id <- getwd()
user.id <- gsub('/.*','',gsub('/Users/','',user.id))


if(user.id=='hanbilee'){
    dir.set = paste0('/Users/',user.id,'/Library/CloudStorage/OneDrive-GCCORP/문서 - AI & BI 연구팀 - Community distance/Community distance/Package')
}else if(user.id=='yoomibaek'){
    dir.set = paste0('/Users/',user.id,'/Library/CloudStorage/OneDrive-GCCORP/Community distance/Package')
}

source(paste0(dir.set,'/package_script/ConstructNetwork.r'))
source(paste0(dir.set,'/package_script/Utility.r'))
source(paste0(dir.set,'/package_script/GetDistance.r'))
source(paste0(dir.set,'/package_script/Module_centrality.r'))



#-------- Run
# toy network csv 파일 및 community gene list 파일 받기
toy_network<- read.csv(paste0(dir.set,"/toy_example/toy_network.csv"))
comm.genelist.final <- readRDS(paste0(dir.set,"/toy_example/comm.genelist.rds"))


# GetDistance.r
# community distance measure, method에서 distance measure 방식 설정
dist.results <- CommDistFunction(
                 toy_network,
                 comm.genelist.final,
                 random= 100,
                 overlap_filtering = TRUE,
                 method= 'closest')


names(dist.results)

class(dist.results[[3]])

plotDist(dist.results)
re1 = MoBC.genes(dist.results, 'union.C2_62','union.C1_83')


# re1= Get.ConnectingGene(dist.results, 'union.C6_16','union.C2_62')
# re2= Get.ConnectingGene(dist.results, 'Stat3','union.C2_62')
# re3 = Get.Centrality(dist.results, 'union.C6_16','union.C2_62')


# head(re1)
# head(re3)

# cor(re3[rownames(re1),'hub.score'] , re1$normalized.freq, method='spearman')


# re1= Get.ConnectingGene(dist.results, 'comm1','union.C2_62')
# re2= Get.ConnectingGene(dist.results, 'g1','union.C2_62')
# re3 = Get.Centrality(dist.results, 'a','union.C2_62')
# head(re3)

# g = dist.results$graph
# community1=dist.results$filtered.community[['union.C1_83']]
# community2=dist.results$filtered.community[['union.C2_62']]

