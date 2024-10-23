


library(dplyr)
library(clusterProfiler)

#--------------- TEST

fn = '/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/문서 - AI & BI 연구팀 - Community distance/Community distance/Validation data results/breast.comm.validation.RData'
load(fn)
# [1] "allid"                          "breast.deg.results.700.2"      
# [3] "CONFIG"                         "dateTag"                       
# [5] "fn"                             "human.ppi.network.final"       
# [7] "res2"                           "tcga.brca.tpm.paired.corr.melt"
# res2.sd


#----------- gene id mapping
stringhidf = '/Users/hanbilee/Downloads/9606.protein.aliases.v12.0.hs.txt.gz'

stringg = read.csv(stringhidf, header=F, sep='\t')
colnames(stringg) = c('protein','symbol','source')
stringg = stringg[-1,]


# entrez
pr2entrez = subset(stringg, source %in% c('UniProt_DR_GeneID'))
pr2entrez1 = unique(pr2entrez[,1:2]) %>% 'colnames<-'(c('protein','EntrezID'))

pr2ensembl = subset(stringg, source %in% c('Ensembl_gene'))
pr2ensembl1 = unique(pr2ensembl[,1:2]) %>% 'colnames<-'(c('protein','gene_id'))

pr2name = subset(stringg, source %in% c('UniProt_GN_Synonyms','UniProt_GN_Name','KEGG_NAME','Ensembl_EntrezGene'))
pr2name1 = unique(pr2name[,1:2]); colnames(pr2name1)[2] = 'gene_name'

# collapse
pr2id = merge(pr2entrez1, pr2ensembl1)
dim(pr2id)
pr2id.symbol = merge(pr2id, pr2name1)
dim(pr2id)


#----------- DEG
deg.res = res2.sd[,1:7] %>% unique
deg.res1 = merge(deg.res, pr2id)

use.deg = subset(deg.res1, abs(log2FoldChange)> 2 & padj < 0.01)$EntrezID %>% unique
length(use.deg)

bgg = deg.res1$EntrezID %>% unique

all.pm = enrichKEGG(use.deg, 
    organism="hsa", 
    # ont='BP',
    # OrgDb ='org.Mm.eg.db' ,
    universe=as.character(bgg),
    pAdjustMethod = "BH", pvalueCutoff = 1.1, qvalueCutoff = 1.1)

all.pm = all.pm@result

head(all.pm,20)

#-------------------------------------------------------------------------------------
# load string
stringhf = '/Users/hanbilee/Downloads/9606.protein.links.v12.0.hs.txt.gz'


string = data.table::fread(stringhf)
string = as.data.frame(string)

string1 <- left_join(string, pr2entrez1, by=c("protein1"="protein"))
string1 <- left_join(string1, pr2entrez1, by=c("protein2"="protein"))
colnames(string1) <- c("protein1", "protein2", "combined_score", "gene1", "gene2")
string1 = string1[!is.na(string1$gene1),]
string1 = string1[!is.na(string1$gene2),]

stringh = subset(string1, combined_score >= 400 & gene1 %in% deg.res1$EntrezID & gene2 %in% deg.res1$EntrezID)
stringh = stringh[,c('gene1','gene2','combined_score')] %>% unique
dim(stringh)

#-------------------------------------------------------------------------------------
# walktrap
library(igraph)
string.cut = c(400,700)
cluster.cutoff = 15

type.cls = lapply(string.cut, function(cutoff){

	use.geneset = use.deg
	cat(cutoff, ' - gene space : ', length(use.geneset),'\n') #3591

	savestring1 = subset(stringh, gene1 %in% use.geneset & gene2 %in% use.geneset & combined_score>=cutoff)
	# savestring1 = subset(savestring1, combined_score >= score_cut)
	colnames(savestring1)[3] = 'weight'

	ntkg = graph_from_data_frame(savestring1[,1:2], directed=FALSE)
	ntkg = igraph::simplify(ntkg, remove.multiple = TRUE, remove.loops = TRUE)
	cat(is_simple(ntkg),'\n')
	cat('# of nodes : ', length(V(ntkg)),'\n') #3591

	dd = distances(ntkg)
	dd = dd[upper.tri(dd, diag=F)]
	cat(summary(dd[!is.infinite(dd)]),'\n')

	adjm = as.matrix(as_adjacency_matrix(ntkg))

	mv = c()
	ix = 2:7
	for(i in ix){
		wc = cluster_walktrap(ntkg, steps = i, merges =TRUE, modularity = T)
		#sizes(wc)
		mwc =  modularity(wc)
		mv[i-1] = mwc
		print(sprintf("steps %s : %s" , i, round(mwc, 3))) #[1] 0.3408206
	}


	use.ix = ix[which.max(mv)]
	# use.ix = use.ixv[typev]
    # use.ix = 4
	cat('max iter : ', use.ix,'\n')

	wcDEG = cluster_walktrap(ntkg, steps = use.ix, merges =TRUE, modularity = T)

	cls = split(wcDEG$names, wcDEG$membership)
	cls.use = cls[lengths(cls)>cluster.cutoff]
	length(cls.use)
	cls.use = cls.use[order(lengths(cls.use), decreasing=T)]
	names(cls.use) = paste0(cutoff, '_', 1:length(cls.use))

	plot(ix, mv, type='b', pch=19, xlab='iter',ylab='modularity', main=paste0(cutoff,' (',length(V(ntkg)),')'))
	abline(v=use.ix, col='red', lty=2)

	return(cls.use)

})


cl400 = type.cls[[1]]
cl700 =  type.cls[[2]]
lengths(cl400)
lengths(cl700)



#-------
use.cl = cl400

# KEGG
ntk.bgg = c(stringh[,1], stringh[,2])

ntk.pml = lapply(use.cl, function(gg){
    pm = enrichKEGG(gg, 
        organism="hsa", 
        # ont='BP',
        # OrgDb ='org.Mm.eg.db' ,
        universe=as.character(ntk.bgg),
        pAdjustMethod = "BH", pvalueCutoff = 1.1, qvalueCutoff = 1.1)
    pm = pm@result

})

lapply(ntk.pml, head)

# GO
library(org.Hs.eg.db)
ntk.gol = lapply(use.cl, function(gg){
    ego0 <- enrichGO(gene          = gg,
                    universe= as.character(ntk.bgg), #intersect GO-BP gene and all gene
                    OrgDb         = org.Hs.eg.db,
                    # maxGSSize = 600,
                    keyType       = 'ENTREZID',
                    ont           = "BP",
                    pAdjustMethod = "BH",
                    pvalueCutoff  = 1.1,
                    qvalueCutoff  = 1.1)
    ego = ego0@result
})
lapply(ntk.gol, head)




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


#-------- Run


stringh700 = subset(stringh, combined_score >= 700)
stringh400 = stringh



use.ntk = stringh700
use.cl = cl700

# GetDistance.r
# community distance measure, method에서 distance measure 방식 설정
dist.results <- CommDistFunction(
                 use.ntk,
                 use.cl,
                 random= 1000,
                 method= 'closest')


class(dist.results)
dist.results@MoBCresults %>% dplyr::arrange(z_score)

re1= MoBC.genes(dist.results, 'cancer_3','cancer_8') # 76905 : 'Lrg1'
re1$gene_name = fgid1[match(re1$gene, fgid1$EntrezID),'gene_name']
head(re1,20)

plot.Dist(dist.results)


ego0 <- enrichGO(gene          = useg,
				universe= as.character(bgg), #intersect GO-BP gene and all gene
				OrgDb         = org.Mm.eg.db,
				# maxGSSize = 600,
				keyType       = 'ENTREZID',
				ont           = "ALL",
				pAdjustMethod = "BH",
				pvalueCutoff  = 1.1,
				qvalueCutoff  = 1.1)
ego = ego0@result
head(ego)