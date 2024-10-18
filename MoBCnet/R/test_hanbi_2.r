

library(dplyr)
library(clusterProfiler)



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




#--------------- TEST
library(igraph)

fn = '/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/문서 - AI & BI 연구팀 - Community distance/Community distance/Validation data results/breast.deg.results.700.sd.4.Rdata'
load(fn)
# [1] "breast.deg.results.700.sd.4" "CONFIG"                     
# [3] "dateTag"                     "fn"   

res = breast.deg.results.700.sd.4
class(res)
re = res@MoBCresults %>% dplyr::arrange(z_score)

res@MoBCresults %>% dplyr::arrange(z_score)

plot.Dist(res)
# 2,3,5

# new
# 여기에 comm.breast.RData (comm.breast), human.ppi.network.csv
load('/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/문서 - AI & BI 연구팀 - Community distance/Community distance/community_distance_file/network/binning/comm.breast.RData') #comm.breast
orig.ppi = read.csv('/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/문서 - AI & BI 연구팀 - Community distance/Community distance/community_distance_file/network/binning/human.ppi.network.csv', row.names=1)

res2 = CommDistFunction(orig.ppi,comm.breast)


res@MoBCresults %>% dplyr::arrange(z_score)
res2@MoBCresults %>% dplyr::arrange(z_score)

res2@MoBCresults %>% dplyr::arrange(distance_score)


network = orig.cls
community.genelist = comm.breast

#------
ntkg = res@graph
class(ntkg)

cls = res@filtered.communities
use.cls = cls[c(2,3,5)]

top = 5


# comm1='2'
# comm2='3'

# re1 = cal.MoBCgenes(res@graph, 
#                 community1=res@filtered.communities[[comm1]], 
#                 community2=res@filtered.communities[[comm2]])

# re2 = cal.MoBCgenes.v2(res@graph, 
#                 community1=res@filtered.communities[[comm1]], 
#                 community2=res@filtered.communities[[comm2]])

# head(re1)
# head(re2)


sig.re = re[1:2,1:2]
linkg = lapply(1:2, function(ix){

    comm1 = sig.re[ix,1]
    comm2 = sig.re[ix,2]

	link.re = cal.MoBCgenes.v2(res@graph, 
					community1=res@filtered.communities[[comm1]], 
					community2=res@filtered.communities[[comm2]])
    link.re = link.re[1:top,]
}) %>% bind_rows %>% pull(gene) %>% unique


# subgraph
useg = unlist(use.cls)
useg = c(useg, linkg)

useg = cls[c(1,2,3,5)] %>% unlist

g2 = igraph::induced_subgraph(ntkg, useg)


set.seed(2)
layout <- igraph::layout_with_fr(g2)

vcolor = rep('grey', length(igraph::V(g2)$name))
vcolor[igraph::V(g2)$name %in% cls[['2']]] = 'orange'
vcolor[igraph::V(g2)$name %in% cls[['3']]] = 'dodgerblue'
vcolor[igraph::V(g2)$name %in% cls[['5']]] = 'forestgreen'

tcolor = rep(NA, length(igraph::V(g2)$name))
tcolor[igraph::V(g2)$name %in% linkg] = 'white'

tfv = igraph::V(g2)$name %in% linkg
names(tfv) = V(g2)$name

labels = V(g2)$name
labels[!tfv] = ''


plt.dir = '/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/문서 - AI & BI 연구팀 - Community distance/Community distance/Validation data results/plot'
png(
	file = paste0(plt.dir,"/comm_2_3_5_linker5_v2.png"),
	# file=paste0(DIR_Presult,"/Cachexia/plt_CAD_dexX/network/method1_clusters_",focus.type,".png"), #kpath c2_cp
	width = 7, height =7, 
    # type='cairo', 
    units = "in", res = 300,  bg='white')

plot(g2, 
    layout = layout, 
    # mark.groups = split(V(g)$name,clv),
    # vertex.label = fgid1[match(V(g)$name, fgid1$EntrezID),'gene_name'],
    vertex.label = labels,
    vertex.color=vcolor,
    vertex.frame.width=1,
    vertex.frame.color=tcolor,#'white',
    edge.color ='grey', #adjustcolor('black', alpha=0.6),
    # vertex.size= (cln[V(cl.g2)$name]^0.5)*4,
    vertex.size=ifelse(tfv, 7,3),
    # vertex.label.dist=1,
    vertex.label.family='Arial',
    # vertex.frame.color = 'grey90',
    vertex.label.color='black',
    # vertex.label.font=ifelse(V(g)$name %in% np.gl[[pn]], 2,1),
    vertex.label.size = 0.0001,
    edge.width=0.5
)

dev.off()

#---------- all

# subgraph
useg = unlist(cls)

g2 = igraph::induced_subgraph(ntkg, useg)


set.seed(2)
layout <- igraph::layout_with_fr(g2)

vcolor = rep('grey', length(igraph::V(g2)$name))
vcolor[igraph::V(g2)$name %in% cls[['1']]] = 'black'
vcolor[igraph::V(g2)$name %in% cls[['2']]] = 'orange'
vcolor[igraph::V(g2)$name %in% cls[['3']]] = 'dodgerblue'
vcolor[igraph::V(g2)$name %in% cls[['4']]] = 'grey'
vcolor[igraph::V(g2)$name %in% cls[['5']]] = 'forestgreen'
vcolor[igraph::V(g2)$name %in% cls[['6']]] = 'lightpink'
vcolor[igraph::V(g2)$name %in% cls[['7']]] = 'lightgreen'
vcolor[igraph::V(g2)$name %in% cls[['8']]] = 'salmon'

tcolor = rep(NA, length(igraph::V(g2)$name))
tcolor[igraph::V(g2)$name %in% linkg] = 'white'

# tfv = igraph::V(g2)$name %in% linkg
# names(tfv) = V(g2)$name

# labels = V(g2)$name
# labels[!tfv] = ''

plt.dir = '/Users/hanbilee/Library/CloudStorage/OneDrive-GCCORP/문서 - AI & BI 연구팀 - Community distance/Community distance/Validation data results/plot'
png(
	file = paste0(plt.dir,"/comm_all.png"),
	# file=paste0(DIR_Presult,"/Cachexia/plt_CAD_dexX/network/method1_clusters_",focus.type,".png"), #kpath c2_cp
	width = 7, height =7, 
    # type='cairo', 
    units = "in", res = 300,  bg='white')

plot(g2, 
    layout = layout, 
    # mark.groups = split(V(g)$name,clv),
    # vertex.label = fgid1[match(V(g)$name, fgid1$EntrezID),'gene_name'],
    vertex.label = '',#labels,
    vertex.color=vcolor,
    vertex.frame.width=1,
    vertex.frame.color=NA,#tcolor,#'white',
    edge.color ='grey', #adjustcolor('black', alpha=0.6),
    # vertex.size= (cln[V(cl.g2)$name]^0.5)*4,
    vertex.size=3, #ifelse(tfv, 7,3),
    # vertex.label.dist=1,
    vertex.label.family='Arial',
    # vertex.frame.color = 'grey90',
    vertex.label.color='black',
    # vertex.label.font=ifelse(V(g)$name %in% np.gl[[pn]], 2,1),
    vertex.label.size = 0.0001,
    edge.width=0.5
)

dev.off()
















# 
library(circlize)
col_fun = colorRamp2(c(-1, 0, 1), c("dodgerblue", "white", "gold"))
colv = col_fun(tpm.cdf[match(V(cl.ntkg)$name, tpm.cdf$EntrezID),'Lrg1'])

labelv = fgid1[match(V(cl.ntkg)$name,fgid1$EntrezID),'gene_name']
labelv[which(!V(cl.ntkg)$name %in% cp.degl[['cancer']])] = ''

lcolv = ifelse(V(cl.ntkg)$name %in% cp.degl.up[['cancer']],'red',ifelse(V(cl.ntkg)$name %in% cp.degl.dn[['cancer']],'darkblue','black' ))
lcolv0 = ifelse(V(cl.ntkg)$name %in% cp.degl.up[['cancer']],'red',ifelse(V(cl.ntkg)$name %in% cp.degl.dn[['cancer']],'darkblue','white' ))
lcolv1 = ifelse(V(cl.ntkg)$name %in% cp.degl.up[['cancer']],'salmon',ifelse(V(cl.ntkg)$name %in% cp.degl.dn[['cancer']],'lightblue','wheat1' ))


pathg = strsplit(pmdf1[,'geneID'],'/') %>% 'names<-'(gsub(" - .*","",pmdf1[,'Description']))
lapply(pathg, function(xx) subset(fgid1, EntrezID %in% xx))
pathg[c('Proteasome','Ribosome','TNF signaling pathway')] %>% unlist %>% table %>% sort
pathg[c('Proteasome','Ribosome','TNF signaling pathway')] %>% unlist %>% table %>% sort

pcolv = rep('grey90',length(V(cl.ntkg)$name))
pcolv[which(V(cl.ntkg)$name %in% pathg[['Proteasome']])] = 'skyblue1'
pcolv[which(V(cl.ntkg)$name %in% pathg[['Ribosome']])] = 'lightpink1'
pcolv[which(V(cl.ntkg)$name %in% pathg[['TNF signaling pathway']])] = 'lightgoldenrod1'
# pcolv[which(V(cl.ntkg)$name %in% pathg[['Pathways of neurodegeneration']])] = 'lightgoldenrod1'


png(
	file = paste0(DIR_Presult,"/Cachexia/network/walktrap/",filen,"/Lrg1figure/ntk_0.7_400_path0.1_pathtag.png"),
	# file=paste0(DIR_Presult,"/Cachexia/plt_CAD_dexX/network/method1_clusters_",focus.type,".png"), #kpath c2_cp
	width = 10, height =10, 
    # type='cairo', 
    units = "in", res = 300,  bg='white')

windowsFonts(Times=windowsFont("Arial"))

set.seed(91) #82
l <- layout_with_fr(cl.ntkg) #layout_with_kk

# Other graph layouts: add_layout_(), component_wise(), layout_as_bipartite(), layout_as_star(), 
# layout_as_tree(), layout_in_circle(), layout_nicely(), layout_on_grid(), layout_on_sphere(), 
# layout_randomly(), layout_with_dh(), layout_with_fr(), layout_with_gem(), layout_with_graphopt(), 
# layout_with_kk(), layout_with_lgl(), layout_with_mds(), layout_with_sugiyama(), merge_coords(), norm_coords(), normalize()

par(cex=1)
plot(
    cl.ntkg,
    # main=sid,
    vertex.frame.width=3,
    vertex.color = pcolv, #lcolv1,
    vertex.label=fgid1[match(V(cl.ntkg)$name,fgid1$EntrezID),'gene_name'],
    vertex.label.color="black",#lcolv,#ifelse(labelv=='','black','darkred'),
    vertex.label.size=1,
    edge.color =adjustcolor('black', alpha=0.6),
    # vertex.size= (cln[V(cl.ntkg)$name]^0.25)*12,
	vertex.size=13,
    vertex.label.family='Arial',
    vertex.frame.color=lcolv0,
    # mark.border=mkcol, mark.col=adjustcolor(mkcol, alpha=0.4),
    layout=l, # layout.circle,
    edge.label = E(cl.ntkg)$weight,
    edge.label.color = 'black',
    # vertex.label.angle=90,
    edge.width=E(cl.ntkg)$weight^(1/2)
    ) 


# spl.nv = seq(-1,1,length.out=100)

# image.plot(legend.only=T, 
#     legend.lab='',
#     legend.cex=1,
#     zlim=range(c(-1, 0, 1)), 
#     col=col_fun(spl.nv),
#     legend.mar = ifelse(TRUE, 10, 1),
#     legend.shrink = 0.2,
#     legend.width = 2, legend.height=0.5
#     )

dev.off()


