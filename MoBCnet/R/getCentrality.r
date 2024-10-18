



# get.freq <-function(g, snode, enode){
# 	edges = igraph::all_shortest_paths(g, snode, enode)
# 	edges = edges$res %>% lapply(function(xx) setdiff(names(xx), names(xx)[c(1,length(xx))]))
# 	etab = edges %>% unlist
# 	return(edges) #!!
# }



# CalCentrality <- function(g, community1, community2){
# 	scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)
# 	shortestm = igraph::distances(g, community1, community2)
# 	rmin  = apply(shortestm,1,function(xx) colnames(shortestm)[which(xx %in% min(xx))])
# 	r.score = sapply(names(rmin), function(start.node){
# 		end.node = rmin[[start.node]]
# 		etab = get.freq(g, start.node, end.node)
# 		nn = length(etab)
# 		etab = etab %>% unlist %>% table
# 		etab = etab/nn
# 		scorevec[names(etab)] = etab
# 		return(scorevec)
# 	}) %>% apply(1,mean)
# 	cmin  = apply(shortestm,2,function(xx) rownames(shortestm)[which(xx %in% min(xx))])
# 	c.score = sapply(names(cmin), function(start.node){
# 		end.node = cmin[[start.node]]
# 		etab = get.freq(g, start.node, end.node)
# 		nn = length(etab)
# 		etab = etab %>% unlist %>% table
# 		etab = etab/nn
# 		scorevec[names(etab)] = etab
# 		return(scorevec)
# 	}) %>% apply(1,mean)
# 	score.df = data.frame(r.score=r.score, c.score=c.score)
# 	score.df$hub.score = apply(score.df,1,sum)
# 	score.df$tag = 'bridge'
# 	score.df$tag[rownames(score.df) %in% c(community1, community2)] = 'seed'
# 	score.df = score.df %>% dplyr::arrange(-hub.score)
# 	return(score.df)
# }




# #' Calculate centrality between two modules from MoBC result 
# #' 
# #' 
# #' @title Get.Centrality
# #' @param MoBC.result results from CommDistFunction function
# #' @param community1.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input
# #' @param community2.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input
# #' @returns data.frame
# #' @export
# #' @examples
# #' Get.Centrality(MoBC.result, 'community_1','community_2')


# Get.Centrality <- function(MoBC.result, community1.name, community2.name){
# 	if(!is(MoBC.result, 'MoBCresult')){
# 		stop("input should be MoBC class", call. = FALSE)
# 	}
# 	communities = MoBC.result@filtered.communities

# 	if(!all(c(community1.name, community2.name) %in% names(communities))){
# 		stop('community name should be included in name of pre-defined community', call. = FALSE)
# 	}

# 	CalCentrality(MoBC.result@graph, 
# 					community1=MoBC.result@filtered.communities[[community1.name]], 
# 					community2=MoBC.result@filtered.communities[[community2.name]])
# }




# CalConnecting <- function(g, community1, community2){
# 	scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)
# 	shortestm = igraph::distances(g, community1, community2)

# 	rmin  = apply(shortestm,1,function(xx) colnames(shortestm)[which(xx %in% min(xx))])
# 	if(is.matrix(rmin)) rmin = rmin %>% as.data.frame %>% as.list
# 	r.score = sapply(names(rmin), function(start.node){
# 		end.node = rmin[[start.node]]
# 		etab = get.freq(g, start.node, end.node) %>% unlist %>% table
# 		scorevec[names(etab)] = etab
# 		return(scorevec)
# 	}) %>% apply(1,sum)
# 	r.score.norm = r.score/length(community1)

# 	cmin  = apply(shortestm,2,function(xx) rownames(shortestm)[which(xx %in% min(xx))])
# 	if(is.matrix(cmin)) cmin = cmin %>% as.data.frame %>% as.list
# 	c.score = sapply(names(cmin), function(start.node){
# 		end.node = cmin[[start.node]]
# 		etab = get.freq(g, start.node, end.node) %>% unlist %>% table
# 		scorevec[names(etab)] = etab
# 		return(scorevec)
# 	}) %>% apply(1,sum)
# 	c.score.norm = c.score/length(community2)

# 	score.df = data.frame(r.score=r.score, c.score=c.score, r.score.norm = r.score.norm, c.score.norm=c.score.norm)
# 	score.df$freq = apply(score.df[,1:2],1,sum)
# 	score.df$normalized.freq = apply(score.df[,3:4],1,sum)
# 	score.df$degree = igraph::degree(g)[rownames(score.df)]
# 	# score.df$normalized.freq = score.df$freq/score.df$degree
# 	score.df$tag = 'bridge'
# 	score.df$tag[rownames(score.df) %in% c(community1, community2)] = 'seed'
# 	score.df = score.df %>% dplyr::arrange(-normalized.freq)
# 	return(score.df)
# }


# CalConnecting.gene2comm <- function(g, community1, community2){
# 	scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)

# 	li = list(g1=community1, g2=community2)
# 	ixix = lengths(li)==1

# 	rv = sapply(li[!ixix][[1]], function(gix1) get.freq(g, li[ixix][[1]], gix1)) %>% unlist  %>% unlist %>% table
# 	scorevec[names(rv)] = rv   

# 	score.df = data.frame(freq=scorevec)
# 	score.df$degree = igraph::degree(g)[rownames(score.df)]
# 	score.df$normalized.freq = score.df$freq/score.df$degree
# 	score.df$tag = 'bridge'
# 	score.df$tag[rownames(score.df) %in% c(community1, community2)] = 'seed'
# 	score.df = score.df %>% dplyr::arrange(-normalized.freq)
# }

# calflag <-function(MoBC.result, communityn){
# 	if(communityn %in% igraph::V(MoBC.result@graph)$name){
# 		return(1)
# 	}else if(communityn %in% names(MoBC.result@filtered.communities)){
# 		return(2)
# 	}else(0)
# }





# #' Inferring connecting genes between modules (or between a module and a gene)
# #' 
# #' 
# #' @title Get.ConnectingGene
# #' @param MoBC.result results from CommDistFunction function
# #' @param community1.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input or a node name (gene name).
# #' @param community2.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input or a node name (gene name).
# #' @returns data.frame
# #' @export
# #' @examples
# #' Get.ConnectingGene(MoBC.result, 'community_1','community_2')
# #' Get.ConnectingGene(MoBC.result, 'community_1','Tgfb1')



# Get.ConnectingGene <- function(MoBC.result, community1.name, community2.name){
# 	if(!is(MoBC.result, 'MoBCresult')){
# 		stop("input should be MoBC class", call. = FALSE)
# 	}
# 	com1.flag = calflag(MoBC.result, community1.name)
# 	com2.flag = calflag(MoBC.result, community2.name)
# 	if(!(com1.flag & com2.flag)){
# 		stop('community or gene should be included in pre-defined community or graph', call. = FALSE)
# 	}

# 	if(any(com1.flag==1|com2.flag==1)){
# 		print('Inferring connecting genes from a gene not a module')
# 		if(com1.flag==1) use.set1 = community1.name else use.set1 = MoBC.result@filtered.communities[[community1.name]]
# 		if(com2.flag==1) use.set2 = community2.name else use.set2 = MoBC.result@filtered.communities[[community2.name]]
# 		re = CalConnecting.gene2comm(MoBC.result@graph, 
# 						community1=use.set1, 
# 						community2=use.set2)
# 	} else{
# 		print('Inferring connecting genes between modules')
# 		re = CalConnecting(MoBC.result@graph, 
# 						community1= MoBC.result@filtered.communities[[community1.name]], 
# 						community2= MoBC.result@filtered.communities[[community2.name]])

# 	}
# 	return(re)
# }



#--------------------------- new


get.freq <-function(g, snode, enode){
	edges = igraph::all_shortest_paths(g, snode, enode)
	edges = edges$res %>% lapply(function(xx) setdiff(names(xx), names(xx)[c(1,length(xx))]))
	# etab = edges %>% unlist
	return(edges) #!!
}

get.freq.v2 <-function(g, snode, enode){
	edges = igraph::all_shortest_paths(g, snode, enode)
	edges = edges$res %>% lapply(function(xx)  setdiff(names(xx), names(xx)[1]))
	# etab = edges %>% unlist
	return(edges) #!!
}

# cal.MoBCgenes.v1 <- function(g, community1, community2){
# 	scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)
# 	shortestm = igraph::distances(g, community1, community2)
# 	rmin  = apply(shortestm,1,function(xx) colnames(shortestm)[which(xx %in% min(xx))])

#     allg = igraph::V(g)$name %>% as.character()
#     # comm1
#     r.sp.genel = sapply(names(rmin), function(start.node){
# 		end.node = rmin[[start.node]]
# 		etab = get.freq(g, start.node, end.node)
# 		return(etab)
# 	})
    
#     r.pathn = sum(lengths(r.sp.genel))
#     r.tab = unlist(r.sp.genel) %>% table %>% sort

#     # comm2
# 	cmin  = apply(shortestm,2,function(xx) rownames(shortestm)[which(xx %in% min(xx))])
#     c.sp.genel = sapply(names(cmin), function(start.node){
# 		end.node = cmin[[start.node]]
# 		etab = get.freq(g, start.node, end.node)
# 		return(etab)
# 	})
    
#     c.pathn = sum(lengths(c.sp.genel))
#     c.tab = unlist(c.sp.genel) %>% table %>% sort


#     r.num = length(community1)
#     c.num = length(community2)

#     r.score = r.tab/r.pathn*r.num/sum(r.num+c.num)
#     c.score = c.tab/c.pathn*c.num/sum(r.num+c.num)
    
#     r.score = r.score[allg] %>% 'names<-'(allg)
#     c.score = c.score[allg] %>% 'names<-'(allg)
#     r.score[is.na(r.score)] = 0
#     c.score[is.na(c.score)] = 0

# 	score.df = data.frame(gene=allg,community1.score =as.numeric(r.score), community2.score=as.numeric(c.score))
#     score.df$score = score.df$community1.score + score.df$community2.score
# 	score.df$tag = 'bridge'
# 	score.df$tag[score.df$gene %in% c(community1, community2)] = 'community genes'
# 	score.df = score.df %>% dplyr::arrange(-score)

# 	return(score.df)
# }




# cal.MoBCgenes.v2 <- function(g, community1, community2){
# 	scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)
# 	shortestm = igraph::distances(g, community1, community2)
# 	rmin  = apply(shortestm,1,function(xx) colnames(shortestm)[which(xx %in% min(xx))])

#     allg = igraph::V(g)$name %>% as.character()
#     # comm1
#     r.sp.genel = sapply(names(rmin), function(start.node){
# 		end.node = rmin[[start.node]]
# 		etab = get.freq.v2(g, start.node, end.node)
# 		return(etab)
# 	})
    
#     r.pathn = sum(lengths(r.sp.genel))
#     r.tab = unlist(r.sp.genel) %>% table %>% sort

#     # comm2
# 	cmin  = apply(shortestm,2,function(xx) rownames(shortestm)[which(xx %in% min(xx))])
#     c.sp.genel = sapply(names(cmin), function(start.node){
# 		end.node = cmin[[start.node]]
# 		etab = get.freq.v2(g, start.node, end.node)
# 		return(etab)
# 	})
    
#     c.pathn = sum(lengths(c.sp.genel))
#     c.tab = unlist(c.sp.genel) %>% table %>% sort


#     r.num = length(community1)
#     c.num = length(community2)

#     r.score = r.tab/r.pathn*r.num/sum(r.num+c.num)
#     c.score = c.tab/c.pathn*c.num/sum(r.num+c.num)
    
#     r.score = r.score[allg] %>% 'names<-'(allg)
#     c.score = c.score[allg] %>% 'names<-'(allg)
#     r.score[is.na(r.score)] = 0
#     c.score[is.na(c.score)] = 0

# 	score.df = data.frame(gene=allg,community1.score =as.numeric(r.score), community2.score=as.numeric(c.score))
#     score.df$score = score.df$community1.score + score.df$community2.score
# 	score.df$tag = 'bridge'
# 	score.df$tag[score.df$gene %in% c(community1, community2)] = 'community genes'
# 	score.df = score.df %>% dplyr::arrange(-score)

# 	return(score.df)
# }



cal.MoBCgenes <- function(g, community1, community2){
	scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)
	shortestm = igraph::distances(g, community1, community2)
	rmin  = apply(shortestm,1,function(xx) colnames(shortestm)[which(xx %in% min(xx))])

    allg = igraph::V(g)$name %>% as.character()
    # comm1
    r.sp.genel = sapply(names(rmin), function(start.node){
		end.node = rmin[[start.node]]
		etab = get.freq(g, start.node, end.node)
		return(etab)
	})
    
    r.pathn = sum(lengths(r.sp.genel))
    r.tab = unlist(r.sp.genel) %>% table %>% sort

    # comm2
	cmin  = apply(shortestm,2,function(xx) rownames(shortestm)[which(xx %in% min(xx))])
    c.sp.genel = sapply(names(cmin), function(start.node){
		end.node = cmin[[start.node]]
		etab = get.freq(g, start.node, end.node)
		return(etab)
	})
    
    c.pathn = sum(lengths(c.sp.genel))
    c.tab = unlist(c.sp.genel) %>% table %>% sort


    r.num = length(community1)
    c.num = length(community2)

    r.score = r.tab/r.pathn*c.num/sum(r.num+c.num) #!!
    c.score = c.tab/c.pathn*r.num/sum(r.num+c.num) #!!
    
    r.score = r.score[allg] %>% 'names<-'(allg)
    c.score = c.score[allg] %>% 'names<-'(allg)
    r.score[is.na(r.score)] = 0
    c.score[is.na(c.score)] = 0

	score.df = data.frame(gene=allg,community1.score =as.numeric(r.score), community2.score=as.numeric(c.score))
    score.df$score = score.df$community1.score + score.df$community2.score
	score.df$tag = 'bridge'
	score.df$tag[score.df$gene %in% c(community1, community2)] = 'community genes'
	score.df = score.df %>% dplyr::arrange(-score)

	return(score.df)
}



#' Calculate centrality between two modules from MoBC result 
#' 
#' 
#' @title Get.Centrality
#' @param MoBC.result results from CommDistFunction function
#' @param community1.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input
#' @param community2.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input
#' @returns data.frame
#' @export
#' @examples
#' Get.Centrality(MoBC.result, 'community_1','community_2')


MoBC.genes <- function(MoBC.result, community1.name, community2.name){
	if(!is(MoBC.result, 'MoBCresult')){
		stop("input should be MoBC class", call. = FALSE)
	}
	communities = MoBC.result@filtered.communities

	if(!all(c(community1.name, community2.name) %in% names(communities))){
		stop('community name should be included in name of pre-defined community', call. = FALSE)
	}

	cal.MoBCgenes(MoBC.result@graph, 
					community1=MoBC.result@filtered.communities[[community1.name]], 
					community2=MoBC.result@filtered.communities[[community2.name]])
}



#' Calculate centrality between two modules from MoBC result 
#' 
#' 
#' @title plotDist
#' @param MoBC.result results from CommDistFunction function
#' @param community1.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input
#' @param community2.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input
#' @param top 
#' @param community1.color 
#' @param community1.name 
#' @returns plot
#' @export
#' @examples
#' plot.MoBC.genes(MoBC.result, community1.name, community2.name, 
#'                    top=10, community1.color='lightblue1',community2.color='lightpink')



plot.MoBC.genes <- function(MoBC.result, community1.name, community2.name, 
                    top=10, community1.color='lightblue1',community2.color='lightpink'){
	if(!is(MoBC.result, 'MoBCresult')){
		stop("input should be MoBC class", call. = FALSE)
	}
	communities = MoBC.result@filtered.communities

	if(!all(c(community1.name, community2.name) %in% names(communities))){
		stop('community name should be included in name of pre-defined community', call. = FALSE)
	}

	re = cal.MoBCgenes(MoBC.result@graph, 
					community1=MoBC.result@filtered.communities[[community1.name]], 
					community2=MoBC.result@filtered.communities[[community2.name]])
    re = re[1:top,]

    useg = unlist(MoBC.result@filtered.communities[c(community1.name, community2.name)])
    useg = c(useg, re$gene)

    g2 <- igraph::induced_subgraph(MoBC.result@graph, useg)

	layout <- igraph::layout_with_fr(g2)
    
    vcolor = rep('grey', length(igraph::V(g2)$name))
    vcolor[igraph::V(g2)$name %in% MoBC.result@filtered.communities[[community1.name]]] = community1.color
    vcolor[igraph::V(g2)$name %in% MoBC.result@filtered.communities[[community2.name]]] = community2.color

    tcolor = rep('white', length(igraph::V(g2)$name))
    tcolor[igraph::V(g2)$name %in% re$gene] = 'red'

	plre = plot(g2, 
		layout = layout, 
		# mark.groups = split(V(g)$name,clv),
		# vertex.label = fgid1[match(V(g)$name, fgid1$EntrezID),'gene_name'],
		# vertex.label = '', #vns
		vertex.color=vcolor,
		vertex.frame.width=5,
		vertex.frame.color=tcolor,#'white',
		edge.color ='grey', #adjustcolor('black', alpha=0.6),
		# vertex.size= (cln[V(cl.g2)$name]^0.5)*4,
		vertex.size=10,
		# vertex.label.dist=1,
		# vertex.frame.color = 'grey90',
		vertex.label.color='black',
		# vertex.label.font=ifelse(V(g)$name %in% np.gl[[pn]], 2,1),
		vertex.label.size = 0.001
		# edge.width=(igraph::E(g2)$weight)*2
	)
    return(plre)
}



#' Calculate centrality between two modules from MoBC result 
#' 
#' 
#' @title plotDist
#' @param MoBC.result results from CommDistFunction function
#' @param pval cut-off for filtering edges between communities
#' @returns plot
#' @export
#' @examples
#' plotDist(MoBC.result, pval=0.05)




plot.Dist <- function(MoBC.result, pval=0.05){
	if(!is(MoBC.result, 'MoBCresult')){
		stop("input should be MoBC class", call. = FALSE)
	}

	distm = MoBC.result@MoBCresults
	sig.dist = subset(distm, pvalue < pval)[,1:3]
	sig.dist$weight = -sig.dist$z_score
	ntkg = igraph::graph_from_data_frame(sig.dist[,c('community_1','community_2','weight')], directed=FALSE)
	ntkg = igraph::simplify(ntkg, remove.multiple = TRUE, remove.loops = TRUE)

    maxn = max(lengths(MoBC.result@filtered.communities))
    col_fun = circlize::colorRamp2(c(0, round(maxn/4),round(maxn/4)*2,round(maxn/4)*3,maxn), c("grey", "orange", "#FF6566","red","darkred"))
    colv = sapply(igraph::V(ntkg)$name, function(gn){
        xx = col_fun(length(MoBC.result@filtered.communities[[gn]])) %>% c
        return(xx)
    }) %>% 'names<-'(igraph::V(ntkg)$name)
    
    commn = lengths(MoBC.result@filtered.communities)[igraph::V(ntkg)$name]
    sizev = (commn-min(commn))/(max(commn)-min(commn))
    sizev = sizev*20+50

	layout <- igraph::layout_with_fr(ntkg)

	plre = plot(ntkg, 
		layout = layout, 
		# mark.groups = split(V(g)$name,clv),
		# vertex.label = fgid1[match(V(g)$name, fgid1$EntrezID),'gene_name'],
		# vertex.label = '', #vns
		vertex.color=colv,
		vertex.frame.width=0.3,
		vertex.frame.color='white',
		edge.color ="grey",#adjustcolor('black', alpha=0.6),
		# vertex.size= (cln[V(cl.ntkg)$name]^0.5)*4,
		vertex.size=sizev,
		# vertex.label.dist=1,
		# vertex.frame.color = 'grey90',
		vertex.label.color='black',
		# vertex.label.font=ifelse(V(g)$name %in% np.gl[[pn]], 2,1),
		vertex.label.size = 0.1,
		edge.width=(rank(-igraph::E(ntkg)$weight))
	)
    df1 = data.frame(n = lengths(MoBC.result@filtered.communities)[igraph::V(ntkg)$name], col=colv) %>% unique
    df1 = df1[order(df1$n, decreasing=T),]
    legend("bottomright", col=df1[,2], pch=19, legend=df1[,1], title='Module size')
}


