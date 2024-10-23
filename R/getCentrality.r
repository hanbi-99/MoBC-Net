



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
# 	communities = MoBC.result@filtered.modules

# 	if(!all(c(community1.name, community2.name) %in% names(communities))){
# 		stop('community name should be included in name of pre-defined community', call. = FALSE)
# 	}

# 	CalCentrality(MoBC.result@graph, 
# 					community1=MoBC.result@filtered.modules[[community1.name]], 
# 					community2=MoBC.result@filtered.modules[[community2.name]])
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
# 	}else if(communityn %in% names(MoBC.result@filtered.modules)){
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
# 		if(com1.flag==1) use.set1 = community1.name else use.set1 = MoBC.result@filtered.modules[[community1.name]]
# 		if(com2.flag==1) use.set2 = community2.name else use.set2 = MoBC.result@filtered.modules[[community2.name]]
# 		re = CalConnecting.gene2comm(MoBC.result@graph, 
# 						community1=use.set1, 
# 						community2=use.set2)
# 	} else{
# 		print('Inferring connecting genes between modules')
# 		re = CalConnecting(MoBC.result@graph, 
# 						community1= MoBC.result@filtered.modules[[community1.name]], 
# 						community2= MoBC.result@filtered.modules[[community2.name]])

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


cal.MoBCgenes.values <- function(g, community1, community2, allg){
    
	scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)
	shortestm = igraph::distances(g, community1, community2)
	rmin  = apply(shortestm,1,function(xx) colnames(shortestm)[which(xx %in% min(xx))])

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
    scorev = score.df$community1.score + score.df$community2.score
    names(scorev) = allg

	return(scorev)
}



# g = res2@graph
# community1 = res2@filtered.communities[[2]]
# community2 = res2@filtered.communities[[3]]
# random = 1000
# ratio = 0.1



cal.MoBC.random <- function(g, community1, community2,random,ratio,show.binning=FALSE){

    hist.bin = hist.bin.function.v2(g, c(community1, community2),random,ratio, show.binning)
    allg = igraph::V(g)$name %>% as.character()

    cl1g = community1
    cl2g = community2

    start.time <- Sys.time()
    comm.distance.list = sapply(1:random, function(j){
        cat(j,'\n')
        samplingN = sapply(hist.bin, function(xx) sum(xx %in% cl1g)) %>% 'names<-'(names(hist.bin))
        cl1g.random = lapply(names(samplingN), function(xn){
            use.bg = (hist.bin[[xn]])
            # set.seed(m+j)
            sample(use.bg, samplingN[[xn]], replace=FALSE)
        }) %>% unlist %>% unique

        samplingN = sapply(hist.bin, function(xx) sum(xx %in% cl2g)) %>% 'names<-'(names(hist.bin))
        cl2g.random = lapply(names(samplingN), function(xn){
            use.bg = setdiff(hist.bin[[xn]], c(cl1g.random)) 
            # set.seed(n+j)
            sample(use.bg, samplingN[[xn]], replace=FALSE)
        }) %>% unlist %>% unique
        
        comm.distance = cal.MoBCgenes.values(g, cl1g.random, cl2g.random, allg) 
        return(comm.distance)
    })
    end.time <- Sys.time()
    cat(end.time-start.time,'\n')
    # start.time-end.time
    return(comm.distance.list)
}




cal.MoBCgenes <- function(g, community1, community2,random,ratio,cal.p){
    
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
	score.df$node_type = 'link'
	score.df$node_type[score.df$gene %in% c(community1, community2)] = 'community genes'
	score.df = score.df %>% dplyr::arrange(-score)


    if(cal.p){
        random.mat = cal.MoBC.random(g, community1, community2,random,ratio)
        pval = sapply(score.df$gene, function(gn){
            xval = score.df[match(gn, score.df$gene),'score']
            pval = sum(random.mat[gn,]>xval)/random
        })
        score.df$pval = pval
    }
	return(score.df[,c('gene','score','node_type')])
}





# cal.MoBCgenes <- function(g, community1, community2){
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

#     r.score = r.tab/r.pathn*c.num/sum(r.num+c.num) #!!
#     c.score = c.tab/c.pathn*r.num/sum(r.num+c.num) #!!
    
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







cal.FCgene <- function(g, community1, community2){

    gene.ix = igraph::V(g)$name

    # re = igraph::all_shortest_paths(g, community1[1:3],community2[1:4])
    # lapply(re$res, function(xx) names(xx)[1] ) %>% unlist %>% unique
    # lapply(re$res, function(xx) names(xx)[length(xx)] ) %>% unlist %>% unique
    
    re = sapply(community1, function(g1){
            
        edges = igraph::all_shortest_paths(g, g1, community2)
        end.ix = edges$res %>% sapply(function(xx) names(xx)[length(xx)])
        end.ix = split(1:length(end.ix), end.ix)
        resl = lapply(end.ix, function(ixix){
            intg = edges$res[ixix] %>% lapply(function(xx) setdiff(names(xx), names(xx)[c(1,length(xx))]))
            intg.tab = unlist(intg) %>% table
            intg.tab = intg.tab/length(intg)
            intg.tab[gene.ix] %>% 'names<-'(gene.ix)
        })
        res = do.call(rbind, resl) %>% as.matrix
        res[is.na(res)] = 0
        resv = apply(res,2,sum)
    })
    re1 = apply(re,1,sum)
    re1 = re1/length(community1)/length(community2)

	score.df = data.frame(gene=gene.ix,score=re1[gene.ix])
    score.df$node_type = 'link'
	score.df$node_type[score.df$gene %in% c(community1, community2)] = 'community genes'
	score.df = score.df %>% dplyr::arrange(-score)

	return(score.df)
}


#' Calculate centrality between two modules from MoBC result 
#' 
#' 
#' @title Get.Centrality
#' @param MoBC.result results from CommDistFunction function
#' @param module1.name The name of the module for which centrality is being calculated. This should be one of the communities provided as input
#' @param module2.name The name of the module for which centrality is being calculated. This should be one of the communities provided as input
#' @returns data.frame
#' @export
#' @examples
#' Get.Centrality(MoBC.result, 'module_1','module_2')


MoBC.genes <- function(MoBC.result, module1.name, module2.name,random,ratio,cal.p=FALSE){
	if(!is(MoBC.result, 'MoBCresult')){
		stop("input should be MoBC class", call. = FALSE)
	}
	communities = MoBC.result@filtered.modules

	if(!all(c(module1.name, module2.name) %in% names(communities))){
		stop('module name should be included in name of pre-defined module', call. = FALSE)
	}

	cal.MoBCgenes(MoBC.result@graph, 
					community1=MoBC.result@filtered.modules[[module1.name]], 
					community2=MoBC.result@filtered.modules[[module2.name]],
                    random=random, ratio=ratio,cal.p=cal.p)
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


MoBC.genes.p <- function(MoBC.result, community1.name, community2.name,random=1000,ratio=0.1){
	if(!is(MoBC.result, 'MoBCresult')){
		stop("input should be MoBC class", call. = FALSE)
	}
	communities = MoBC.result@filtered.modules

	if(!all(c(community1.name, community2.name) %in% names(communities))){
		stop('community name should be included in name of pre-defined community', call. = FALSE)
	}

	cal.MoBCgenes.p(MoBC.result@graph, 
					Module1=MoBC.result@filtered.modules[[community1.name]], 
					Module2=MoBC.result@filtered.modules[[community2.name]],
                    random=random,
                    ratio = ratio)
}



#' Calculate centrality between two modules from MoBC result 
#' 
#' 
#' @title plotDist
#' @param MoBC.result results from CommDistFunction function
#' @param module1.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input
#' @param module2.name The name of the community for which centrality is being calculated. This should be one of the communities provided as input
#' @param top 
#' @param module1.color 
#' @param module2.color 
#' @returns plot
#' @export
#' @examples
#' plot.MoBC.genes(MoBC.result, module1.name, module2.name, 
#'                    top=10, module1.color='lightblue1',module2.color='lightpink')



plotMoBC_genes <- function(MoBC.result, module1.name, module2.name, 
                    top=10, module1.color='lightblue1',module2.color='lightpink'){
	if(!is(MoBC.result, 'MoBCresult')){
		stop("input should be MoBC class", call. = FALSE)
	}
	communities = MoBC.result@filtered.modules

	if(!all(c(module1.name, module2.name) %in% names(communities))){
		stop('community name should be included in name of pre-defined community', call. = FALSE)
	}

	re = cal.MoBCgenes(MoBC.result@graph, 
					community1=MoBC.result@filtered.modules[[module1.name]], 
					community2=MoBC.result@filtered.modules[[module2.name]],
                    random=1, ratio=1,cal.p=FALSE)
    re = re[1:top,]

    useg = unlist(MoBC.result@filtered.modules[c(module1.name, module2.name)])
    useg = c(useg, re$gene)

    g2 <- igraph::induced_subgraph(MoBC.result@graph, useg)

	layout <- igraph::layout_with_fr(g2)
    
    vcolor = rep('grey', length(igraph::V(g2)$name))
    vcolor[igraph::V(g2)$name %in% MoBC.result@filtered.modules[[module1.name]]] = module1.color
    vcolor[igraph::V(g2)$name %in% MoBC.result@filtered.modules[[module2.name]]] = module2.color

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




plotDist <- function(MoBC.result, pval=0.05){
	if(!is(MoBC.result, 'MoBCresult')){
		stop("input should be MoBC class", call. = FALSE)
	}

	distm = MoBC.result@MoBCresults
	sig.dist = subset(distm, pvalue < pval)[,1:3]
	sig.dist$weight = -sig.dist$z_score
	ntkg = igraph::graph_from_data_frame(sig.dist[,c('Module1','Module2','weight')], directed=FALSE)
	ntkg = igraph::simplify(ntkg, remove.multiple = TRUE, remove.loops = TRUE)

    maxn = max(lengths(MoBC.result@filtered.modules))
    comm.col = colorspace::sequential_hcl(length(MoBC.result@filtered.modules), "Terrain") %>% 'names<-'(names(MoBC.result@filtered.modules))
    
    commn = lengths(MoBC.result@filtered.modules)[igraph::V(ntkg)$name]
    sizev = (commn-min(commn))/(max(commn)-min(commn))
    sizev = sizev*20+20

	layout <- igraph::layout_with_fr(ntkg)

	plre = plot(ntkg, 
		layout = layout, 
		# mark.groups = split(V(g)$name,clv),
		# vertex.label = fgid1[match(V(g)$name, fgid1$EntrezID),'gene_name'],
		# vertex.label = '', #vns
		vertex.color=comm.col[igraph::V(ntkg)$name],
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
		edge.width=(rank(igraph::E(ntkg)$weight))
	)

    legend("bottomright", col=comm.col, pch=19, legend=names(comm.col), title='Module')
}


