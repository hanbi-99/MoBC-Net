




# library(igraph)
# library(dplyr)



#-------------------- function
# check

CheckInput <-function(dist.result){
    check.id = c('results','filtered.community','graph')
    name.flag =  all(names(dist.result) %in% check.id)
    list.flag = is.list(dist.result)
    commg.flag = is.list(dist.result$filtered.community)
    graph.flag = igraph::is_igraph(dist.result$graph)

    if(!(name.flag & list.flag & commg.flag & graph.flag)){
        stop("input should be a result from 'CommDistFunction' function", call. = FALSE)
    }
}

get.freq <-function(g, snode, enode){
    edges = igraph::all_shortest_paths(g, snode, enode)
    edges = edges$res %>% lapply(function(xx) setdiff(names(xx), c(snode, enode)))
    etab = edges %>% unlist
    return(edges[lengths(edges)>0]) #!!
}


CalCentrality <- function(g, community1, community2){
    scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)
    shortestm = igraph::distances(g, community1, community2)
    rmin  = apply(shortestm,1,function(xx) colnames(shortestm)[which(xx %in% min(xx))])
    r.score = sapply(names(rmin), function(start.node){
        end.node = rmin[[start.node]]
        etab = get.freq(g, start.node, end.node)
        nn = length(etab)
        etab = etab %>% unlist %>% table
        etab = etab/nn
        scorevec[names(etab)] = etab
        return(scorevec)
    }) %>% apply(1,mean)
    cmin  = apply(shortestm,2,function(xx) rownames(shortestm)[which(xx %in% min(xx))])
    c.score = sapply(names(cmin), function(start.node){
        end.node = cmin[[start.node]]
        etab = get.freq(g, start.node, end.node)
        nn = length(etab)
        etab = etab %>% unlist %>% table
        etab = etab/nn
        scorevec[names(etab)] = etab
        return(scorevec)
    }) %>% apply(1,mean)
    score.df = data.frame(r.score=r.score, c.score=c.score)
    score.df$hub.score = apply(score.df,1,sum)
    score.df$tag = 'bridge'
    score.df$tag[rownames(score.df) %in% c(community1, community2)] = 'seed'
    score.df = score.df %>% dplyr::arrange(-hub.score)
    return(score.df)
}



Get.Centrality <- function(dist.result, community1.name, community2.name){
    CheckInput(dist.result)
    if(!all(c(community1.name, community2.name) %in% names(dist.result$filtered.community))){
        stop('community name should be included in name of pre-defined community', call. = FALSE)
    }
    CalCentrality(dist.result$graph, 
                    community1=dist.result$filtered.community[[community1.name]], 
                    community2=dist.result$filtered.community[[community2.name]])
}




CalConnecting <- function(g, community1, community2){
    scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)
    shortestm = igraph::distances(g, community1, community2)

    rmin  = apply(shortestm,1,function(xx) colnames(shortestm)[which(xx %in% min(xx))])
    if(is.matrix(rmin)) rmin = rmin %>% as.data.frame %>% as.list
    r.score = sapply(names(rmin), function(start.node){
        end.node = rmin[[start.node]]
        etab = get.freq(g, start.node, end.node) %>% unlist %>% table
        scorevec[names(etab)] = etab
        return(scorevec)
    }) %>% apply(1,sum)
    r.score.norm = r.score/length(community1)

    cmin  = apply(shortestm,2,function(xx) rownames(shortestm)[which(xx %in% min(xx))])
    if(is.matrix(cmin)) cmin = cmin %>% as.data.frame %>% as.list
    c.score = sapply(names(cmin), function(start.node){
        end.node = cmin[[start.node]]
        etab = get.freq(g, start.node, end.node) %>% unlist %>% table
        scorevec[names(etab)] = etab
        return(scorevec)
    }) %>% apply(1,sum)
    c.score.norm = c.score/length(community2)

    score.df = data.frame(r.score=r.score, c.score=c.score, r.score.norm = r.score.norm, c.score.norm=c.score.norm)
    score.df$freq = apply(score.df[,1:2],1,sum)
    score.df$normalized.freq = apply(score.df[,3:4],1,sum)
    score.df$degree = igraph::degree(g)[rownames(score.df)]
    # score.df$normalized.freq = score.df$freq/score.df$degree
    score.df$tag = 'bridge'
    score.df$tag[rownames(score.df) %in% c(community1, community2)] = 'seed'
    score.df = score.df %>% dplyr::arrange(-normalized.freq)
    return(score.df)
}


CalConnecting.gene2comm <- function(g, community1, community2){
    scorevec = rep(0, length(igraph::V(g))) %>% 'names<-'(igraph::V(g)$name)

    li = list(g1=community1, g2=community2)
    ixix = lengths(li)==1

    rv = sapply(li[!ixix][[1]], function(gix1) get.freq(g, li[ixix][[1]], gix1)) %>% unlist  %>% unlist %>% table
    scorevec[names(rv)] = rv   

    score.df = data.frame(freq=scorevec)
    score.df$degree = igraph::degree(g)[rownames(score.df)]
    score.df$normalized.freq = score.df$freq/score.df$degree
    score.df$tag = 'bridge'
    score.df$tag[rownames(score.df) %in% c(community1, community2)] = 'seed'
    score.df = score.df %>% dplyr::arrange(-normalized.freq)
}

calflag <-function(dist.result, communityn){
    if(communityn %in% igraph::V(dist.result$graph)$name){
        return(1)
    }else if(communityn %in% names(dist.result$filtered.community)){
        return(2)
    }else(0)
}


Get.ConnectingGene <- function(dist.result, community1.name, community2.name){
    # gene일 가능성 확인
    CheckInput(dist.result)
    com1.flag = calflag(dist.result, community1.name)
    com2.flag = calflag(dist.result, community2.name)
    if(!(com1.flag & com2.flag)){
        stop('community or gene should be included in pre-defined community or graph', call. = FALSE)
    }

    if(any(com1.flag==1|com2.flag==1)){
        print('Inferring connecting genes from a gene not a module')
        if(com1.flag==1) use.set1 = community1.name else use.set1 = dist.result$filtered.community[[community1.name]]
        if(com2.flag==1) use.set2 = community2.name else use.set2 = dist.result$filtered.community[[community2.name]]
        re = CalConnecting.gene2comm(dist.result$graph, 
                        community1=use.set1, 
                        community2=use.set2)
    } else{
        print('Inferring connecting genes between modules')
        re = CalConnecting(dist.result$graph, 
                        community1= dist.result$filtered.community[[community1.name]], 
                        community2= dist.result$filtered.community[[community2.name]])

    }
    return(re)
}



