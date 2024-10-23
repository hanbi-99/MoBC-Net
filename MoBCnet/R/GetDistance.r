#' module distance function
#' 
#' Calculate the distance and z_score of the communities
#' use.f can be choosed in the DistFunction.r
#' ex) get.shortest.dist, get.kernel.dist, get.centre.dist, get.separation.dist, get.closest.dist
#' As a default, get.closest.dist function is used to measure the distance between communities
#' User also can make the dist function and use it for calculating module distance
#' This function is made to know the z-score of a measured distance from distances of degree-preserved random networks
#' 
#' @param network network dataframe
#' @param module.genelist module genes list
#' @param random the number of random network formation and distance calculation
#' @param overlap_filtering overlap genes filtering (TRUE/FALSE)
#' @param method distance measuring method
#' @return distance results, module.genelist, network
#' @export 
#' 



CommDistFunction <- function(network,
							 module.genelist,
							 random = 1000,
							 overlap_filtering = TRUE,
                             ratio = 0.1,
                             show.binning=FALSE,
                             method = c('closest', 'shortest', 'kernel', 'centre', 'separation')) {
    # cat(method,'\n')
    if (is.character(method)){
        dist.function <- match.arg(method)
        # cat(dist.function,'\n')
        dist.function <- switch(dist.function,
            closest = get.closest.dist,
            shortest = get.shortest.dist,
            kernel = get.kernel.dist,
            centre = get.centre.dist,
            separation = get.separation.dist)
    } else if(is.function(method)){
        dist.function = method
    } else {
        stop('Method function is wrong. Check the method function', call.=FALSE)
    }
	if (is.null(names(module.genelist)) | any(is.na(names(module.genelist)))) {
		stop('Please assign names to the module list.', call.=FALSE)
	}
	g.res  <- preprocessedNetwork(network)
    comm.genelist <- CommunityGenelist(module.genelist, g.res, overlap_filtering = overlap_filtering)
	distm <- igraph::distances(g.res, igraph::V(g.res), igraph::V(g.res))
	# hist.bin <- hist.bin.function(g.res, comm.genelist)  
    hist.bin = hist.bin.function.v2(g.res, comm.genelist,random,ratio,show.binning)

    cat(length(hist.bin), 'hist bins have been made with', length(unlist(hist.bin)), 'nodes', '\n')
	if (!(dim(distm)[1] == length(igraph::V(g.res)) & dim(distm)[2] == length(igraph::V(g.res)))) {
		stop('Distance matrix is wrongly constructed. Check the network dataframe', call.=FALSE)
	}
    cat('Dist matrix :', dim(distm)[1],'X',dim(distm)[2], 'is made','\n')
    cat('Random distance measuring is going to be processed by', random, 'times','\n')

	results = lapply(1:(length(comm.genelist)-1), function(m){
		 dist.rel =lapply((m+1):length(comm.genelist), function(n){ #dist.rel =
			cat('Distance measuring :','module',names(comm.genelist)[m],' - ','module', names(comm.genelist)[n],'\n')

            cl1g = comm.genelist[[m]]
			cl2g = comm.genelist[[n]]
			comm.distance.list = sapply(1:random, function(j){
				
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
				
				comm.distance = dist.function(distm, cl1g.random, cl2g.random) 
				return(comm.distance)
			}) %>% unlist

			xval = dist.function(distm, cl1g, cl2g) 
			zval = (xval-mean(comm.distance.list))/sd(comm.distance.list)
			pval = sum(comm.distance.list<xval)/random

			df1 = data.frame(Module1=names(comm.genelist)[m], Module2=names(comm.genelist)[n], z_score=zval, distance_score=xval, pvalue=pval)
			# cat('-end\n')
			return(df1)
			})
		dist.rel = do.call(rbind, dist.rel)
		}) #%>% dplyr::bind_rows %>% as.data.frame
	results = do.call(rbind, results)
	x <- new("MoBCresult",
        MoBCresults = results,
        filtered.modules = comm.genelist,
        graph = g.res)
	return(x)
	}

