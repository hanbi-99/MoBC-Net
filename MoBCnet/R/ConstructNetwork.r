#'preprocessedNetwork
#' 
#' Background network constitution
#' compose largest component network from the dataframe of network
#' @param dataframe input
#' @return largest component netowrk
#' @export 
#' 
preprocessedNetwork <- function(dataframe) {
     if (is.null(dataframe) | ncol(dataframe) < 2) {
        stop('Network dataframe file is missing.', call. = FALSE)
     } else {
        gnet = igraph::graph_from_data_frame(dataframe, directed = F, vertices = NULL)
        gnet = igraph::simplify(gnet, remove.multiple = TRUE, remove.loops = TRUE)
        g.largest.res <- igraph::largest_component(gnet, mode= c('strong')) 

        print(paste0('Largest component network nodes ', length(igraph::V(g.largest.res)), ' (', 'original network nodes - ', length(igraph::V(gnet)), ')', ' are composed in the network'))
        return(g.largest.res)
     }
} 

#' genelist.overlap
#' 
#' get rid of overlapped genes from the community gene list
#' 
#' @param genelist list of community genes
#' @param overlap_filtering TRUE or FALSE
#' @return genelist
#' @export 
#' 

genelist.overlap <- function(genelist, overlap_filtering=TRUE) {
    if (overlap_filtering == TRUE){
       its.list <- c()
       res <- lapply(1:(length(genelist)-1), function(x){
            lapply((x+1):length(genelist), function(y){
                its <- intersect(genelist[[x]], genelist[[y]])
                its.list <- c(its.list, its)
            })
        })
        if (!length(unlist(res))==0) {
            cat('Overlapped', length(unlist(res)),'genes are removed from the genelist','\n')
        } else {
            cat('There is no overlapped genes in the genelist', '\n')
        }
        results <- lapply(genelist, function(x){
            x = setdiff(x, unlist(res))
        })
        return(results)
    } else {
        cat('Overlapped genes are not considered','\n')
        return(genelist)
    }
}

#' CommunityGenelist
#' 
#' filtering the community genelist from the graph of network
#' 
#' @param genelist list of community genes
#' @param graph network graph
#' @param overlap_filtering TRUE or FALSE
#' @export 
#' 

CommunityGenelist <- function(genelist, g, overlap_filtering = TRUE) {
    if (is.null(genelist) | length(genelist) <= 1) {
        stop('Community genelist is missing.', call. = FALSE)
    } else if (!is.list(genelist)) {
        stop('Input must be list.', call. = FALSE)
    } else {
        genelist = genelist.overlap(genelist, overlap_filtering = overlap_filtering)
        glist.len <- length(unique(unlist(genelist))) 
        intersect.len <- length(intersect(unlist(genelist), igraph::V(g)$name))
        glist.final <- lapply(genelist, function(xx) intersect(xx, igraph::V(g)$name))
        comm.len <- sum(lengths(glist.final) > 0)
        if (intersect.len == 0 | comm.len == 0) {
            stop('Network does not include the community genelist. Check the genelist and the network.', call.= FALSE)
        }       
        cat(paste0('Community ', intersect.len,' nodes ','(original nodes - ',glist.len, ')', ' are included in the network and the number of community (the number of nodes > 0) is ', comm.len, '\n'))
        return(glist.final)
    }
}

