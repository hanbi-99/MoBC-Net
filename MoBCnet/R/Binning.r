#' Binning function
#'
#' through the histogram of degree, divide the hist into groups with similar degree
#' Binning function is used to make the similar degree distribution pool for making degree-preserved random networks
#' 
#' @param graph input
#' @param community.list input
#' @return similar degree distributed gene list
#' @export 
#'  

hist.bin.function <- function(g.res,community.gl) {
    
    d.all = igraph::degree(g.res)
    maxn = community.gl %>% lengths %>% max
    maxn = max(c(maxn, 100))
    hist.all <- hist(d.all, breaks= 300, plot= FALSE)
    res <- lapply(1:c(sum(hist.all$counts>= maxn)+1), function(x) {
        if (hist.all$counts[[x]] >= maxn) {
            d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[x+1]]
        } else if (hist.all$counts[[x]] < maxn) {
            d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[length(hist.all$counts)+1]]
        }
        })
    hist.bin <- res[1:c(sum(hist.all$counts>= maxn)+1)]
    names(hist.bin) <- 1:c(sum(hist.all$counts>= maxn)+1)
    hist.bin = lapply(hist.bin, names)
    return(hist.bin)
    }
