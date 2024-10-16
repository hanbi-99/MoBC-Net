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

# hist.bin.function <- function(g.res,community.gl) {
    
#     d.all = igraph::degree(g.res)
#     maxn = community.gl %>% lengths %>% max
#     maxn = max(c(maxn, 100))
#     hist.all <- hist(d.all, breaks= 300, plot= FALSE)
#     res <- lapply(1:c(sum(hist.all$counts>= maxn)+1), function(x) {
#         if (hist.all$counts[[x]] >= maxn) {
#             d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[x+1]]
#         } else if (hist.all$counts[[x]] < maxn) {
#             d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[length(hist.all$counts)+1]]
#         }
#         })
#     hist.bin <- res[1:c(sum(hist.all$counts>= maxn)+1)]
#     names(hist.bin) <- 1:c(sum(hist.all$counts>= maxn)+1)
#     hist.bin = lapply(hist.bin, names)
#     return(hist.bin)
#     }


hist.bin.function <- function(g.res,community.gl) {
    
    d.all = igraph::degree(g.res)
    maxn = community.gl %>% lengths %>% max
    maxn = max(c(maxn, 100))
    hist.all <- hist(d.all, breaks= 300, plot= FALSE)
	hist.all$counts = hist.all$counts[order(hist.all$counts, decreasing=T)]
    # hist.all$breaks = hist.all$breaks[order(hist.all$counts, decreasing=T)]
	res.list <- list()
    res <- lapply(1:(sum(hist.all$counts > maxn)+1), function(x)  {
		if ( x== 1) {
			if (hist.all$breaks[x] == 0) {
                rl0 = d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[x+1]]
			    res.list[[x]] <- rl0
			} else {
                rl0 <- d.all[d.all <= hist.all$breaks[x]]
				res.list[[x]] <- rl0
			}
        } else if (hist.all$counts[[x]] >= maxn) {
            # x = 2
			rl = d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[x+1]]
			res.list[[x]] <- rl
        } else if (hist.all$counts[[x]] < maxn) {
			# x = 12
			while (!(sum(hist.all$counts[x:length(hist.all$counts)]) < maxn)) {
				result = for (n in 1:(length(hist.all$counts)-x))  {
				len = d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[x+n]] %>% length
				if (len > maxn*0.8 & len < maxn*2.3) {
					rl2 = d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[x+n]]
					res.list[[x]] <- rl2
					x = x + n
					# print(x)
					# break 
				} 
			  } 
			}
			while (hist.all$counts[x:length(hist.all$counts)] %>% sum < maxn) {
					rl3 = d.all[hist.all$breaks[x] < d.all & d.all <= hist.all$breaks[length(hist.all$breaks)]+20]
					res.list[[x]] <- rl3
					# print(x)
					break  
			  }    
		  }
		return(res.list)
        })
    res = lapply(res, compact)
	res2 = list()
	for (x in 1:sum(lengths(res))) { 
		if (x < length(res)) {
			res2[[x]] = res[[x]][[1]]
		}
		else if ( x >= length(res) ) {
			for ( y in 1:(x-length(res)+1) ) {
              res2[[x]] = res[[length(res)]][[y]]
			}
		}
	}
	names(res2) <- 1: length(res2)
    hist.bin = lapply(res2, names)
	if (lengths(hist.bin[length(hist.bin)][1])< maxn*0.3) {
		hist.bin[[length(hist.bin)-1]] = c(hist.bin[[length(hist.bin)]], hist.bin[[length(hist.bin)-1]])
		hist.bin = hist.bin[1:length(hist.bin)-1]
	}
	if (length(unlist(hist.bin)) == length(names(d.all))) {
		print(paste0("Hist bin - ", length(hist.bin), "  has been made"))
	    return(hist.bin)
	} else {
		print("Hist bin has not been made")
	}
    }
