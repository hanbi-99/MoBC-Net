

## distance function
#' mean of shortest path lengths of all pariwised community genes 
get.shortest.dist = function(distm, x, y){
    mean(distm[x,y])
}

#' average of kernelized distance between communities
get.kernel.dist = function(distm, x, y){
    dist = -(sum(log(rowSums(exp(-(distm[x,y] +1))/length(y)))) + sum(log(colSums(exp(-(distm[x,y] +1))/length(x)))))/(length(x) + length(y))
    return(dist)
}

#' shortest path length between centers of communities
get.centre.dist = function(distm, x, y) {
    c1 = names(which.min(rowSums(distm[x,x])))
    c2 = names(which.min(rowSums(distm[y,y])))
    dist = distm[c1,c2]
    return(dist)
}

#' difference between shortest distance of communities and the average shortest distance of each community
get.separation.dist = function(distm, x, y){
    xy = distm[x,y] %>% mean
    x1 = distm[x,x] %>% mean
    y1 = distm[y,y] %>% mean
    xy-(x1+y1)/2 
}

#' average of sum of min of every community node based shortest path length
get.closest.dist = function(distm, xx1, yy1){
		x1 = distm[yy1,xx1] %>%apply(1,min) %>% sum 
		x2 = distm[yy1,xx1] %>%apply(2,min) %>% sum 
		(x1+x2)/(length(xx1)+length(yy1))   
}
