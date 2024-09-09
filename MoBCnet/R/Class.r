
#' Class "MoBCresult"
#' This class represents the distance results of communities
#'
#'
#' @name MoBCresult-class
#' @docType class
#' @slot MoBCresult community distance result
#' @slot filtered.communities a list of community genes
#' @slot graph background network
#' @exportClass MoBCresult




setOldClass("igraph")

setClass("MoBCresult",
        representation = representation(
            MoBCresults = "data.frame",
            filtered.communities = "list",
            graph = "igraph"
            )
         )

setMethod("summary", "MoBCresult",
    function(object){
      head(object@MoBCresults)
    })
