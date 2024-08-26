
# https://github.com/YuLab-SMU/DOSE/blob/devel/R/00-AllClasses.R


##' Class "compareClusterResult"
##' This class represents the comparison result of gene clusters by GO
##' categories at specific level or GO enrichment analysis.
##'
##'
##' @name compareClusterResult-class
##' @aliases compareClusterResult-class show,compareClusterResult-method
##'   summary,compareClusterResult-method plot,compareClusterResult-method
##' @docType class
##' @slot compareClusterResult cluster comparing result
##' @slot geneClusters a list of genes
##' @slot fun one of groupGO, enrichGO and enrichKEGG
##' @slot gene2Symbol gene ID to Symbol
##' @slot keytype Gene ID type
##' @slot readable logical flag of gene ID in symbol or not.
##' @slot .call function call
##' @slot termsim Similarity between term
##' @slot method method of calculating the similarity between nodes
##' @slot dr dimension reduction result
##' @exportClass compareClusterResult
##' @author Guangchuang Yu \url{https://yulab-smu.top}
##' @exportClass compareClusterResult
##' @seealso 
##'   \code{\linkS4class{enrichResult}}
##' @keywords classes
setClass("compareClusterResult",
         representation = representation(
             compareClusterResult = "data.frame",
             geneClusters = "list",
             fun = "character",
             gene2Symbol    = "character",
             keytype        = "character",
             readable       = "logical",
             .call          = "call",
             termsim        = "matrix",
             method         = "character",
             dr             = "list"
         )
         )





x <- new("enrichResult",
            result         = Over,
            pvalueCutoff   = pvalueCutoff,
            pAdjustMethod  = pAdjustMethod,
            qvalueCutoff   = qvalueCutoff,
            gene           = as.character(gene),
            universe       = extID,
            geneSets       = geneSets,
            organism       = "UNKNOWN",
            keytype        = "UNKNOWN",
            ontology       = "UNKNOWN",
            readable       = FALSE
            )



#- example

setClass("MoBCresult",
        representation = representation(
            MoBCresult = "data.frame",
            filtered.communities = "list",
            graph = "igraph"
            )
         )



# tags
# https://littleheroncodes.github.io/Adventures-in-R-package/
# https://uoftcoders.github.io/studyGroup/lessons/r/packages/lesson/