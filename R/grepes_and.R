#' @title Get Intersection Set for Two Sets
#' @description Get intersection set for two sets, which can be numbers, characters, vectors even dataframe, matrix or list.
#' @param a see argument x in \code{\link[set]{toVector}}
#' @param b see argument x in \code{\link[set]{toVector}}
#' @return intersection set
#' @name and2
#' @rdname and2
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' A %and% B
#' A %and% B %and% C

"%and%" <- function(a,b){
    a=toVector(a)
    a=unique(a)
    b=toVector(b)
    b=unique(b)
    a[a %in% b]
}

#' @rdname and2
#' @export
"%a%" <- `%and%`
#' @rdname and2
#' @export
"%A%" <- `%and%`

#' @rdname and2
#' @export
"&" <- function(a,b) {
    if (all(is.logical(a),is.logical(b))){
        base::`&`(a,b)
    }else if (all(any(is.numeric(a),is.logical(a)),
                  any(is.numeric(b),is.logical(b)),
                  length(a)==1,
                  length(b)==1)){
        base::`&`(a,b)
    }else if(any(is.character(a),is.character(b),
                 is.data.frame(a),is.data.frame(b),
                 is.matrix(a),is.matrix(b),
                 is.list(a),is.list(b),
                 is.array(a),is.array(b))){
        a %and% b
    }else{
        stop('set only support character, dataframe, matrix, list or array')
    }
}
