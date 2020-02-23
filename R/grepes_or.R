#' @title Get Union Set for Two Sets
#' @description Get union set for two sets.
#' @param a see argument x in \code{\link[set]{toVector}}
#' @param b see argument x in \code{\link[set]{toVector}}
#' @return union set
#' @name or2
#' @export
#'
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' C <- c("a","e","h")
#' A %or% B
#' A %and% B %or% C

"%or%" <- function(a,b){
    numeric_chekc=all(is.numeric(a),is.numeric(b))
    a=toVector(a,numeric_chekc)
    a=unique(a)
    b=toVector(b,numeric_chekc)
    b=unique(b)
    unique(c(a,b))
}

#' @rdname or2
#' @export
"%r%" <- `%or%`
#' @rdname or2
#' @export
"%R%" <- `%or%`

#' @rdname or2
#' @export
"|" <- function(a,b) {
    if (all(is.logical(a),is.logical(b))){
        base::`|`(a,b)
    }else if (all(any(is.numeric(a),is.logical(a)),
                  any(is.numeric(b),is.logical(b)),
                  length(a)==1,
                  length(b)==1)){
        base::`&`(a,b)
    }else if(any(is.character(a),is.character(b),
                 is.data.frame(a),is.data.frame(b),
                 is.matrix(a),is.matrix(b),
                 is.list(a),is.list(b),
                 is.array(a),is.array(b),
                 all(is.numeric(a),length(a)>1),
                 all(is.numeric(b),length(b)>1))){
        a %or% b
    }else{
        stop('set only support character, dataframe, matrix, list or array')
    }
}
