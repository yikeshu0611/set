#' @title Get Elements only Existed in Dataset a
#' @description Get elements only existed in dataset a.
#' @param a see argument x in \code{\link[set]{toVector}}
#' @param b see argument x in \code{\link[set]{toVector}}
#' @return elements only existed in dataset a
#' @export
#' @name not2
#' @rdname not2
#' @examples
#' A <- c("a","b","c")
#' B <- c("a","b","c","d")
#' B %not% A

"%not%" <- function(a,b){
    numeric_chekc=all(is.numeric(a),is.numeric(b))
    a=toVector(a,numeric_chekc)
    a=unique(a)
    b=toVector(b,numeric_chekc)
    b=unique(b)
    a[!(a %in% b)]
}

#' @rdname not2
#' @export
"%n%" <- `%not%`
#' @rdname not2
#' @export
"%N%" <- `%not%`

#' @rdname not2
#' @export
"/" <- function(a,b) {
    if (all(is.logical(a),is.logical(b))){
        base::`/`(a,b)
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
        a %not% b
    }else{
        stop('set only support character, dataframe, matrix, list or array')
    }
}
