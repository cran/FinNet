#' Function to return an object after assigning new names
#'
#' Combines \code{magrittr::set_colnames}, \code{magrittr::set_rownames}, \code{magrittr::set_names}
#'
#' @param x Object on which to operate
#' @param names New names
#' @param where What to change:
#' - \code{col} Column names
#' - \code{row} Row names
#' - \code{names} Names attribute
#'
#' @return The original object, with new names
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

.set_names <- function(x, names, where = c('col', 'row', 'attr')){
  `f<-` <- ifelse(where == 'col', `colnames<-`,
                  ifelse(where == 'row', `rownames<-`, `names<-`))
  f(x) <- names
  x
}

#' Function to check whether an object is \code{NA} or \code{NULL}
#'
#' Combines \code{base::is.na}, \code{base::is.null}
#'
#' @param x Object on which to operate
#' @param negating Whether to return the negation of the result
#'
#' @return Logical, depending on \code{negating}:
#' \itemize{
#'  \item if \code{negating} is \code{FALSE}, it returns \code{TRUE} if \code{x} is \code{NA} or \code{NULL};
#'  \item if \code{negating} is \code{TRUE}, it returns \code{TRUE} if \code{x} is \strong{neither} \code{NA} \strong{nor} \code{NULL}.
#' }
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

is.null.na <- function(x, negating = FALSE){
  res <- any(is.null(x), is.na(x))
  ifelse(negating, !res, res)
}

#' Function to check whether an object is neither \code{NA} nor \code{NULL}
#'
#' Combines \code{base::is.na()|> magrittr::not()} and \code{base::is.null|> magrittr::not()}
#'
#' @param x Object on which to operate
#'
#' @return Logical: \code{TRUE} if \code{x} is neither \code{NA} or \code{NULL}, \code{FALSE} otherwise.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

is.neither.null.nor.na <- function(x){
  !(is.null(x)|is.na(x))
}

#' Function to extract the symbols of the objects passed as \code{...}
#'
#' This function will not work when called inside the function to which the objects were explicitly passsed as \code{...}.
#'
#' @references https://stackoverflow.com/a/11892680
#'
#' @param ... Objects on which to operate
#'
#' @return A vector of strings matching the provided objects' symbols.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal
#'
naming <- function(...){
  as.character(match.call(expand.dots = TRUE))[-1]
}

#' Function to list multiple objects passed as \code{...}
#'
#' @param ... Objects on which to operate
#' @param naming Logical | Whether to name the list after the symbols of the provided objects. Defaults to \code{TRUE}
#'
#' @return A (possibly named) list of the provided objects.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

listing <- function(..., naming = TRUE){
  y <- list(...)
  if(naming){
    names(y) <- naming(...)
  }
  y
}

#' Function to chose the right algorithm when querying one or more information over multiple \code{firm} objects
#'
#' @param firm List of objects on which to operate
#' @param naming Logical | Whether to name the result after \code{names(firms)}. Defaults to \code{TRUE}
#' @param unlisting Logical | Whether to un-list the result. Defaults to \code{FALSE}
#'
#' @return The queried information.
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

query.firms_switch <- function(firm, which, naming = TRUE, unlisting = FALSE){
  res <- if(length(which == 1)){
    query.firm(firm = firm, which = which, naming = naming)
  } else {
    lapply(which, function(this){
      query.firm(firm = firm, which = this, naming = naming)
    })
  }

  if(length(res)>0){
    if(naming)names(res) <- which
    if(unlisting)res <- unlist(res)
  }

  res
}

#' Function to compute the binary values of the FM or FO matrix for multiple \code{firm} objects
#'
#' @param firms List of objects on which to operate
#' @param which Whether to use ownership or management to construct the matrix
#' @param cols Possible values assumed by the enquired variable (determined autmotically if not provided)
#'
#' @return The values to be filled in the binary FO or FM matrix
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

get.binarary.values <- function(firms, which, cols = NULL){
  data <- query.firms(firms = firms, which = which)

  if(is.null.na(cols)){
    cols <- unlist(data)|> unique()|> sort()
  }

  res <- lapply(data, function(x){
    pos <- match(x, cols)
    y <- rep(0, times = length(cols))
    y[pos] <- 1
    y
  })

  unlist(res)
}

#' Function to check whether the provided colour is actually a shade of grey and correct it if necessary
#'
#' @param hex The RGB colour to check
#'
#' @return A valid grey colour in RGB format
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

check.correct.grey <- function(hex){
  clrs <- lapply(c(2, 4, 6), function(i){
    substr(hex, i, i+1)
  })|> unlist()

  y <- rep(clrs[1], 3)|> paste0(collapse = '')|>
    paste0('#', x = _)

  if(!all(clrs==clrs[1])){
    warning(c('Incorrect grey specified: ', hex,
              '\nReplacing with:', y))
  }

  y
}


#' Function to rescale numeric vectors
#'
#' Rescale continuous vector to have specified minimum and maximum
#'
#' From: The package \href{https://scales.r-lib.org/}{\code{scales}}
#'
#' @param x A vector to re-scale
#' @param to output range (numeric vector of length two)
#' @param from input range (vector of length two).  If not given, is
#'   calculated from the range of \code{x}
#'
#' @return Re-scaled vector
#'
#' @author \href{https://scales.r-lib.org/}{Hadley Wickham}
#'
#' @keywords internal

rescale.numeric <- function(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE), ...) {
  if (diff(from) || diff(to)) {
    return(ifelse(is.na(x), NA, mean(to)))
  }
  (x - from[1]) / diff(from) * diff(to) + to[1]
}

#' Function to create the default palette and ramp it up as needed
#'
#' @param length Length of the desired palette
#'
#' @return A vector of RGB codes containing \code{length} colours
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

palette <- function(length = 6){
  grDevices::colorRampPalette(c('#00204DFF', '#31446BFF', '#666970FF',
                                '#958F78FF', '#CBBA69FF', '#FFEA46FF')
  )(length)
}

#' Function to set a vertex  or edge attribute of a \code{network} or \code{graph} object
#'
#' @param x The representation of the network as a \code{network} or \code{graph} object
#' @param where What network element does the attribute refer to. Either \code{edge}/\code{tie} or \code{vertex}/\code{node}.
#' @param attr_name Name of the attribute to set
#' @param value of the attribute to set
#' @param which A subset of elements on which the attribute should be applied. Defaults to all the vertexes/nodes.
#'
#' @return A \code{network} or \code{graph} object with the desired attribute
#'
#' @author \enc{Telarico, Fabio Ashtar}{Fabio Ashtar Telarico}
#'
#' @keywords internal

add.attribute <- function(x, where = c('edge', 'vertex'),
                          attr_name, value, which = NULL){
  if(methods::is(x, 'igraph')){

    if(where == 'edge'){
      x <- igraph::set.edge.attribute(
        x, name = attr_name, value = value,
        index = if(is.null.na(which)){
          igraph::E(x)
        } else {
          which
        }
      )
    } else {
      x <- igraph::set.vertex.attribute(
        x, name = attr_name, value = value,
        index = if(is.null.na(which)){
          igraph::V(x)
        } else {
          which
        }
      )
    }

  } else {

    if(where == 'edge'){
      x <- network::set.edge.attribute(
        x, attrname = attr_name, value = value,
        e =  if(is.null.na(which)){
          seq_along(x$mel)
        } else {
          which
        }
      )
    } else {
      x <- network::set.vertex.attribute(
        x, attrname = attr_name, value = value,
        index =  if(is.null.na(which)){
          seq_len(network::network.size(x))
        } else {
          which
        }
      )
    }
  }

  x
}
