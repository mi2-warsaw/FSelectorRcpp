#' Formula to variables names
#'
#' Converts formula to character vector
#'
#'
#' @noRd
#'
formula2names <- function(formula, data) {
  # lFormula = as.list(formula)
  #
  # y = as.character(lFormula[[2]])
  #
  # if(as.list(lFormula[[3]])[[1]] == ".")
  # {
  #   cnames = colnames(data)
  #   x = cnames[!cnames == y]
  # } else
  # {
  #   x = as.list(lFormula[[3]])[-1]
  #   x = sapply(x, as.character)
  # }
  y <- formula[[2]]
  x <- attr(stats::terms(formula, data = data), "term.labels")

  list(y = as.character(y), x = x)
}

#' Create formula object
#'
#' Utility function to create formula object
#'
#' @param attr character vector with names of independent variables
#' @param class single string with dependent variable name
#'
#' @examples
#'
#' to_formula(names(iris)[-5], names(iris)[5])
#'
#' @importFrom stats as.formula
#' @export
to_formula <- function(attr, class) {
  as.formula(paste(class, paste(attr, collapse = " + "), sep = " ~ "))
}

#### fs_get_best_attributes ----
#' Extract best attributes subset from fitted object.
#'
#' @param x object fitted with \code{exhaustive_search} function.
#'
#' @examples
#'
#' #' # evaluator from FSelector package
#' evaluator = function(subset, data)
#' {
#'   library(rpart)
#'   k <- 5
#'   splits <- runif(nrow(data))
#'   results = sapply(1:k, function(i) {
#'   test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
#'   train.idx <- !test.idx
#'   test <- data[test.idx, , drop=FALSE]
#'   train <- data[train.idx, , drop=FALSE]
#'   tree <- rpart(to_formula(subset, "Species"), train)
#'   error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
#'   return(1 - error.rate)
#'    })
#'   return(mean(results))
#' }
#'
#'  fit = exhaustive_search(names(iris)[-5], evaluator, iris, allowParallel = FALSE)
#'  get_best_attributes(fit)
#'
#'  # with to_formula
#'  to_formula(get_best_attributes(fit), "Species")
#'
#' @export
get_best_attributes <- function(x) {
  UseMethod("get_best_attributes")
}

#' @export
get_best_attributes.ExhaustiveSearchResult <- function(x) {
  x$attributes[as.logical(x$bestAttr)]
}

#### print functions ----

#' @export
print.ExhaustiveSearchResult <- function(x, ...) {
  cat("Exhaustive Search Result:\n\n")
  cat("\t", paste(get_best_attributes(x), collapse = " + "))
  cat("\n\n\n")
  if (!is.null(x$allResult)) {
    cat('  Results for other attributes subsamples are avaiable.
        You can extract them with:
        x[["allResult"]]\n')
  } else {
    cat("  Results for other attributes subsamples are not avaiable.
        You can get them by rerunning fs_exhaustive_search with:
        keepAll = TRUE\n")
  }
}

#### create children ----

#' @importFrom utils combn
get_children <- function(parent, direction = c("forward", "backward", "both"),
                         omit.func = NULL) {
  # adopted from FSelector package
  direction <- match.arg(direction)
  if (!is.null(omit.func)) {
    omit.func <- match.fun(omit.func)
  }

  cols <- length(parent)
  if (cols <= 0) {
    stop("Parent attribute set cannot be empty.")
  }

  m1 <- NULL
  m2 <- NULL

  if (direction == "forward" || direction == "both") {
    rows <- cols - sum(parent)
    if (rows > 0) {
      m1 <- matrix(parent, ncol = cols, nrow = rows, byrow = TRUE)
      CurrRow <- 1
      CurrCol <- 1
      while(CurrCol <= cols && CurrRow <= rows) {
        if (m1[CurrRow, CurrCol] == 0) {
          m1[CurrRow, CurrCol] <- 1
          CurrRow <- CurrRow + 1
        }
        CurrCol <- CurrCol + 1
      }
    }
  }

  if (direction == "backward" || direction == "both") {
    rows <- sum(parent)
    if (rows > 1) {
      m2 <- matrix(parent, ncol = cols, nrow = rows, byrow = TRUE)
      CurrRow <- 1
      CurrCol <- 1
      while(CurrCol <= cols && CurrRow <= rows) {
        if (m2[CurrRow, CurrCol] == 1) {
          m2[CurrRow, CurrCol] <- 0
          CurrRow <- CurrRow + 1
        }
        CurrCol <- CurrCol + 1
      }
    }
  }

  m <- rbind(m1, m2)
  if (is.null(m)) {
    return(m)
  }

  if (!is.null(omit.func)) {
    RowToOmit <- apply(m, 1, omit.func)
    return(m[!RowToOmit, , drop = FALSE])
  } else {
    return(m)
  }
}

#' Names for different classes
#'
#' Sets names of output based on call
#'
#'
#' @noRd
#'
call2names <- function(vecCall) {
  charCall <- as.character(vecCall)
  if (length(charCall) == 1) {
    charCall
  } else if (charCall[1] == "$") {
    charCall[3]
  } else {
    toSub <- grep(pattern = "\\$", x = charCall, value = TRUE)
    gsub(pattern = ".*\\$", replacement = "", x = toSub)
  }
}

#' Format handler
#'
#' Handles values format
#'
#'
#' @noRd
#'
format_handler <- function(xCall, x) {
  x <- as.data.frame(x)
  colnames(x) <- call2names(xCall)
  x
}
