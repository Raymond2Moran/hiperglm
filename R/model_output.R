#' Extract estimated coefficients from a hiper_glm object
#'
#' @param hglm_out A fitted hiper_glm model object
#'
#' @return A numeric vector of estimated coefficients
#' @export
coef.hiper_glm <- function(hglm_out) {
  if (!"hiper_glm" %in% class(hglm_out)) {
    stop("Input is not a valid hiper_glm object.")
  }

  if (!is.null(hglm_out$coefficients)) {
    return(as.numeric(hglm_out$coefficients))
  } else {
    stop("No coefficients found in the hiper_glm object.")
  }
}

#' @export
vcov.hiper_glm <- function(hglm_out) {
  warning("This function is yet to be implemented.")
}

#' @export
print.hiper_glm <- function(hglm_out) {
  cat("Output of hiper_glm.\n")
}

# summary <- function() {

# }

# pred <- function() {

# }