#' @title leg_type
#'
#' @description Internal function.
#'
#' @return Returns the classification of a party system type.
#'
#' @noRd

leg_type <- function(x) {
    x <- sort(x, decreasing = TRUE)
    if (any(x > 1)) {
        stop("Shares must be in range 0 to 1 (not 0 to 100)")
    }
    
    if (x[1] >= 0.5) {
        return("A (single-winning party)")
    } 
    if ((x[2] + x[3]) < 0.5) {
        return("B (strongly dominant party)")
    }
    if ((x[2] + x[3]) >= 0.5) {
        return("C (top-three system)")
    }
    if ((x[1] + x[3]) < 0.5) {
        return("D (top-two system)")
    }
    if ((x[1] + x[2]) < 0.5) {
        return("E (open system)")
    }
}
