## HW5 Class/Methods

setClass(
    Class = "sparse_numeric",
    slots = c(
        value = "numeric",
        pos = "integer",
        length = "integer"
    )
)

## setValidity() method

setValidity(
  Class = "sparse_numeric", 
  method = function(object) {
    errors = character()
    
    # Checks that the value vector is numeric class
    if (!is.numeric(object@value)) {
      errors = c(errors, "Value vector must be numeric class")
    }
    
    # Checks that position vector is integer class
    if (!is.integer(object@pos)) {
      errors = c(errors, "Position vector must be integer class")
    }
    
    # Check equal length of value and position vectors
    if (length(object@value) != length(object@pos)) {
      errors = c(errors, "Value and position vector lengths must be equal")
    }
    
    # Check for zeros in value vector
    if (any(object@value == 0)) {
      errors = c(errors, "Value vector must not contain zeros")
    }
    
    # Check for NA or NaN in value vector
    if (any(is.na(object@value)) || any(is.nan(object@value))) {
      errors = c(errors, "Value vector must not contain NA or NaN")
    }
    
    # Check for NA in positions
    if (any(is.na(object@pos))) {
      errors = c(errors, "Position vector must not include NA")
    }
    
    # Check that all positions are positive
    if (any(object@pos <= 0)) {
      errors = c(errors, "All positions must be positive integers")
    }
    
    # Check for duplicate positions
    if (any(duplicated(object@pos))) {
      errors = c(errors, "Position vector should not contain duplicated")
    }
    
    # Check that positions are sorted
    if (is.unsorted(object@pos)) {
      errors = c(errors, "Position vector must be sorted")
    }
    
    # Check length attribute
    if (length(object@length) != 1 || !is.integer(object@length)) {
      errors = c(errors, "Length attribute must be a single integer value")
    } else if (object@length < max(object@pos)) {
      errors = c(errors, "Length attribute must be greater than or equal to maximum position")
    } else if (object@length != as.integer(object@length) || object@ length <= 0) {
      errors = c(errors, "Length attribute must be a positive integer")
    }
    
    # Return TRUE if no errors, else return error vector
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
    
})

# Initialize method

setMethod("initialize", "sparse_numeric",
          function(.Object, value = numeric(0), pos = integer(0), length = 0L) {
            .Object@value <- value
            .Object@pos <- as.integer(pos)
            .Object@length <- as.integer(length)
            return(.Object)
          }
)

## add generic function + method

setGeneric(
  "sparse_add",
  function(x, y, ...) {
    standardGeneric("sparse_add")
  }
)

setMethod(
  "sparse_add",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length) {
      stop("Arguments must have the same length")
    }
    
    all_positions = sort(unique(c(x@pos, y@pos)))
    
    if (length(all_positions) == 0) {
      return(new("sparse_numeric",
                 value = numeric(0),
                 pos = integer(0),
                 length = x@length))
    }
    
    x_vals = rep(0, length(all_positions))
    y_vals = rep(0, length(all_positions))
    
    x_match = match(x@pos, all_positions)
    y_match = match(y@pos, all_positions)
    
    x_vals[x_match] = x@value
    y_vals[y_match] = y@value
    
    result_vals = x_vals + y_vals
    non_zero = result_vals != 0
    
    new("sparse_numeric",
        value = result_vals[non_zero],
        pos = all_positions[non_zero],
        length = x@length)
  }
)

## multiply generic function + method

setGeneric(
  "sparse_mult",
  function(x, y, ...) {
    standardGeneric("sparse_mult")
  }
)

setMethod(
  "sparse_mult",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length) {
      stop("Arguments must have the same length")
    }
    
    common_positions = intersect(x@pos, y@pos)
    
    if (length(common_positions) == 0) {
      return(new("sparse_numeric",
                 value = numeric(0),
                 pos = integer(0),
                 length = x@length))
    }
    
    x_vals = x@value[match(common_positions, x@pos)]
    y_vals = y@value[match(common_positions, y@pos)]
    
    result_vals = x_vals * y_vals
    non_zero = result_vals != 0
    
    new("sparse_numeric",
        value = result_vals[non_zero],
        pos = common_positions[non_zero],
        length = x@length)
  }
)

## subtract generic function + method

setGeneric(
  "sparse_sub",
  function(x, y, ...) {
    standardGeneric("sparse_sub")
  }
)

setMethod(
  "sparse_sub",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length) {
      stop("Arguments must have the same length")
    }
    
    all_positions = sort(unique(c(x@pos, y@pos)))
    
    if (length(all_positions) == 0) {
      return(new("sparse_numeric",
                 value = numeric(0),
                 pos = integer(0),
                 length = x@length))
    }
    
    x_vals = rep(0, length(all_positions))
    y_vals = rep(0, length(all_positions))
    
    x_match = match(x@pos, all_positions)
    y_match = match(y@pos, all_positions)
    
    x_vals[x_match] = x@value
    y_vals[y_match] = y@value
    
    result_vals = x_vals - y_vals
    non_zero = result_vals != 0
    
    new("sparse_numeric",
        value = result_vals[non_zero],
        pos = all_positions[non_zero],
        length = x@length)
  }
)

## cross product generic function + method

setGeneric(
  "sparse_crossprod",
  function(x, y, ...) {
    standardGeneric("sparse_crossprod")
  }
)

setMethod(
  "sparse_crossprod",
  signature(x = "sparse_numeric", y = "sparse_numeric"),
  function(x, y, ...) {
    if (x@length != y@length) {
      stop("Arguments must have the same length")
    }
    
    common_positions = intersect(x@pos, y@pos)
    
    if (length(common_positions) == 0) {
      return(0)
    }
    
    x_vals = x@value[match(common_positions, x@pos)]
    y_vals = y@value[match(common_positions, y@pos)]
    
    return(sum(x_vals * y_vals))
  }
)

## Arithmetic operator methods

setMethod("+", 
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) {
            sparse_add(e1, e2)
          }
)

setMethod("*", 
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) {
            sparse_mult(e1, e2)
          }
)

setMethod("-", 
          signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) {
            sparse_sub(e1, e2)
          }
)

## Coercion: numeric to sparse_numeric

setAs("numeric", "sparse_numeric",
      function(from) {
        non_zero_ids = which(from != 0)
        
        new("sparse_numeric",
            value = from[non_zero_ids],
            pos = as.integer(non_zero_ids),
            length = as.integer(length(from)))
      }
)

# Coercion: sparse_numeric to numeric
setAs("sparse_numeric", "numeric",
      function(from) {
        result = rep(0, from@length)
        result[from@pos] = from@value
        return(result)
      }
)

# Show method

setMethod("show", "sparse_numeric",
          function(object) {
            cat("Sparse numeric vector of length", object@length, "\n")
            if (length(object@pos) == 0) {
              cat("All zeros\n")
            } else {
              cat("Non-zero values:\n")
              for (i in seq_along(object@pos)) {
                cat(sprintf("  [%d] = %g\n", object@pos[i], object@value[i]))
              }
            }
          }
)

# Plot method

setMethod("plot", 
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            # Find overlapping non-zero positions
            common_positions = intersect(x@pos, y@pos)
            
            if (length(common_positions) == 0) {
              plot(0, 0, type = "n", 
                   xlab = "Position", ylab = "Value",
                   main = "Overlapping Non-Zero Elements",
                   xlim = c(0, max(x@length, y@length)),
                   ylim = c(0, 1))
              text(max(x@length, y@length)/2, 0.5, 
                   "No overlapping non-zero elements")
              return(invisible(NULL))
            }
            
            x_vals = x@value[match(common_positions, x@pos)]
            y_vals = y@value[match(common_positions, y@pos)]
            
            plot(common_positions, x_vals, 
                 col = "blue", pch = 16, cex = 1.5,
                 xlab = "Position", ylab = "Value",
                 main = "Overlapping Non-Zero Elements",
                 ylim = range(c(x_vals, y_vals)),
                 ...)
            points(common_positions, y_vals, 
                   col = "red", pch = 17, cex = 1.5)
            legend("topright", 
                   legend = c("x", "y"),
                   col = c("blue", "red"),
                   pch = c(16, 17))
          }
)

# Additional method: length

setMethod("length", "sparse_numeric",
          function(x) {
            return(x@length)
          }
)
