as.simple.formula <- function(attributes, class) {
	return(as.formula(paste(class, paste(attributes, sep = "", collapse = " + "), sep = " ~ ")))
}

get.data.frame.from.formula <- function(formula, data) {
	d = model.frame(formula, data, na.action = NULL)
	for(i in 1:dim(d)[2]) {
		if(is.factor(d[[i]]) || is.logical(d[[i]]) || is.character(d[[i]]))
			d[[i]] = factor(d[[i]])
	}
	return(d)
}

entropyHelper <- function(x, unit = "log") {
    return(entropy:::entropy(table(x, useNA="always"), unit = unit))
}


discretize.all <- function(formula, data) {
  new_data = get.data.frame.from.formula(formula, data)
  
  dest_column_name = dimnames(new_data)[[2]][1]
  if(!is.factor(new_data[[1]])) {
    new_data[[1]] = equal.frequency.binning.discretization(new_data[[1]], 5)
  }
  
  new_data = supervised.discretization(formula, data = new_data)
  
  # reorder attributes
  new_data = get.data.frame.from.formula(formula, new_data)
  return(new_data)
}

# unupervised
equal.frequency.binning.discretization <- function(data, bins) {
  bins = as.integer(bins)
  if (!is.numeric(data)) 
    stop("Data must be numeric")
  if(bins < 1)
    stop("Number of bins too small")
  
  complete = complete.cases(data)
  ord = order(data)
  len = length(data[complete])
  blen = len / bins
  new_data = data
  
  p1 = p2 = 0
  
  for(i in 1:bins) {
    p1 = p2 + 1
    p2 = round(i * blen)
    new_data[ord[p1:min(p2, len)]] = i
  }
  
  return(factor(new_data))
}

# unupervised
equal.width.binning.discretization <- function(data, bins) {
  if (!is.numeric(data)) 
    stop("Data must be numeric")
  if(bins < 1)
    stop("Number of bins too small")
  return(cut(data, bins))
}

#MDL - Fayyad, Irani
supervised.discretization <- function(formula, data) {
  data = get.data.frame.from.formula(formula, data)
  complete = complete.cases(data[[1]])
  all.complete = all(complete)
  if(!all.complete) {
    new_data = data[complete, , drop=FALSE]
    result = RWeka:::Discretize(formula, data = new_data, na.action = na.pass)
    return(result)
  } else {
    return(RWeka:::Discretize(formula, data = data, na.action = na.pass))
  }
  
}
