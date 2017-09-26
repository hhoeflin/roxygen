# Extract all methods from an RC definition, returning a list of "objects".
r6_methods <- function(obj) {
  stopifnot(methods::is(obj, "R6ClassGenerator"))

  methods_public <- obj$public_methods
  methods_active <- obj$active

  ## of the public methods, the initialize method will be renamed to "new"
  if(any(names(methods_public) == "initialize")) {
      names(methods_public)[which(names(methods_public) == "initialize")] <- "new"
  }
  
  ## add the metadata
  for(i in seq_along(methods_public)) {
      methods_public[[i]] <- add_r6_metadata(methods_public[[i]], name=names(methods_public)[i], class=class(obj), is_active=FALSE)
  }
  for(i in seq_along(methods_active)) {
      methods_active[[i]] <- add_r6_metadata(methods_active[[i]], name=names(methods_active)[i], class=class(obj), is_active=TRUE)
  }

  
  methods_obj <- lapply(c(methods_public, methods_active), object)
  return(methods_obj)
}

add_r6_metadata <- function(val, name, class, is_active) {
  class(val) <- c("r6method", "function")
  attr(val, "r6class") <- class
  attr(val, "r6method") <- name
  attr(val, "r6is_active") <- is_active

  val
}



# Modified from docstring - a function has a doc block
# if it's a call to {, with more than 1 element, and the first element is
# a character vector.
# it also checks if the srcref is attached and uses comments if
# they are available
docblock <- function(f) {
  stopifnot(is.function(f))
  if (is.primitive(f)) return(NULL)

  b <- body(f)
      
  if (length(b) <= 2 || !identical(b[[1]], quote(`{`))) return(NULL)

  first <- b[[2]]
  if (!is.character(first)) return(NULL)

  ## now get all the elements that are character vectors until there are no more
  element_is_character <- unlist(lapply(b, is.character))
  if(!all(element_is_character[-1])) {
      last_block_line <- min(which(!element_is_character[-1]))
  }
  else {
      last_block_line <- length(b)
  }

  ## now we need some more processing
  ## first standardize so that no empty lines exist
  block <- unlist(as.list(b)[2:last_block_line])

  ## make it into one character string, then split on empty lines into sub-blocks
  block_one_line <- paste(block, collapse="\n")
  ## Strip off trailing and leading blank lines:
  block_one_line <- gsub("^\n+|\n+$", "", block_one_line)

  subblocks <- strsplit(block_one_line, split="\n\\s*\n[\n\\s]*", perl=TRUE, fixed=FALSE)[[1]]

  subblocks_split <- do.call(rbind, lapply(subblocks, split_block))

  ## check that only param lines exist and at most one return line
  all_hunk_names <- unique(subblocks_split$hunk_names)

  nonallowed_names <- setdiff(all_hunk_names, c(NA, "param", "return"))
  if(length(nonallowed_names) > 0) {
      stop("@-values not allowed used: ", paste(paste0("@", nonallowed_names), collapse=", "))
  }

  ## check that there is at most one return line
  if(sum(with(subblocks_split, !is.na(hunk_names) & hunk_names == "return")) > 1) {
      stop("More than 1 return value specified")
  }

  ## extract all non-parameter lines from all subblocks
  ## then remove the empty ones
  non_at_blocks <- with(subblocks_split, hunk_values[is.na(hunk_names)])
  non_at_blocks <- non_at_blocks[unlist(lapply(non_at_blocks, length)) > 0]
  non_at_blocks_rd <- paste(non_at_blocks, collapse="\n\n")
  
  ## get all param-hunks
  param_hunks <- with(subblocks_split, hunk_values[!is.na(hunk_names) & hunk_names=="param"])
  
  ## if there are any param_lines, make a describe block
  if(length(param_hunks) > 0) {
      param_beginning <- "\\strong{Parameters}\n\\describe{"
      param_ending <- "}"
      item_lines <- unlist(lapply(as.list(param_hunks), paramhunk_to_item))
      param_block_rd <- paste(c(param_beginning, item_lines, param_ending), collapse="\n")
  }
  else {
      param_block_rd <- character(0)
  }

  ## get the return-hunk 
  return_hunks <- with(subblocks_split, hunk_values[!is.na(hunk_names) & hunk_names=="return"])
  if(length(return_hunks) > 0) { # in this case has to be of length 1
      return_beginning <- "\\strong{Return}\n"
      return_ending <- "\n\n"
      return_lines <- returnhunk_to_item(return_hunks)
      return_block_rd <- paste(c(return_beginning, return_lines, return_ending), collapse="\n")
  }
  else {
      return_block_rd <- character(0)
  }
    
  return(paste(c("", non_at_blocks, param_block_rd, return_block_rd), collapse="\n\n"))
}


split_block <- function(block) {
    ## we want to split off any blocks that are non-parameteric blocks
    ## that is the case for all blocks before the occurence of an @ sign
    ## split at the @-signs
    block_split <- strsplit(block, split="[\\s\n]*@", perl=TRUE, fixed=FALSE)[[1]]
    non_at_line <- block_split[1]
    at_lines <- block_split[-1]
    # the word at the beginning of each break is the name of the parameter
    at_line_regexp <- "^(\\S+)\\s(.*)$"
    hunk_names <- gsub(at_line_regexp, "\\1", at_lines)
    hunk_values <- gsub(at_line_regexp, "\\2", at_lines)

    return(data.frame(hunk_names=c(NA, hunk_names), hunk_values=c(non_at_line, hunk_values), stringsAsFactors = FALSE))
}


paramhunk_to_item <- function(line) {
    ## strip starting whitespace
    line <- gsub("^\\s+", "", line, perl=TRUE)

    ## split off the first word
    first_space <- regexpr("\\s", line, perl=TRUE)
    if(first_space==-1) { # no hit
        first_word <- line
        remaining <- ""
    }
    else {
        first_space_len <- attr(first_space, "match.length")
        first_word <- substr(line, start=1, stop=first_space-1)
        remaining <- substr(line, start=first_space + first_space_len, stop=nchar(line))
    }
       
    ## check that remaining is rdComplete
    if(!rdComplete(first_word)) {
        stop(paste("Not Rd complete:", first_word))
    }
    if(!rdComplete(remaining)) {
        stop(paste("Not Rd complete:", remaining))
    }
    return(paste0("\\item{", first_word, "}{", remaining, "}"))
}

returnhunk_to_item <- function(line) {
    
    ## nothing to do but check that remaining is rdComplete
    if(!rdComplete(line)) {
        stop(paste("Not Rd complete:", line))
    }
    return(line)
}
