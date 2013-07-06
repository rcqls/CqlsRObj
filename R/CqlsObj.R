# Simple mechanism without root inheritance as for RObj mechanism
CqlsObj <- function(...) {
  args.call <-  as.list(match.call())[-1]
  names.call <- names(args.call)
  if(is.null(names.call)) names.call <- rep("",length(args.call)) 
  class <- as.character(args.call[nchar(names.call)==0]) #input2character(substitute(class))
  names.call <- names.call[nchar(names.call)>0]
  ## new binding
  obj <- Binding()
  for(nm in names.call) CqlsRObj:::assignInBinding(obj,nm,eval.parent(args.call[[nm]]))
  class(obj) <- c(class,"Binding")
  obj
}

# External pointer declared inside an environment
.ExternalInEnvir <- function(name,...,envir,PACKAGE) {
  externalPtr <- .External(name,...,PACKAGE = PACKAGE)
  attr(externalPtr,"envir") <- envir
  externalPtr
}

externalPtrOutput.CqlsObj <- function(cqlsobj) capture.output(print(cqlsobj$extPtr))

# utility function to convert an input to a character even if the input is not a character. 
#input2character <- function(substituted.name) if(is.character(substituted.name)) substituted.name else deparse(substituted.name)
