########## Binding class #######################################
Binding<-function(env,self=FALSE) {
  if(missing(env)) env<-new.env()
  if(!is.environment(env)) stop("env has to be an environment!") 
  if(self) assign("self",env,envir=env)
  class(env) <- "Binding"
  env
}

## names.Binding <- function(binding) ls(binding)

names.Binding <- function(binding) {
  objsList <- ls(binding)
  ## copy .ptR with .ptRVal
  tmp<- objsList[grep("(.*)\\.ptR$",objsList,perl=TRUE)]
  if(length(tmp)) tmp <- paste(tmp,"Val",sep="")
  objsList <- c(objsList,tmp)
  objsList <- sub("(.*)\\.ptRVal$","\\1",objsList,perl=TRUE)
  objsList <- sub("(.*)\\.ptR$","\\.\\1",objsList,perl=TRUE)
  objsList <- sub("(.*)\\.RMethod$","\\1\\(\\)",objsList,perl=TRUE)
  objsList <- sort(objsList)
  names(objsList) <- NULL
  objsList
}

print.Binding <- function(binding,...) {
  class(binding) -> oldClass
  attr(binding,"class") <- NULL
  cat("Binding in ");print(binding)
  class(binding) <- oldClass
  return(invisible())
}

# replacement???
# print.Binding <- function(binding) NextMethod()




############################################################
# delegate value if the result of binding[[name]] is of class ptR
# 
# no use of binding[[...]] since an object could implement this method for its own class.
"$.Binding" <- function(binding,name) {
  # First, methR? => Important: has to be in first position!!!!
  if(exists(methRname <- paste(name,"RMethod",sep="."),envir=binding) ) {
    #print("Before");print(paste(name,"RMethod",sep="."));print(binding) 
    res <- get(methRname,envir=binding,inherits=TRUE) #parent environments allowed for methods!
    #print("After")
    environment(res) <- new.env(parent=binding) #like this, one can define "super" which is temporary! 
    if(inherits(binding,"RObj") &&  (methRname %in% ls(parent.class<-parent.env(parent.env(binding))))) {
      super <- get(methRname,envir=parent.class,inherits=TRUE)
      environment(super) <- binding
      assign("super",super,envir=environment(res))
    }
    return(res)
  }

  # Then, the dynamic and static stuff

  if( substring(name,1,1) == "." ) return(get(paste(substring(name,2),"ptR",sep="."),envir=binding,inherits=FALSE))
  if( exists( (ptRname <- paste(name,"ptR",sep=".")) ,envir=binding,inherits=FALSE)) return(get(ptRname,envir=binding,inherits=FALSE)$value)
  # the following case specifically considered in the assignment is treated as the classic static case: if( length(grep("\\.ptR$","",name)) ) return(binding[[name]])
  # classic pattern => the static mode unless there exists the field <name>.ptR
  return(get(name,envir=binding,inherits=FALSE)) # static and normal mode!!!
}

# the rule is that a name starting by a "." is a ptR
# RMK: if value is a function this does not mean that it is a method, use RClass instead!
# no use of binding[[...]] <- ... since an object could implement this method for its own class.
"$<-.Binding" <- assignInBinding <- function(binding,name,value) {
  # find  the right name
  # specific patterns
  if( substring(name,1,1) == "." ) ptRname <- paste(substring(name,2),"ptR",sep=".")
  else if( grepl("\\.ptR$","",name) ) ptRname <- name
  # classic pattern => the static mode unless there exists the field <name>.ptR
  else if(!exists( (ptRname <- paste(name,"ptR",sep=".")) ,envir=binding,inherits=FALSE)) {
    # static and normal mode!!!
    assign(name,value,envir=binding) 
    return(invisible(binding))
  }
  #else #nothing to do! ptRname already updated in the if clause!
 
  # dynamic mode (ptRname is updated just above!)
  # no methods considered => restriction of the environment => only the current!
  if(exists(ptRname,envir=binding,inherits=FALSE)) {# already exists => update mode
    if(!inherits(binding[[ptRname]],"ptR")) stop("field ",ptRname," has to be of class ptR!")
    if( is.null(value) ) {#remove the ptR in binding
      rm(list=ptRname,envir=binding)                
    } else if(inherits(value,"ptR")) {
      # RMK: the old ptR will be removed as an usual bindingect except that if it is used by another 
      # bindingect, the environment of ptR will not be removed as R does.
      assign(ptRname, value, envir=binding)
    } else #only the content is updated
      assign("value",value,envir=get(ptRname,envir=binding))
  } else { # creation mode
    assign(ptRname, ptR(value),envir=binding)        #ok since ptR(ptr) returns ptr if ptr is of class ptR  
  }
  binding
}

# Not sure to be used! eventually useable to update global variable for class from bindingect but is it normal to allow this!
get.Binding <- function(binding,name) { #only called if binding is Binding
  if(!exists(name,envir=binding)) return(NULL)
  env <- binding
  repeat {
    if(exists(name,envir=env)) return(env)
    env <- parent.env(env)
    if(identical(env,emptyenv())) break
  }
  return(NULL)
}

# is "parent" in the parents of "binding"? 
include.Binding <- function(binding,parent) { #only called if binding is Binding
  env <- parent.env(binding)
  repeat {
    if( identical(env,parent) ) return(TRUE)
    env <- parent.env(env)
    if(identical(env,emptyenv())) break
  }
  return(FALSE)
}
