### TODO: think maybe to  restricted assignment till some root environment.
### be careful with the assign(...,inherits=TRUE)

############ ptR  class #########################################
# creation of ptR object
ptR <- function(value=NULL) {
  if(inherits(value,"ptR")) return(value) #used in "$.Binding"
  ptr <- new.env()
  ptr$value <- value
  class(ptr) <- "ptR" 
  ptr
}

print.ptR<-function(ptr) {
  cat("*");print(ptr$value)
}

# to update a ptR element
"%<-%" <- function(obj,value) UseMethod("%<-%") 

# Rmk: It is only called when obj exists, i.e. not in expression: obj %<-% "toto" when obj 
"%<-%.default" <- function(objname,value) {
  #default is to create a new ptR except if object already exists!
  if(!is.character(objname)) stop("an object reference ",objname," has to be character!")
  if(is.null(value)) {
    rm(list=objname,envir=parent.frame(),inherits=TRUE)
    return(invisible())
  }
  if(exists(objname,envir=parent.frame())) {
    # get the object
    obj<-get(objname,envir=parent.frame())
    # only ptR object has considered!
    if(!inherits(obj,"ptR")) stop("object ",objname," has to be of class ptR!")
    # depending on the class of the value object
    if(inherits(value,"ptR")) 
      # update the existing object
      obj<-assign(objname,value,envir=parent.frame(),inherits=TRUE)
    else 
      obj$value <- value
  } else {
    # create a new ptR
    obj <- assign(objname,ptR(value),envir=parent.frame())
  }
  obj
}

"%<-%.ptR" <- function(obj, value) {
  if(inherits(value,"ptR")) {
    objname<-deparse(substitute(obj))
    obj<-assign(objname,value,envir=parent.frame(),inherits=TRUE)
  } else obj$value <- value
  invisible(obj)
}


#generic stuff
Ops.ptR <- function (e1, e2) {
  if(inherits(e1,"ptR")) e1 <- e1$value
  if(inherits(e2,"ptR")) e2 <- e2$value
  ptR(get(.Generic, mode = "function")(e1, e2))
}

Math.ptR <- function(e1,...) ptR(get(.Generic, mode = "function")(e1$value,...))

Summary.ptR <- function(e1,...,na.rm=FALSE) ptR(get(.Generic, mode = "function")(e1$value,...,na.rm=na.rm))

Math2.ptR <- function(e1,digits) ptR(get(.Generic, mode = "function")(e1$value,digits))

mean.ptR <- function(obj,...) ptR(mean(obj$value,...))
