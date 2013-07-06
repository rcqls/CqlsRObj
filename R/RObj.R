#############################################################################################
# RClass and RModule:

RClass <- RModule <- function(name,include,...) {
  # try to return RClass of the existing object
  res <- try(inherits(name,"RObj"),TRUE)
  if(!inherits(res,"try-error") && res ) {
    rclass <- sapply(get.RClass(name),class2RClass)
    rclass <- rclass[-length(rclass)]
    names(rclass) <- NULL
    return(rclass)
  }
  # otherwise, creation of the RClass!
  if(missing(include)) include <- NULL else include <- get.Mixins(deparse(substitute(include)))
  # type is either Module or Class
  type <- as.character(match.call())[1]
  name<-deparse(substitute(name))
  name <- get.ParentClass(name)
  tmpParent <- NULL
  if(length(name)==2) { #parent rclass  
    tmpParent <- get.Names(name[2])#,"RClass")
    name <- name[1]    
  }
  tmpClass <- get.Names(name)
  # already exists?
  ### obj.to.create <- !exists(name,envir=.RObj[[type]],inherits=FALSE) 
  ### if ( obj.to.create ) {
  ### replaced by
  obj <- get.FromRoot(tmpClass$rclass)
  if(obj.to.create <- is.null(obj)) {
    # to create
    if(is.null(tmpParent)) parent <- .RObj
    else {
      parent <- get.FromRoot(tmpParent$rclass)
      if(is.null(parent)) stop("parent RClass is not available!")
    }
    obj <- Binding(new.env(parent=parent))
    names(tmpClass$name) <- NULL
    class(obj) <- c(type,class(obj))
    if(type=="RClass") attr(obj,"RClass") <- c(RClass2class(tmpClass$rclass),get.RClass(obj))
    # save the RClass or RModule in the container 
    container <- get.FromRoot(tmpClass$path)
    if(is.null(container)) stop("container is not available!") 
    assign(tmpClass$name,obj,envir=container)
  ### } else {
  ###  obj <- get(name,envir=.RObj[[type]])
  }
  # include 
  if(!is.null(include)) {
    #print(include)
    for(inc in rev(include)) {
      tmpMixins <- get.Names(inc)
      mixins <- get.FromRoot(tmpMixins$rclass)
      if(is.null(mixins)) stop(tmpMixins$rclass," is not an RModule to be included!")
      RInclude(obj,mixins)
    }
  }
  # methods
  RMethod(obj,...)
  # assign the object
  ### if( obj.to.create ) assign(tmp$name,obj,envir=parent)
  updateRObj()
  invisible(obj)
}

get.RClass <- function(obj) {
  rclass <- NULL
  env <- obj
  if(!identical(env,emptyenv())) {
    repeat {
      env <- parent.env(env)
      if( identical(env,.RObj) ) break
      if(inherits(env,"RClass")) rclass <- c(rclass,attr(env,"RClass")[1])
    }
  }
  rclass <- c(rclass,"RObj")
  rclass
}

RClass2class <- function(rclass) paste(unlist(strsplit(rclass,"\\$")),collapse=".")
class2RClass <- function(class) paste(unlist(strsplit(class,"\\.")),collapse="$")

clear.extSpaces <- function(str) gsub("[[:space:]]+$","",gsub("^[[:space:]]+","",str))


get.Mixins <- function(name) sapply(unlist(strsplit(name,"<")),clear.extSpaces)

get.ParentClass <- function(name) {
  res <- sapply(unlist(strsplit(name,"<")),clear.extSpaces)
  if(length(res)>2) stop("An RClass has only one parent RClass!")
  res
}

get.Names <- function(name) {
  res <- list()
  # classname and path
  path <- unlist(strsplit(name,"\\$"))
  if(length(path)==1) {
    res$path <- NULL
    res$name <- name
  } else {
    res$name <- path[length(path)]
    res$path <- paste(path[-length(path)],collapse="$")
  }
  res$rclass <- paste(c(res$path,res$name),collapse="$")
  res
}

get.FromRoot <- function(name,root=".RObj") {
  try(eval(parse(text=paste(c(root,name),collapse="$")),envir=globalenv()),TRUE)->res
  if(inherits(res,"try-error")) NULL
  else res
}

RMethod <- function(obj,...) {
  if(!inherits(obj,c("RClass","RModule","RObj"))) stop("only for objects of class RClass or RModule!")
  tmp <- list(...)
  ntmp <- names(tmp)
  if( !is.null(ntmp) ) {
    # remove empty names 
    tmp <- tmp[ntmp!=""]
    ntmp <- names(tmp)
    # the methods
    for( mth in ntmp ) {#for( mth in ntmp[ntmp!="include"] ) {
      #cat("method",mth,"\n")
      if(is.null(tmp[[mth]]) && is.function(obj[[paste(mth,"RMethod",sep=".")]]) ) {
        rm(list=paste(mth,"RMethod",sep="."),envir=obj)
      } else if(is.function(tmp[[mth]])) obj[[paste(mth,"RMethod",sep=".")]] <- tmp[[mth]]
    }
    ### include no longer a possible parameter
    ###for( mth in ntmp[ntmp=="include"] ) {
    ###  if(inherits(tmp[[mth]],"Binding")) RInclude(obj,tmp[[mth]])
    ###}
  }
}

is.RMethod <- function(obj,method) {
  if(!inherits(obj,c("RClass","RModule","RObj"))) stop("is.RMethod only applicable for objects of class RClass or RModule!")
  env <- obj
  repeat {
    env <- parent.env(env)
    if( identical(env,.RObj) ) break
    if( exists(method,envir=env,inherits=FALSE) ) return(TRUE)   
  } 
  return(FALSE)
} 

RNew <- function(rclass,...) {
  tmp <- get.Names(deparse(substitute(rclass))) 
  rclass <- get.FromRoot(tmp$rclass)
  if(is.null(rclass)) rclass <- .RObj[[tmp$name]]
  obj <- Binding(new.env(parent=rclass),self=TRUE)
  class(obj) <- c(attr(rclass,"RClass"),"Binding")
  init <- obj$initialize
  ## cat("Init->");print(init)
  if(!is.null(init) && is.function(init)) init(...)
  obj 
}

RInclude <- function(obj,...) {
  if(!inherits(obj,"Binding")) stop("obj has to be of class Binding!")
  for(mixins in rev(list(...))) {
    if(!inherits(mixins,"RModule")) stop("obj has to be of class RModule!")
    if(!include.Binding(obj,mixins)) { # the new mixins does not be already in the parents
      parent.env(mixins) <- parent.env(obj)
      parent.env(obj) <- mixins
    }
  }
  invisible()
}


## Not very useful
excludeMixins <- function(obj,mixins) {
  if(!inherits(obj,"Binding")) stop("obj has to be of class Binding!")
  if(!inherits(mixins,"RModule")) stop("obj has to be of class RModule!")
  ## to complete!!!! But not veru useful
  parent.env(mxixins) <- parent.env(obj)
  parent.env(obj) <- mixins
  invisible()
}

names.RObj <- function(obj) {
  objsList<-ls(obj)
  env <- obj
  repeat {
    env <- parent.env(env)
    if( identical(env,.RObj) ) break
    #avoid RClass and RModule object
    objsList <- c(objsList,unlist(sapply(ls(env),function(o) if(inherits(env[[o]],c("RClass","RModule"))) NULL else  o)))   
  }
  objsList <- sub("(.*)\\.RMethod$","\\1\\(\\)", sub("(.*)\\.ptR$","\\.\\1",objsList,perl=TRUE), perl=TRUE)
  names(objsList) <- NULL
  objsList <- setdiff(objsList,c("self","initialize()"))
  objsList
}

split.RObj <- function(obj) {
  res <- list()
  objsList <- names.RObj(obj)
  #ptR names
  ptRnames <- gsub("\\.(.*)$","\\1\\.ptR",objsList,perl=TRUE)
  inds <- which( ptRnames != objsList )
  res$ptR <- ptRnames[inds]
  if(!is.null(res$ptR)) names(res$ptR) <- objsList[inds]
  objsList<- setdiff(objsList,objsList[inds])
  #RMethod names
  methRnames <- gsub("(.*)\\(\\)$","\\1\\.RMethod",objsList,perl=TRUE)
  inds <- which( methRnames != objsList )
  res$methR <- methRnames[inds]
  if(!is.null(res$ptR)) names(res$methR) <- objsList[inds]
  #static names
  res$static <- setdiff(objsList,objsList[inds])
  res
}

# Delegate to print method if necessary!
print.RObj <- function(obj,...) {
  if(is.RMethod(obj,"print")) obj$print(...) 
  else {
    # fields
    objnames <- split.RObj(obj)
    # static
    if(length(objnames$static)) {
      for(i in 1:length(objnames$static) ) {
        cat("$",objnames$static[i],"\n",sep="")
        print(obj[[objnames$static[i]]])
      }
    }
    # dynamic
    if(length(objnames$ptR)) {
      tmp <- names(objnames$ptR)
      for(i in 1:length(objnames$ptR) ) {
        cat("$",tmp[i],"\n",sep="")
        print(obj[[objnames$ptR[i]]])
      }
    }
    # methods names
    cat("R-methods:\n");print(sort(names(objnames$methR))) 
    # R-class
    cat("R-class:\n");print(RClass(obj))
    # S3 class
    cat("S3-class:\n");print(class(obj)) 
  }
}


## REPLACEMENT

updateRObj <- function(force=FALSE) {
  ## if not existing need to be created!
  if(!exists(".RObj",envir=globalenv(),inherits=FALSE)) {
    root <- Binding(new.env(parent=globalenv()))
    assign(".RObj",root,envir=globalenv())
  }
  ## automatic attachment
  if(("package:CqlsRObj" %in% search()) || force ) {
      while( ".RObj" %in% search()) detach(".RObj")
      if(!force) {
        pos <- which("package:CqlsRObj" == search() )[1]+1
        attach(.RObj,pos=pos)
      } else {
        attach(.RObj)
      }
  }
}


## TO DELETE WHEN USELESS!!
# newRootObj <- function(rootName) { #rootName is a name
#   root <- Binding(new.env(parent=globalenv()))
#   assign(rootName,root,envir=globalenv())
#   assign("RModule",Binding(new.env(parent=root)),envir=root)
#   assign("RClass",Binding(new.env(parent=root$RModule)),envir=root)
#   return(invisible())
# }



 

# initRootObj <- function(root,attach=TRUE,force=FALSE) {
#   assign(".RObj",root,envir=globalenv())
#   attachRootObj(force)
# } 

# attachRootObj <- function(force=FALSE) {
#   if(("package:CqlsRObj" %in% search()) || force ) {
#       while( ".RObj$RClass" %in% search()) detach(".RObj$RClass")
#       while( ".RObj$RModule" %in% search()) detach(".RObj$RModule")
#       if(!force) {
#         pos <- which("package:CqlsRObj" == search() )[1]+1
#         attach(.RObj$RClass,pos=pos)
#         attach(.RObj$RModule,pos=pos)
#       } else {
#         attach(.RObj$RClass)
#         attach(.RObj$RModule)
#       }
#   }
# }

# detachRootObj <- function() {
#   if(("package:CqlsRObj" %in% search())) {
#       while( ".RObj$RClass" %in% search()) detach(".RObj$RClass")
#       while( ".RObj$RModule" %in% search()) detach(".RObj$RModule")
#   }
# }
