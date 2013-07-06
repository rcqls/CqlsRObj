\name{CqlsObj}
\alias{CqlsObj}
\alias{.ExternalInEnvir}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{CqlsObj class}
\description{
   CqlsObj class as a combination of usual S3 class and Ruby-like Binding.
}
\usage{
CqlsObj(...)
.ExternalInEnvir(name,...,envir,PACKAGE)
}
%- maybe also 'usage' for other objects documented here.
%\arguments{
%  \item{env}{ an environment. }
%  \item{name}{ an object of class \code{Binding}. }
%  \item{var}{ a variable name. }
%  \item{value}{ any R object or an object of class "ptR". }
%}
%\details{
%  \code{CqlsObj()} returns at least a \code{Binding} \R object. Unnamed arguments are implicitly converted in character and considered as the classes of the returned object (completed with the class \code{Binding}). Named arguments are used as initial values of fields of the newly created object.
%
%  \code{.ExternalInEnvir()} is an extension of the \code{.External()} instruction attaching the environment containing the external pointer as its attribute \code{envir}.
%  }
\value{
  \code{CqlsObj()} returns at least a \code{Binding} \R object. Unnamed arguments are implicitly converted in character and considered as the classes of the returned object (completed with the class \code{Binding}). Named arguments are used as initial values of fields of the newly created object.  
}
\author{ R. Drouilhet }

\seealso{ \code{\link{ptR}}, \code{\link{Binding}}}
\examples{
b <- CqlsObj(Spy,Man) #or CqlsObj("Spy","Man")

class(b)  # => c("Spy","Man","Binding")

b$first <- "James"  # regular R variable (not a ptR)
b$.last <- "Bond"   # last.ptR is created in binding b with value "toto"
b$.last             # "*" stands for a ptR and equivalent to b$last.ptR
b$last               # equivalent to b$.last$value or b$last.ptR$value

print(b)

print.Spy <- function(obj) cat("My name is",obj$first,obj$last,"!\n")

print(b)

b2 <- CqlsObj(Man,Spy,first=b$first,.last=b$.last) #instance variables can be initialized 
# equivalent to:
# b2 <- CqlsObj(Man,Spy)
# b2$first <- b$first # just a value not a ptR
# b2$.last <- b$.last # but not b2$.last <- b$last (only the value of b$last in b2$.last)

print(b2)

b2$first <- "JAMES"
b2$.last <- "BOND" # or b2$last <- "BOND" but maybe it is confusing!

print(b)

print.Man <- function(obj) cat("I am",obj$first,obj$last,"!\n")

print(b2)

# example of update in a dynamic way
update.Man <- function(obj,first,last) {obj$first <- first;obj$.last <- last}

update(b2,"Jamie","Bondie") # here it is!

print(b2) # b2$first and b2$.last updated

print(b) # b$.last updated but not b$first

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
