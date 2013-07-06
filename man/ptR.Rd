\name{ptR}
\alias{ptR}
\alias{\%<-\%}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Pointers for R objects }
\description{
  Pointer mechanism and dynamic fields for object in the Rb-OOP system.
}
\usage{
ptR(value)
key \%<-\% value
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{key}{ a character string or an object of class "ptR". }
  \item{value}{ any R object or an object of class "ptR". }
}
\details{
  Thanks to the environment object provided by the \R system, it is possible to easily provide in the \R system the notion of pointer. Let us assume that a variable \code{pt1} is assigned to a new \R environment and that the variable \code{value} is initialized to the number 1. This content is accessible by typing the code \code{pt1$value}. Then, a new variable \code{pt2} assigned to \code{pt1} contains the variable \code{value} as a list does. But, differently from a list, any change of the content of the variable \code{value} in the environment \code{pt1} is also updated in the environment \code{pt2}. For example, if one executes \code{pt1$value <- 2} then \code{pt2$value} leads to the output 2. The dynamic nature of environment is definitely different from the static nature of most of the other objects provided by the \R system.
  The class \code{ptR} related to the notion of pointer is internally represented as an environment containing only one R variable named \code{value}. 
  }
\value{
    The execution of the code \code{ptR(value)} returns a pointer (i.e. object of class \code{ptR}) with content \code{value}. To directly assign a pointer, the user operator \code{\%<-\%} is provided.
At the creation, the name of the pointer in its character string has to be supplied in the left-handed side. Once the pointer is created, to update the content of the pointer, it is then possible to directly put the pointer in the left-handed side. If the right-handed side refers to an existing pointer, a replacement of the environment is directly applied as expected. However, this last possibility is better performed by directly assigning a pointer with another pointer.
Let us also underline that the notion of pointer has been introduced to provide the notion of dynamic instance variable in the concept of OOP called R-OOP proposed to mimick Ruby-like OOP in the \R system. In a such context, the user does not have to know how a pointer is internally represented.
}
\author{ R. Drouilhet }

\seealso{ \code{\link{Binding}}, \code{\link{CqlsObj}}, \code{\link{RObj}}}
\examples{
## pointer
famousName <- ptR("Brown")
famousName
famousName$value

# pointer via the operator "\%<-\%"
"b" \%<-\% c(3,1:4) # equivalent to:  b <- ptR(c(3,1:4))
b
b$value

# since b is created, you could modified it
b \%<-\% c(b$value,2) # append 2
b

# equality means that the content is now dynamic!
bb <- b
bb

bb \%<-\% 5:2
bb
# b has changed too since b and bb represent the same environment.
b 
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
