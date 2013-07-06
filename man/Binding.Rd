\name{Binding}
\alias{Binding}
\alias{$.Binding}
\alias{$<-.Binding}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Ruby-like Binding }
\description{
  Ruby-like binding to help mimicking OOP in the ruby system.
}
\usage{
Binding(env,self=FALSE)
binding$.var <- value
binding$.var
binding$var
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{env}{ an environment. }
  \item{binding}{ an object of class \code{Binding}. }
  \item{var}{ a variable name. }
  \item{value}{ any R object or an object of class "ptR". }
}
\details{Let us notice that the class \code{Binding} is mainly proposed for development use.
  Thanks to the environment object provided by the \R system, it is possible to mimick the basic principles of usual Oriented Object Programmation used in several languages as Ruby, Python, \dots.
  The concept of encapsulation is provided by the class \code{Binding}. An object, called \code{binding} for example, of the class \code{Binding} is no more than an environment with extraction facilities. Inside the object \code{binding}, objects named \code{<name>.ptR} are  considered as dynamic (i.e., not static) fields when objects named \code{<name>.RMethod} are considered as methods. A dynamic field is simply an object of class \code{ptR} (often called pointer). A method is no more than a function with object \code{binding} as environment. The class \code{Binding} allows us to manipulate pointers (i.e., objects of class \code{ptR}) and methods in a transparent way. \code{binding$.pt <- value} creates a pointer \code{pt} actually saved in the object \code{pt.ptR} inside the environment \code{binding}. \code{binding$.pt} returns the \code{ptR} object (saved in \code{pt.ptR}) and  \code{binding$pt} directly returns the value of the \code{ptR} object (inside \code{pt.ptR}). There is currently no explicit way to assign method in a \code{Binding} object. A method \code{meth} is simply declared inside \code{binding} by typing code of the form \code{binding$meth.RMethod <- function(...) {...}}. Extraction is however specifically supplied by \code{binding$meth} (i.e. without code{.RMethod}). In such a way, the environment \code{binding} is applied to the automatically extracted function \code{meth.RMethod} which then acts as a method. Any other extraction of the form \code{binding$name} when no object called \code{name.ptR} or \code{name.RMethod} is then considered as a regular (static) R object inside the environment \code{binding}. One then understand that the function \code{meth.RMethod} is then considered as a regular function and not a method.

The option \code{self=TRUE} when calling \code{Binding} completes the creation of the environment by assigning the object \code{self} inside the newly created environment to the environment itself. Combined with the extraction facilities provided by the class \code{Binding}, this allow the user to develop methods using pointers encapsulated inside the environment (see the example below).
  }
\value{
  \code{Binding} returns an object of class \code{Binding}. When called with \code{self=TRUE}, the resulting object (which is an environment) contains the object \code{self} with value the environment itself.   
}
\author{ R. Drouilhet }

\seealso{ \code{\link{ptR}}, \code{\link{CqlsObj}}, \code{\link{RObj}}}
\examples{
"nom" \%<-\% "Brown" # equivalent to nom <- ptR("Brown")
nom
nom$value

# new Binding which is simply an environment of class "Binding"
b <- Binding()
b$.nom <- "toto" # nom.ptR is created in binding b with value "toto"
b$.nom           # equivalent to b$nom.ptR
b$nom            # equivalent to b$.nom$value or b$nom.ptR$value

#dynamic stuff
b$.nom <- nom
b$.nom
nom \%<-\% "Bond"
b$.nom

# method mechanism
b2 <- Binding(self=TRUE) # self is created inside the environment b2 with value b2 
b2$.nom <- "James Bond"
b2
# method hello is created by specifically appending ".RMethod" 
b2$hello.RMethod <- function() cat("I am ",self$nom,"!\n",sep="")
# Call the method as below
b2$hello()        # => as a method, its environment is b2 itself!
# Be carefull, compare the environments of the two following functions
b2$hello
b2$hello.RMethod
# Indeed, the following call is not a method call! 
# b2$hello.RMethod()  # => error since its environment is not b2 and self is then unknown.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
