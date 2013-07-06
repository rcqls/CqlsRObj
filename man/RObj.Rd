\name{RObj}
\alias{RObj}
\alias{RClass}
\alias{RModule}
\alias{RMethod}
\alias{RNew}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Class in the Ruby way }
\description{
  Functions providing a OOP mechanism (called R-OOP) inspired from the Ruby language. In order to avoid any confusion, each usual terminoly used in OOP will be prepended by "R-". Then, one will say a R-class instead of class, a R-module instead of module, a R-method instead of method, \dots.
}
\usage{
RClass(name,\dots)
RModule(name,\dots)
RMethod(name,\dots)
RNew(name,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{ a (non-empty) character string specifying the name \code{<name>} of the R-class or the R-module to define. For the functions \code{RClass} and \code{RNew}, \code{name} is a R-classname of the form: \code{[_Path_$]_RClass_ [< _ParentRClass_]} where \code{_RClass_} is the R-classname to define, \code{_Path_} is the (optional) complete name of the R-module containing the new R-class and \code{_ParentRClass_} is the (optional) parent R-classname. For the function \code{RModule}, \code{name} has a similar form as before except that no parent R-class needs to be provided. The argument \code{name} fulfilled in the method \code{RMethod} is either a R-class or an object in the R-OOP mechanism.}
  \item{\dots}{further arguments depending on the call to perform.}
}
\details{
  Thanks to the concepts of pointer (see \code{ptR} class) and  binding (see \code{Binding}) derived from the notion of environment prvided by the \R system, it is possible to mimick the Object-Oriented Programming used in languages like Ruby, Python, \dots. The functions \code{RClass} and \code{RModule} allow us to respectively define R-class and R-module. Module in Ruby can be used either as Mixins or as collection of functions and other objects like Class, for example. R-class is an object specifying the common methods related to a same family of objects to be instantied. Inheritance is provided as Ruby does by appending the parent R-class just after the name of the R-class to define. The new R-classname and parent R-classname are separated by the symbol\code{<} as in Ruby. 
Let us underline that all R-classes and R-modules are respectively of class \code{RClass} and \code{RModule} but also of class \code{Binding}. Each R-object as instance of R-Class is also of class \code{Binding}. Actually, all the inheritance stuffs is delegated to the class \code{Binding} which is mainly an environment.

The concept of instance variable introduced in Ruby is also proposed in the R-OOP mechanism. It
differs between the two languages mainly due to the difference in the nature of objects. In Ruby, atomic objects are mainly static when the other objects are mainly dynamic (Array, Hash, ...). In R, most of the objects are intrinsically static but thanks to the notion of environment and the derived notion of pointer (see class \code{ptR}), every static objects in R can now be also used as dynamic objects. An instance variable in Ruby is declared and used inside a method by giving a variable name of the form \code{@varname}. In the R-OOP mechanism, each R-object is encapsulated a binding and at least the variable \code{self} is instantied and refers to the binding of the R-object itself.
An instance variable is then provided inside a R-method by specifying \code{self$varname <- ...} for static variable and  \code{self$.varname <- ...} for dynamic variable . In both cases, \code{self$varname} returns the content of the instance variable (equivalent in Ruby to \code{@varname}).    
}
\value{
  Nothing is returned except for the function \code{RNew} which returns an object of class \code{name}. Notice that the function \code{RClass} with an existing object as argument returns its class in the R-OOP mechanism.   
}
\author{ R. Drouilhet }

\seealso{ \code{\link{Binding}}, \code{\link{ptR}}}
\examples{
\dontrun{# This is a ruby code}
\dontrun{module Famous}
\dontrun{  class Man}
\dontrun{    include Artist}
\dontrun{    def initialize(first="James",last="Bond")}
\dontrun{      @first=first}
\dontrun{      @last=last}
\dontrun{    end}
\dontrun{    def hello}
\dontrun{      puts "Hello! I am "+@first+" "+@last+"\\n"}
\dontrun{    end}
\dontrun{  end}
\dontrun{end}
\dontrun{jb3 = Famous::Man.new()}
\dontrun{jb3.hello}
\dontrun{# the code below tries to mimick it} 

## Module Artist used as a Mixin
RModule(Artist,
    sing=function() cat("La! La! La!\n")
)

## Module Famous used as a container
RModule(Famous)

## Class inside the Module Famous 
RClass(Famous$Man,
  include=Artist, 
  initialize=function(first="James",last="Bond") {self$first <- first;self$.last <- last},
  hello=function() cat("Hello! I am ",self$first," ",self$last,".\n",sep="") 
)
 
jb <- RNew(Famous$Man)
jb2 <- RNew(Famous$Man,first="JAMES",last=jb$.last)

jb$hello()  
jb2$hello() 
jb$sing()   # comes from Artis Mixin!

# Notice that, in the initialize function, "first" is static when "last" is dynamic.
jb$first <- "james"   # content of static instance variable updated
jb$last <- "Bond 007" # content of dynamic instance variable updated
jb$hello()   
# as "last" is a dynamic instance variable 
jb2$hello() # jb2$last has been updated at the same time as jb$last

RClass(jb) # class in the R-OOP mechanism
class(jb)  # usual class in the R system 

# Usual generic function applies!
print(jb)  # => print.RObj called
# The RClass Famous$Man becomes the S3 class Famous.Man 
print.Famous.Man <- function(obj) obj$hello()
print(jb)  # => now, print.Famous.Man called
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{htest}
%\keyword{univar}% __ONLY ONE__ keyword per line
