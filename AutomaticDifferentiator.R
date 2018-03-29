#automatic differentiator experiment:

#first, implement dual arithmetic:
"%++%"<-function(d1,d2){
  return(c(d1[1]+d2[1], d1[2]+d2[2]  ))
}
"%--%"<-function(d1,d2){
  return(c(d1[1]-d2[1], d1[2]-d2[2]  ))
}

#NOTE: (a,b)*(c,d)= (a*c, a*d+ c*b) ; if b is the derivative of a and d is the derivative of c, then the second part of result is the derivative of the first part of the result. Dual number arithmetic is differentiation.
"%**%"<-function(d1,d2){
  return(c(d1[1]*d2[1], d1[1]*d2[2]+d2[1]*d1[2]  ))
}
"%//%"<-function(d1,d2){
  return(c(d1[1]/d2[1], (d2[1]*d1[2]- d1[1]*d2[2]) / d2[1]^2  ))
}

dual_sin<-function(d){
  return(c(sin(d[1]), cos(d[1])*d[2]))
}
dual_cos<-function(d){
  return(c(cos(d[1]), -sin(d[1])*d[2]))
}

dual_exp <- function(d){
  return(c(exp(d[1]), exp(d[1])*d[2] ))
}

dual_log <- function(d){
  return(c(log(d[1]), (1/d[1])*d[2] ))
}

"%^^%"<-function(d, k){
  return(c(d[1]^k, k*(d[1]^(k-1))*d[2]))
}
dual_abs <- function(d){
  return(c(abs(d[1]), sign(d[1])*d[2] ))
}

#now, for example, suppose we want derivative of x^2+x+1 at x=2:
f<-function(d){
  (d%^^%2) %++% d %++% c(1,0)
}
x_0=c(2,1) #KEY POINT: if f(x)=x, then f'(x)=1, so to compute the derivative at any number x then we always set the infitesimal part of the dual number to 1.

#derivative is 2x+1, so 5
f(x_0)
#cool, perfect!


#or, a more complex function: eg. ((sin(x))^2)+cos(log(x))
# at x=2, which according to wolfram is -1.076283
f<-function(d){
  (dual_sin(d) %^^% 2) %++% dual_cos(dual_log(d))
}
x_0=c(2,1)
f(x_0)


#now try ((sin(x))^2)/cos(log(x))
# at x=2, which according to wolfram is -0.537423
f<-function(d){
  (dual_sin(d) %^^% 2) %//% dual_cos(dual_log(d))
}
x_0=c(2,1)
f(x_0)


