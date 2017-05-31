library(devtools)
install_github("PecanProject/pecan", subdir="all")

# Play with example from GPfit package
library(GPfit)
## 1D Example 1
n = 5; d = 1;
computer_simulator <- function(x){
  x = 2*x+0.5;
  y = sin(10*pi*x)/(2*x) + (x-1)^4;
  return(y)
}
set.seed(3);
library(lhs);
x = maximinLHS(n,d);
y = computer_simulator(x);
GPmodel = GP_fit(x,y);
print(GPmodel)

## 2D Example: GoldPrice Function
computer_simulator <- function(x) {
  x1=4*x[,1] - 2; x2=4*x[,2] - 2;
  t1 = 1 + (x1 + x2 + 1)^2*(19 - 14*x1 + 3*x1^2 - 14*x2 +
                              6*x1*x2 + 3*x2^2);
  t2 = 30 + (2*x1 -3*x2)^2*(18 - 32*x1 + 12*x1^2 + 48*x2 -
                              36*x1*x2 + 27*x2^2);
  y = t1*t2;
  return(y)
}
n = 30; d = 2;
set.seed(1);
library(lhs);
x = maximinLHS(n,d);
y = computer_simulator(x);
GPmodel = GP_fit(x,y);
print(GPmodel)

##############################################################
# GP_fit only needs two arguments:
# X: matrix of parameters/probs (640 x 12 for LHC runs)
# Y: vector of output (640 observations). 
# fits fxn so can generate new output (likelihood) without running full model
# Then, can use MCMC and replace LPJ-GUESS run at each iteration by running
# the gaussian likelihood function.
# Huh. in this example it only works for 1 time step, 1 site.
# I guess that's why we might want the paramters to predict a likelihood- that's okay
###############################################################
