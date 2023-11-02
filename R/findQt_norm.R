findQt_norm <- function(p, k)
{
  if(k == 1)
  {
    qnorm(p)
  } else if(k == 2)
  {
    bdnorm <- function(x, y) {1/sqrt(2*pi^2)*exp(-(x^2+y^2)/2)}

    optimize(f = function(x) abs(pracma::integral2(bdnorm, x, 999, x, 999)$Q - (1-p)),
             interval = c(-10, 10), tol = 1e-4)[1]

  } else if(k == 3)
  {
    tdnorm <- function(x, y, z) {1/sqrt(2*pi^3)*exp(-(x^2+y^2+z^2)/2)}

    optimize(f = function(x) abs(pracma::integral3(tdnorm, x, 9, x, 9, x, 9) - (1-p)),
             interval = c(-10, 10), tol = 1e-4)[1]

  } else
  {
    optimize(f = function(x) abs(MCInt_norm(k, x, 9, 20000) - (1-p)),
             interval = c(-5, 5), tol = 1e-4)[1]
  }
}
