findQt_t<- function(p, df, k)
{
  if(k == 1)
  {
    qt(p, df)
  } else if(k == 2)
  {
    bdt <- function(x, y) {1/(2*pi)*(1+(x^2+y^2)/df)^(-(df+2)/2)}

    optimize(f = function(x) abs(pracma::integral2(bdt, x, 999, x, 999)$Q - (1-p)),
             interval = c(-10, 10), tol = 1e-4)[1]

  } else if(k == 3)
  {
    tdt <- function(x, y, z)
    {
      ((df*pi)^(-3/2)*gamma(df/2+3/2)/gamma(df/2)*(1+(x^2+y^2+z^2)/df)^(-(df+3)/2))
    }

    optimize(f = function(x) abs(pracma::integral3(tdt, x, 9, x, 9, x, 9) - (1-p)),
             interval = c(-10, 10), tol = 1e-4)[1]

  } else
  {
    optimize(f = function(x) abs(MCInt_t(k, df, x, 9, 20000) - (1-p)),
             interval = c(-5, 5), tol = 1e-4)[1]
  }
}
