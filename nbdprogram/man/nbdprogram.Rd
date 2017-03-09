\name{nbdprogram}
\alias{nbdprogram}
\title{NBD Program}
\description{
 Estimates purchase rates in stationary markets.
}
\usage{
nbdprogram(b = 0.20, w = 2.1, t = 4)
}
\arguments{
\item{b}{This argument accept a numeric value about the market penetration in range
 0-1. Default is \code{b=0.20}.}
\item{w}{This argument accept a numeric value about the frequency of purchases.
  Default value is \code{w=2.1}.}
\item{t}{This argument accept an integer value in range 1-7. These are hard-coded
  periods of time:
  \itemize{
  \item \code{t=1} for a day;
  \item \code{t=2} for a week;
  \item \code{t=3} for 4 weeks;
  \item \code{t=4} for 12 weeks;
  \item \code{t=5} for 24 weeks;
  \item \code{t=6} for 48 weeks;
  \item \code{t=7} for 365 days.
  }
  Default value is (\code{t=4}).}
}
\details{
  NBD Program is based on the original code developed by Prof. Robert East. It requires
  data on penetration, purchase frequency and data collection period. It computes repeat
  purchase and new purchase rates for different periods. The Gamma distribution of
  persons buying different times is worked out for each period and the proportion
  of sales attributable to different rates of purchase is calculated (Negative Binomial
  Distribution). Results also show how penetration and purchase frequency change when
  periods of different duration are used.
}
\value{
  Return a list object.
}
\author{
Daniele Medri
}
\references{
  East, R. and Wright, M. and Vanhuele, M. (2013). Consumer Behaviour: Applications
  in Marketing. SAGE Publications, pp. 72-85.
}
\seealso{

  Ehrenberg, A., Byron S. (2000). Managerial Uses of Descriptive Marketing
  Models-The Case of the Dirichlet. Diss. Griffith University.
  \url{https://www.marketingscience.info/wp-content/uploads/staff/2015/08/6893.pdf}

  Ehrenberg, A., Uncles, M. D. (1999). Understanding Dirichlet-type markets.
  London, Centre for Research in Marketing, South Bank University.

  Goodhardt, G. J., Ehrenberg, A., Chatfield, C. (1984), The Dirichlet: A
  Comprehensive Model of Buying Behaviour. Journal of the Royal Statistical
  Society, 147 (part 5), 621-55.

  Ehrenberg, A. (1959). The Pattern of Consumer Purchases. Journal of the Royal
  Statistical Society. Series C (Applied Statistics), 8(1), pp. 26-41.
}
\examples{
\dontrun{
nbdprogram(b=0.08, w=2, t=2)
}
}
\keyword{ marketing }
