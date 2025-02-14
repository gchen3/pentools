% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{get_pvfs}
\alias{get_pvfs}
\title{Calculate Present Value of Future Salaries (PVFS)}
\usage{
get_pvfs(remaining_prob_vec, interest_vec, sal_vec)
}
\arguments{
\item{remaining_prob_vec}{Numeric vector. The remaining survival probabilities for each period.}

\item{interest_vec}{Numeric vector. The interest rates for each period.}

\item{sal_vec}{Numeric vector. The projected salaries for each period.}
}
\value{
Numeric vector. A vector of PVFS values, where each element represents the present value of future salaries starting from that period.
}
\description{
This function computes the present value of future salaries (PVFS) for a given set of probabilities, interest rates, and salary values over a time period.
}
\details{
The present value of future salaries is calculated by:
- Adjusting projected salaries by the survival probability at each period.
- Discounting future salaries to their present value using the interest rates provided.
}
\examples{
# Example inputs
remaining_prob_vec <- c(1, 0.95, 0.90, 0.85)
interest_vec <- c(0.03, 0.03, 0.03, 0.03)
sal_vec <- c(50000, 52000, 54000, 56000)

# Calculate PVFS
get_pvfs(remaining_prob_vec, interest_vec, sal_vec)

}
