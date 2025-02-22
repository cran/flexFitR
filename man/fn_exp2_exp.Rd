% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/99_growth_curves.R
\name{fn_exp2_exp}
\alias{fn_exp2_exp}
\title{Exponential exponential Function 2}
\usage{
fn_exp2_exp(t, t1, t2, alpha, beta)
}
\arguments{
\item{t}{Numeric. The time value.}

\item{t1}{Numeric. The lower threshold time. Assumed to be known.}

\item{t2}{Numeric. The upper threshold time.}

\item{alpha}{Numeric. The parameter for the first exponential term.
Must be greater than 0.}

\item{beta}{Numeric. The parameter for the second exponential term.
Must be less than 0.}
}
\value{
A numeric value based on the double exponential model.
If \code{t} is less than \code{t1}, the function returns 0.
If \code{t} is between \code{t1} and \code{t2} (inclusive),
the function returns \code{exp(alpha * (t - t1)^2) - 1}.
If \code{t} is greater than \code{t2}, the function returns
\code{(exp(alpha * (t2 - t1)^2) - 1) * exp(beta * (t - t2))}.
}
\description{
Computes a value based on an exponential growth curve and exponential decay model for time.
}
\details{
\if{html}{
\deqn{
f(t; t_1, t_2, \alpha, \beta) =
\begin{cases}
0 & \text{if } t < t_1 \\
e^{\alpha \cdot (t - t_1)^2} - 1 & \text{if } t_1 \leq t \leq t_2 \\
\left(e^{\alpha \cdot (t_2 - t_1)^2} - 1\right) \cdot e^{\beta \cdot (t - t_2)} & \text{if } t > t_2
\end{cases}
}
}
}
\examples{
library(flexFitR)
plot_fn(
  fn = "fn_exp2_exp",
  params = c(t1 = 35, t2 = 55, alpha = 1 / 600, beta = -1 / 30),
  interval = c(0, 108),
  n_points = 2000,
  auc_label_size = 3,
  y_auc_label = 0.15
)
}
