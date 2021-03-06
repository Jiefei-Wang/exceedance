% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FDX-parameters.R
\name{param_fast_GW}
\alias{param_fast_GW}
\title{Fast Exceedance control using the GW method}
\usage{
param_fast_GW(
  statistic = c("kth_p", "KS", "HC", "BJ", "Simes"),
  param1 = NULL,
  param2 = NULL,
  range_type = c("index", "proportion")
)
}
\arguments{
\item{statistic}{character, the name of the statistic for the test, see details.}

\item{param1}{integer, the first parameter for the statistic, see details.}

\item{param2}{integer, the second parameter for the statistic, see details.}

\item{range_type}{character, the type of the parameters, see details.}
}
\value{
an exceedance_parameters object
}
\description{
Performing a fast exceedance control of the false discovery proportion(FDP)
under the framework proposed by Genovese, C., & Wasserman, L. (2004),
where FDP is defined by the number of false positives devided by the
number of total rejections.
The GW method requires an uniform(0,1) distributional test statistic
and each value in the input data represents a p-value from a hypothesis.
The sources of the data can be derived from any testing procedure
(e.g. pvalues from testing high-throughput gene data).
This function only supports specific uniform(0,1) distributional tests.
please consider \code{param_general_GW} if you want a generic version of the GW method,
}
\details{
\emph{Note: We will use the term \code{samples} and \code{pvalues} interchangebly to refer
the data gathered by an inference procedure.}

This function perform the fast GW algorithm, currently it supports
the following test statistics:
\itemize{
\item kth_p: The kth pvalue statistic
\item KS: The Kolmogorov-Smirnov statistic
\item HC: The higher criticism statistic
\item BJ: The Berk-Jones statistic
}

For each statistic, you can specify the index of the ascending ordered samples
to control which data will be considered in the test statistics.
For example, the index of the kth pvalue statistic determines the value of \code{k}
and use the kth smallest sample as its statistic.
similarly, the index of KS, HC and BJ determines which ordered samples
will be used to compute the test statistic.

By default, \code{range_type = "index"}, which means \code{param1} and \code{param2} represent
the index. However, the index can also depend on the sample size. Therefore,
if \code{range_type = "proportion"}, The index is determined by the formula
\code{max(floor(n*param),1)}, where \code{n} is the sample size.

For the kth pvalue statistic, \code{param1} determine the value of \code{k} and must be
a single integer or a 0-1 value depending on \code{range_type}.

For KS, HC and BJ, the formula to decide the index is a little bit complicated.
If \code{range_type = "index"}, \code{param1} determines which
small sample(s) will be considered as the evidence of significance.
For example, if \code{param1 = 2} and the second smallest sample is significantly small,
it can lead to a significant result. Conversely,
\code{param2} determines which large sample(s) can be treated as significance.
Both \code{param1} and \code{param2} can be a vector of integer. By default,
if both \code{param1} and \code{param2} are null, it is equivalent to \code{param1 = c(0, 1)},
\code{param2 = NULL} and \code{range_type = "proportion"}.

If \code{range_type = "proportion"}, \code{param1} and \code{param2} can be length 1 vectors,
which will be explained as the index from \code{1} to \code{max(floor(n*param),1)}, or they can
be length 2 vectors, where the index ranges from \code{max(floor(n*param[1]),1)} to
\code{max(floor(n*param[2]),1)}.
}
\examples{
## The 3rd pvalue statistic
param_fast_GW(statistic = "kth_p", param1 = 3)

## One-sided KS statistic
param_fast_GW(statistic = "KS", param1 = c(0,1), range_type = "proportion")

## One-sided KS statistic, 
## Test first 10 smallest pvalues only
param_fast_GW(statistic = "KS", param1 = 1:10, range_type = "index")

}
