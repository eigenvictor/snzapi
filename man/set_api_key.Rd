% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{set_api_key}
\alias{set_api_key}
\title{Store API key for use in queries}
\usage{
set_api_key(key)
}
\arguments{
\item{key}{an API key generated at https://portal.apis.stats.govt.nz/}
}
\description{
Keep your API key secret. In general, it is safest to not save API keys
directly in your code, rather store them in an external yaml file which is
listed in your .gitignore file.
}
\examples{
set_api_key(config::get("api_key"))

}
