% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download-cvm-data.R
\name{download_cvm_data}
\alias{download_cvm_data}
\title{Download data from the Securities and Exchange Commission of Brazil (CVM)}
\usage{
download_cvm_data(config)
}
\arguments{
\item{config}{A list of lists containing the CVM data arguments. They are \code{asset} (asset
name) and \code{cnpj} (fund ID, as per daily report api).}
}
\value{
A data frame containing the following columns:
\itemize{
\item asset
\item date
\item return
}

and arranged by asset and date.
}
\description{
Download fund data from the Securities and Exchange Commission of Brazil
(CVM).
}
\details{
This function downloads the data directly from the \href{http://dados.cvm.gov.br/dataset/fi-doc-inf_diario}{daily report api}.
}
