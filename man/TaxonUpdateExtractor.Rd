% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taxon_update_extractor.R
\name{TaxonUpdateExtractor}
\alias{TaxonUpdateExtractor}
\title{Single Taxon Update Extractor class}
\value{
a dataframe with backtransformed LHT values
}
\description{
Class that extracts and transform predicted values of LHTs for a single species obtained from FishLife
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TaxonUpdateExtractor-new}{\code{TaxonUpdateExtractor$new()}}
\item \href{#method-TaxonUpdateExtractor-extract_and_backtransform}{\code{TaxonUpdateExtractor$extract_and_backtransform()}}
\item \href{#method-TaxonUpdateExtractor-clone}{\code{TaxonUpdateExtractor$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TaxonUpdateExtractor-new"></a>}}
\if{latex}{\out{\hypertarget{method-TaxonUpdateExtractor-new}{}}}
\subsection{Method \code{new()}}{
Initialise the TaxonPredictionExtractor
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaxonUpdateExtractor$new(
  update_prefix,
  lht_names,
  back_transform_matrix,
  predicted_results
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{update_prefix}}{string to be added as prefix to the column names keeping the obtained new LHT values}

\item{\code{lht_names}}{list of user-defined LHT names associated with their FishLife's counterparts}

\item{\code{back_transform_matrix}}{list of backward-transformation functions to be applied on obtained LHT from FishLife}

\item{\code{predicted_results}}{matrix of predicted results from FishLife}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TaxonUpdateExtractor-extract_and_backtransform"></a>}}
\if{latex}{\out{\hypertarget{method-TaxonUpdateExtractor-extract_and_backtransform}{}}}
\subsection{Method \code{extract_and_backtransform()}}{
Extract and backtransform the updated LHT values from FishLife
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaxonUpdateExtractor$extract_and_backtransform()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TaxonUpdateExtractor-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TaxonUpdateExtractor-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaxonUpdateExtractor$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
