
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

![R-CMD-check](https://github.com/poldham/gnfinderr/workflows/R-CMD-check/badge.svg)
[![Codecov test
coverage](https://codecov.io/gh/poldham/gnfinderr/branch/master/graph/badge.svg)](https://codecov.io/gh/poldham/gnfinderr?branch=master)
<!-- badges: end -->

`gnfindrr` is an experimental R wrapper for the [gnfinder
library](https://github.com/gnames/gnfinder) for scientific name
discovery in texts created by [Dmitry
Mozzherin](https://github.com/dimus).

`gnfinder` forms part of the [Global Names Architecture
(GNA)](https://github.com/gnames) suite of tools for working with
taxonomic names and biodiversity data. `gnfinder` is presently available
as a command line tool and can also be used as a gRPC service, a library
or in a docker container. `gnfinder` is used to text mine the
Biodiversity Heritage Library for taxonomic names at the scale of
millions of pages.

`gnfinderr` has the modest ambition of providing a thin wrapper in R
with a focus on text mining smaller data frames and returning data
frames to the user. `gnfinderr` uses the tidyverse and includes the pipe
`%>%`.

`gnfinderr` is not intended for use on large datasets (as it will take a
very long time). For that use `gnfinder` directly. What `gnfinderr` is
useful for is smaller datasets of upto a few thousand texts. This will
give you a good flavour of what you can do with `gnfinder` if you are
interested in scaling up later on.

`gnfinderr` is at an early experimental stage and presently just maps
the main results to a data frame.

## Installation

To work with `gnfinderr` you need to install `gnfinder` as a command
line app for your operating system as described here
<https://github.com/gnames/gnfinder>. These steps are reproduced below.

*Step 1:* Get the latest release for your operating system from here
<https://github.com/gnames/gnfinder/releases>

Make sure that you download the right version for your system to avoid
considerable confusion.

*Step 2:* Linux or OSX

Move `gnfinder` executable somewhere in your PATH (for example
/usr/local/bin)

    sudo mv path_to/gnfinder /usr/local/bin

*Step 2:* Windows

Here you have options.

One possible way would be to create a default folder for executables and
place `gnfinder` there.

Use Windows+R keys combination and type “cmd”. In the appeared terminal
window type:

    mkdir C:\bin
    copy path_to\gnfinder.exe C:\bin

Add C:directory to your PATH environment variable.

*Step 3:* Test run `gnfinder`

In terminal bring up the `gnfinder` page.

    gnfinder

Run a quick query:

    echo "Pomatomus saltator and Parus major" | gnfinder find -c -l eng

You are good to go.

*Step 4:* install `gnfinderr`

`gnfinderr` is not on CRAN. The development version can be installed
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("poldham/gnfinderr")
```

## Example

This is presently a one function package.

``` r
library(gnfinderr)

intro <- gnfinder(string = c("Lepidium meyennii is a hot plant.", "Escherichia coli is not a plant at all"))
```

`gnfinderr` is intended to be informative and to fail fast. The default
search uses the combination of dictionaries and bayes regression that
power gnfinder and will check any names discovered against the Catalogue
of Life (by default)

An id column is generated and mapped to the input texts to facilitate
joins. Alternatively you can provide ids at input. Example datasets of
various sizes are provided to experiment with. Be warned that the
`zootaxa_titles` dataset with 20,000 titles takes a long time to run.
Here we use the small `fivetexts` dataset to pass document ids to join
the results.

Most of the functions from `gnfinder` are available in `gnfinderr`
(except language options and metadata is not returned). By default all
available arguments are TRUE (bayes is on). We can change the setting by
entering a value in the relevant argument.

Turning off check\_names will speed up processing and simplify the
returns:

``` r
library(gnfinderr)

df <- gnfinder(string = five$text, id = five$id, check_names = FALSE)
```

You can also turn off the Bayes regression by entering a value for
`nobayes`:

``` r
df <- gnfinder(string = five$text, id = five$id, nobayes = TRUE, check_names = FALSE)
```

In general name discovery in texts is facilitated by leaving the bayes
setting as is.

When working with gnfinder it is best to be selective in the use of
texts to facilitate matching. Thus, if you include article metadata such
as author names and organisations you may get unexpected false positive
matches.

### Specifying Sources for Check Names

By default `gnfinderr` checks names against the Catalogue of Life
Taxonomy. The results are returned in a set of data.frames under
verification and include a best result.

For a list of available sources see
<https://index.globalnames.org/datasource>. Here we will check against:

  - [The Catalogue of Life](https://www.catalogueoflife.org/) (1),
  - [The Global Biodiversity Information Facility (GBIF) Backbone
    Taxonomy](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c)
    (11)
  - [The Open Tree of Life Reference
    Taxonomy](https://tree.opentreeoflife.org/about/taxonomy-version/ott3.2)
    (179)

We can specify the source we would like to check by supplying a vector
of ids to
`source_ids`.

``` r
df <- gnfinder(string = five$text, id = five$id, source_ids = c(1,11,179))
```

This will return a tibble (data frame) that contains other data frames
under `verification`.

``` r
df$verification
```

The verification table contains bestresult and preferredresults dfs,
where the best result is for the best match and the preferred results
are from the taxonomic services in `source_ids`. These tables include
details of the taxon ids and taxonomic hierarchy.

### Limitations

`gnfinder` works with UTF8 texts and should not be expected to work with
other kinds of texts. If you don’t know whether you have utf8 formatted
text the package should tell you when you enter your texts.

R runs on a single core and so, for the time being, does `gnfinderr`.
Work is planned to explore options for running in parallel. As such,
expect the package to be slow on large numbers of texts. Turn off
`check_names` for a performance boost.

### Future plans

At this early stage the focus is on:

  - Improving the code
  - Allowing a user to return the raw json to file with identifiers
  - Returning the metadata
  - Examining ways to access nested data frames with document
    identifiers
  - Examining options for running in parallel
  - Linking through to R packages such as {taxize} and {rgbif}
  - Following the ROpenSci guide for package submission

Suggestions and comments are welcome on the [Github
Issues](https://github.com/poldham/gnfinderr/issues) page. A [Code of
Conduct](https://github.com/poldham/gnfinderr/blob/master/CODE_OF_CONDUCT.md)
is provided for collaborators.
