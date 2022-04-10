
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ElectionsLATAM

<!-- Database Agnostic Package to Generate and Process 'SQL' Queries in R. -->

Allows the user to generate and execute select, insert, update and
delete ‘SQL’ queries the underlying database without having to
explicitly write ‘SQL’ code.

| Release                                                                                                          | Usage                                                                                                    | Development                                                                                                                                                                                            |
|:-----------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|                                                                                                                  | [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-blue.svg)](https://cran.r-project.org/) | [![Travis](https://travis-ci.org/rOpenStats/ElectionsLATAM.svg?branch=master)](https://travis-ci.org/rOpenStats/ElectionsLATAM)                                                                        |
| [![CRAN](http://www.r-pkg.org/badges/version/ElectionsLATAM)](https://cran.r-project.org/package=ElectionsLATAM) |                                                                                                          | [![codecov](https://codecov.io/gh/rOpenStats/ElectionsLATAM/branch/master/graph/badge.svg)](https://codecov.io/gh/rOpenStats/ElectionsLATAM)                                                           |
|                                                                                                                  |                                                                                                          | [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) |

# How to get started

``` r
install.packages("ElectionsLATAM")
```

# How to get started (Development version)

Install the R package using the following commands on the R console:

``` r
devtools::install_github("rOpenStats/ElectionsLATAM", build_opts = NULL)
```

# A simple example

To get started execute the following commands:

``` r
# 0.  Load libraries
library(ElectionsLATAM)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(readr)
#> Warning: package 'readr' was built under R version 4.1.2
```

``` r

# 1.  Costa Rica
costa.rica.ein.path <- file.path(getPackageDir(), "costa-rica")
ecological.inference.calvo <- EcologicalInferenceStrategyCalvo$new()
costa.rica.ein <-
    EcologicalInferenceProcessor$new(
      ecological.inference.strategy = ecological.inference.calvo,
      election.name = "2022-costa-rica-general-ballotage-n4",
      scenario = "final",
      data.input.path = "~/.R/GOelecciones/data/ein/costa-rica",
      input.file = "2021-generales_pivot_candidatos_n4.csv",
      location.fields = c("id_unidad"),
      votes.field = "votos",
      #potential.votes.field = "habilitados",
      ignore.fields = "habilitados",
      col.types = cols(
        .default = col_number(),
        id_unidad = col_character()
      )
    )
dummy <- costa.rica.ein$loadInputPivotCandidatos()
#> INFO  [15:50:16.461] Loading input election {input.filepath: `~/.R/GOelecciones/data/ein/costa-rica/2021-generales_pivot_candidatos_n4.csv`}
costa.rica.ein$output.election <- readr::read_delim(
    #ballotage.processor$pivot.filepath,
    file.path(costa.rica.ein.path,
              paste("2022-ballotage_pivot_candidatos_n4.csv", sep = "_")),
    delim = ";",
    col_types = cols(
      .default = col_double(),
      id_unidad = col_character()
    )
  )
costa.rica.ein$runScenario(include.blancos = TRUE, include.ausentes = TRUE)
#> INFO  [15:50:16.658] Setting seed {seed: `143324`}
#> INFO  [15:50:16.713] Starting with {input.locations: `6661`, output.locations: `6738`, locations.available: `6661`}
#> INFO  [15:50:17.057] After filtering locations {input.locations: `6661`, output.locations: `6661`}
#> INFO  [15:50:17.155] ParamsEstim {nR: `10`, nC: `5`}
#> INFO  [15:50:24.452] calcFractions 
#> INFO  [15:50:24.460] Results for {description: `input.original`, results: `FA= 3.24|habilitados= 62.25|Otros= 2.81|PLN= 10.15|PLP= 4.6|PNR= 5.56|PSD= 6.25|PUSC= 4.62|blanco_y_nulo= 0.52`, total.votes: `2083243`}
#> INFO  [15:50:24.471] Results for {description: `input`, results: `FA= 8.54|Otros= 7.41|PLN= 26.8|PLP= 12.14|PNR= 14.66|PSD= 16.5|PUSC= 12.2|blanco_y_nulo= 1.38|ausente= 0.37`, total.votes: `2083243`}
#> INFO  [15:50:24.474] Results for {description: `output.original`, results: `habilitados= 63.79|PLN= 16.63|PSD= 18.64|blanco_y_nulo= 0.93`, total.votes: `1978230`}
#> INFO  [15:50:24.477] Results for {description: `output`, results: `PLN= 43.31|PSD= 48.6|blanco_y_nulo= 2.43|ausente= 5.65`, total.votes: `1972818`}
#> INFO  [15:50:24.478] Votes {total.input.votes: `2083243`, total.input.applied.votes: `2083243`, total.output.votes: `1978230`, total.output.applied.votes: `1972818`, change.input.output.votes: `0.9496`, change.input.output.applied.votes: `0.947`, dismissed.input.votes: `1`, dismissed.output.votes: `0.9973`}
#>                         PLN    PSD blanco_y_nulo ausente 1 - rowSums(dsOUTpre)
#> FA                    83306  73882         16847    1568                  3056
#> Otros                 13331 117779         11154   10915                  1737
#> PLN                  559594    314            48      23                   395
#> PLP                  154011  66339         10382   21407                  1703
#> PNR                   37250 218892           125   49630                   688
#> PSD                      29 344873            17      20                    92
#> PUSC                  46570 181029          2745   22488                  2181
#> blanco_y_nulo          5959   1473          5581   13578                  2233
#> ausente                2111   5572            11       2                    41
#> 1 - rowSums(dsINpre)  -4292   -585         -1210    -642                 -1008
costa.rica.test.path <- file.path(tempdir(), "test","costa-rica")
costa.rica.ein$exportBetab(output.folder = costa.rica.test.path, overwrite = TRUE)
#> INFO  [15:50:24.492] Ecological inference Betab file writen {betab.filepath: `/var/folders/4r/f_k7yqz92p76h7b32m953pyr0000gp/T//Rtmp9c92pS/test/costa-rica/2022-costa-rica-general-ballotage-n4-ein-betab-scen-final-s-143324.csv`}
dummy <- costa.rica.ein$generateOutputJSON(costa.rica.test.path,
                                           filename = "balotaje_n4_ei.json")
#> INFO  [15:50:24.519] Ecological inference json writen {json.filepath: `/var/folders/4r/f_k7yqz92p76h7b32m953pyr0000gp/T//Rtmp9c92pS/test/costa-rica/balotaje_n4_ei.json`}

costa.rica.ein$makeSankeyDiagram(output.path = costa.rica.test.path)
#> INFO  [15:50:24.528] Generating sankeyNetwork {nodes: `13`, links: `36`}
#> INFO  [15:50:24.769] Generating webshot {sankey.d3.png.filepath: `/var/folders/4r/f_k7yqz92p76h7b32m953pyr0000gp/T//Rtmp9c92pS/test/costa-rica/2022-costa-rica-general-ballotage-n4-ein-sankey-scen-final-s-143324.png`}
```

<img src="man/figures/README-costa-rica-1.png" width="100%" />

``` r
# Saving output table for reproducibility
#  write_rds(costa.rica.ein$output.table, file.path(costa.rica.ein.path, "ein_2021_general_2022_ballotage.rds"))
```

# Troubleshooting

Please note that the ‘ElectionsLATAM’ project is released with a
[Contributor Code of
Conduct](https://github.com/rOpenStats/ElectionsLATAM/blob/master/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.
