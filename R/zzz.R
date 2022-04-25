#' ElectionsLATAM
#'
#' A package to work with SQL datasources in a simple manner
#'
#' @docType package
#' @name ElectionsLATAM
#' @import R6 lgr
#' @importFrom utils str
#' @author Alejandro Baranek <abaranek@dc.uba.ar>
#' @examples
#' library(ElectionsLATAM)
#' library(readr)
#' costa.rica.ein.path <- file.path(getPackageDir(), "costa-rica")
#' ecological.inference.wittenberg <- EcologicalInferenceStrategyWittenbergEtAl$new()
#' costa.rica.ein <-
#'   EcologicalInferenceProcessor$new(
#'     ecological.inference.strategy = ecological.inference.wittenberg,
#'     election.name = "2022-costa-rica-general-ballotage-n4",
#'     scenario = "final",
#'     data.input.path = costa.rica.ein.path,
#'     input.file = "2021-generales_pivot_candidatos_n4.csv",
#'     location.fields = c("id_unidad"),
#'     votes.field = "votos",
#'     # potential.votes.field = "habilitados",
#'     ignore.fields = "habilitados",
#'     col.types = cols(
#'       .default = col_number(),
#'       id_unidad = col_character()
#'     )
#'   )
#' dummy <- costa.rica.ein$loadInputPivotCandidatos()
#' costa.rica.ein$output.election <- readr::read_delim(
#'   file.path(
#'     costa.rica.ein.path,
#'     paste("2022-ballotage_pivot_candidatos_n4.csv", sep = "_")
#'   ),
#'   delim = ";",
#'   col_types = cols(
#'     .default = col_double(),
#'     id_unidad = col_character()
#'   )
#' )
#' costa.rica.ein$runScenario(include.blancos = TRUE, include.ausentes = TRUE)
#'
#' @param libname Library name
#' @param pkgname Package name
# execute onload
.onLoad <- function(libname, pkgname) {

}
