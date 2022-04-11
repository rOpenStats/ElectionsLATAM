

test_that("Costa Rica ecological Inference", {
  costa.rica.ein.path <- file.path(getPackageDir(), "costa-rica")
  ecological.inference.calvo <- EcologicalInferenceStrategyCalvoEtAl$new()
  costa.rica.ein <-
    EcologicalInferenceProcessor$new(
      ecological.inference.strategy = ecological.inference.calvo,
      election.name = "2022-costa-rica-general-ballotage-n4",
      scenario = "final",
      data.input.path = costa.rica.ein.path,
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
  costa.rica.ein$output.election <- readr::read_delim(
    #ballotage.processor$pivot.filepath,
    file.path(costa.rica.ein.path, paste("2022-ballotage_pivot_candidatos_n4.csv", sep = "_")),
    delim = ";",
    col_types = cols(
      .default = col_double(),
      id_unidad = col_character()
    )
  )
  costa.rica.ein$runScenario(include.blancos = TRUE, include.ausentes = TRUE,
                             max.potential.votes.rel.dif = 0.2)
  costa.rica.test.path <- file.path(tempdir(), "test","costa-rica")
  costa.rica.ein$exportBetab(output.folder = costa.rica.test.path, overwrite = TRUE)
  dummy <- costa.rica.ein$generateOutputJSON(costa.rica.test.path,
                                             filename = "balotaje_n4_ei.json")

  costa.rica.ein$makeSankeyDiagram(output.path = costa.rica.test.path)
  output.table.expected <-
    read_rds(file.path(costa.rica.ein.path, "ein_2021_general_2022_ballotage.rds"))
  costa.rica.ein$expectCompatible(output.table.expected, tolerance.rel = 0.32)

  # Using potential votes
  costa.rica.ein <-
    EcologicalInferenceProcessor$new(
      ecological.inference.strategy = ecological.inference.calvo,
      election.name = "2022-costa-rica-general-ballotage-n4",
      scenario = "final",
      data.input.path = costa.rica.ein.path,
      input.file = "2021-generales_pivot_candidatos_n4.csv",
      location.fields = c("id_unidad"),
      votes.field = "votos",
      potential.votes.field = "habilitados",
      col.types = cols(
        .default = col_number(),
        id_unidad = col_character()
      )
    )
  dummy <- costa.rica.ein$loadInputPivotCandidatos()
  costa.rica.ein$output.election <- readr::read_delim(
    #ballotage.processor$pivot.filepath,
    file.path(costa.rica.ein.path, paste("2022-ballotage_pivot_candidatos_n4.csv", sep = "_")),
    delim = ";",
    col_types = cols(
      .default = col_double(),
      id_unidad = col_character()
    )
  )
  costa.rica.ein$runScenario(include.blancos = TRUE, include.ausentes = TRUE,
                             max.potential.votes.rel.dif = 0.3)

})

