#' EcologicalInferenceProcessor
#' @examples
#' library(ElectionsLATAM)
#' library(readr)
#' costa.rica.ein.path <- file.path(getPackageDir(), "costa-rica")
#' ecological.inference.calvo <- EcologicalInferenceStrategyCalvoEtAl$new()
#'costa.rica.ein <-
#'  EcologicalInferenceProcessor$new(
#'    ecological.inference.strategy = ecological.inference.calvo,
#'    election.name = "2022-costa-rica-general-ballotage-n4",
#'    scenario = "final",
#'    data.input.path = costa.rica.ein.path,
#'    input.file = "2021-generales_pivot_candidatos_n4.csv",
#'    location.fields = c("id_unidad"),
#'    votes.field = "votos",
    #potential.votes.field = "habilitados",
#'    ignore.fields = "habilitados",
#'    col.types = cols(
#'      .default = col_number(),
#'      id_unidad = col_character()
#'    )
#'  )
#' dummy <- costa.rica.ein$loadInputPivotCandidatos()
#'costa.rica.ein$output.election <- readr::read_delim(
#'  file.path(costa.rica.ein.path,
#'            paste("2022-ballotage_pivot_candidatos_n4.csv", sep = "_")),
#'  delim = ";",
#'  col_types = cols(
#'    .default = col_double(),
#'    id_unidad = col_character()
#'  )
#' )
#'costa.rica.ein$runScenario(include.blancos = TRUE, include.ausentes = TRUE)
#'
#' @importFrom R6 R6Class
#' @import dplyr
#' @import magrittr
#' @author ken4rab
#' @export
EcologicalInferenceProcessor <- R6Class("EcologicalInferenceProcessor",
public = list(
  election.name = NA,
  ein.strategy = NA,
  data.input.path = NA,
  input.filename = NA,
  scenario = NA,
  input.election.original = NULL,
  output.election.original = NULL,
  input.election = NA,
  output.election = NA,
  location.fields = NA,
  votes.field = NA,
  potential.votes.field = NULL,
  ignore.fields = NULL,
  col.types = NA,
  parties.mapping = NA,
  reverse.mapping = NA,
  data.folder.d3 = NA,
  seed = NA,
  # filter
  locations.available = NULL,
  # output
  output.table = NA,
  output = NA,
  sankey.network = NA,
  sync.result = NA,
  logger = NA,
  initialize = function(election.name,
                        ecological.inference.strategy,
                        data.input.path,
                        input.filename,
                        scenario,
                        location.fields,
                        votes.field,
                        parties.mapping = NULL,
                        reverse.mapping = TRUE,
                        potential.votes.field = NULL,
                        ignore.fields = NULL,
                        col.types = cols(.default = col_number()),
                        seed = 143324) {
    self$election.name   <- election.name
    self$ein.strategy    <- ecological.inference.strategy
    self$data.input.path <- data.input.path
    self$input.filename  <- input.filename
    self$scenario        <- scenario
    self$location.fields <- location.fields
    self$votes.field     <- votes.field
    self$potential.votes.field <- potential.votes.field
    self$ignore.fields   <- ignore.fields
    self$col.types       <- col.types
    self$seed            <- seed
    self$data.folder.d3 <- file.path(data.input.path, "d3")
    dir.create(self$data.folder.d3, showWarnings = FALSE, recursive = TRUE)
    self$parties.mapping <- parties.mapping
    self$reverse.mapping <- reverse.mapping
    self$logger <- genLogger(self)
    self
  },
  wallTime = function(expr) {
    system.time(expr)[3]
  },
  generateOutput = function() {
    ret <- list()
    col.names <- colnames(self$output.table)
    row.names <- rownames(self$output.table)
    n <- nrow(self$output.table)
    m <- ncol(self$output.table)
    for (j in seq_len(m - 1)) {
      output.party <- col.names[j]
      output.party.values <- list()
      for (i in seq_len(n - 1)) {
        input.party <- row.names[i]
        output.party.values[[input.party]] <- self$output.table[i, j]
      }
      output.party.values[["Total"]] <- sum(self$output.table[seq_len(n - 1), j])
      ret[[output.party]] <- output.party.values
    }
    self$output <- ret
  },
  loadInputPivotCandidatos = function(input.filename = self$input.filename) {
    logger <- getLogger(self)
    input.filepath <- file.path(self$data.input.path, input.filename)
    logger$info("Loading input election",
                input.filepath = input.filepath
    )
    self$input.election <- loadPivotInput(input.filepath = input.filepath, col_types = self$col.types)
    self$input.election
  },
  generateNormalizedOutput = function(indicator = "perc") {
    allowed.indicators <- c("perc", "count")
    stopifnot(indicator %in% allowed.indicators)
    ret <- data.frame(source = character(), target = character(), value = numeric())
    input.names <- rownames(self$output.table)
    input.names <- input.names[seq_len(length(input.names) - 1)]
    output.names <- colnames(self$output.table)
    output.names <- output.names[seq_len(length(output.names) - 1)]
    rows <- seq_len(length(input.names))
    cols <- seq_len(length(output.names))
    total.votes <- sum(self$output.table[rows, cols])
    c <- 1
    for (i in rows) {
      for (j in cols) {
        value <- self$output.table[i, j]
        if (indicator == "perc") {
          value <- round(value / total.votes * 100, 2)
        }
        ret[c, ] <- c(input.names[i], output.names[j], value)
        c <- c + 1
      }
    }
    ret
  },
  makeSankeyDiagram = function(output.path = NULL) {
    logger <- getLogger(self)
    # A connection data frame is a list of flows with intensity for each flow
    links <- self$generateNormalizedOutput(indicator = "perc")
    nodes <- data.frame(
      name = c(
        as.character(links$source),
        as.character(links$target)
      ) %>% unique()
    )

    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes <- data.frame(cat = "s", name = unique(links$source))
    nodes <- rbind(nodes, data.frame(cat = "t", name = unique(links$target)))
    nodes %<>% mutate(id = paste(cat, name, sep = "_"))

    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    links$IDsource <- match(paste("s", links$source, sep = "_"), nodes$id) - 1
    links$IDtarget <- match(paste("t", links$target, sep = "_"), nodes$id) - 1
    logger$info("Generating sankeyNetwork", nodes = nrow(nodes), links = nrow(links))
    sankey.network <- sankeyNetwork(
      Links = links, Nodes = nodes,
      Source = "IDsource", Target = "IDtarget",
      Value = "value", NodeID = "name",
      iterations = 3,
      sinksRight = TRUE,
      fontSize = 18
    )
    self$sankey.network <- sankey.network
    if (!is.null(output.path)) {
      tmp.path <- file.path(tempdir(), "sankey", "d3")
      dir.create(tmp.path, showWarnings = FALSE, recursive = TRUE)
      tmp.html.filepath <- file.path(tmp.path, "sn.html")
      saveNetwork(sankey.network, tmp.html.filepath)

      sankey.d3.png.filepath <- file.path(
        output.path,
        self$getFilename(paste(self$election.name, "-ein-sankey", sep = ""), extension = "png")
      )
      write_delim(links,
                  file = gsub("\\.png", ".csv", sankey.d3.png.filepath),
                  delim = ";"
      )
      # you convert it as png
      file.exists(tmp.html.filepath)

      sankey.d3.html.filepath <- gsub("\\.png", ".html", sankey.d3.png.filepath)
      file.copy(tmp.html.filepath, sankey.d3.html.filepath)
      logger$info("Generating webshot",
                  sankey.d3.png.filepath = sankey.d3.png.filepath
      )
      webshot::webshot(tmp.html.filepath,
                       sankey.d3.png.filepath,
                       vwidth = 1000, vheight = 900
      )
    }
    sankey.network
  },
  getFilename = function(file.prefix, extension = "csv") {
    paste(file.prefix, "-scen-", self$scenario, "-s-", self$seed, ".", extension, sep = "")
  },
  generateOutputJSON = function(output.path = NULL,
                                filename = self$getFilename(paste(self$election.name, "-ein", sep = ""), extension = "json")) {
    logger <- getLogger(self)
    self$generateOutput()
    eir.json <- jsonlite::toJSON(self$output, pretty = TRUE)
    eir.json <- gsub("\\[|\\]", "", eir.json)
    eir.json <- gsub("\\\"|\\\"", "", eir.json)
    eir.json <- paste("var poll = ",
                      eir.json,
                      ";\n",
                      sep = ""
    )
    if (!is.null(output.path)){
      json.filepath <- file.path(
        output.path,
        filename
      )
      write_file(x = eir.json, file = json.filepath, append = FALSE)
      logger$info("Ecological inference json writen", json.filepath = json.filepath)
    }
    eir.json
  },
  getBetab = function() {
    betab <- self$output.table[
      seq_len(nrow(self$output.table) - 1),
      seq_len(ncol(self$output.table) - 1)
    ]
    betab <- as.data.frame(betab)
    total.cols <- ncol(betab)
    cols <- seq_len(total.cols)
    for (j in cols) {
      betab[, total.cols + j] <- 0
      names(betab)[total.cols + j] <- paste(names(betab)[j], "perc", sep = "_")
    }
    betab
    for (i in seq_len(nrow(betab))) {
      betab[i, total.cols + cols] <- round(betab[i, cols] / sum(betab[i, cols]), 3)
    }
    betab$source <- rownames(betab)
    betab$scenario <- self$scenario
    betab <- betab[, c("scenario", "source", names(betab)[seq_len(2 * total.cols)])]
    betab
  },
  exportBetab = function(output.folder = NULL, overwrite = FALSE) {
    logger <- getLogger(self)
    betab <- self$getBetab()
    if (!is.null(output.folder)){
      dir.create(output.folder, showWarnings = FALSE, recursive = TRUE)
      betab.filepath <- file.path(
        output.folder,
        self$getFilename(paste(self$election.name, "-ein-betab", sep = ""), extension = "csv")
      )
      logger$info("Ecological inference Betab file writen",
                  betab.filepath = betab.filepath
      )
      if (!file.exists(betab.filepath) | overwrite) {
        write_delim(betab, file = betab.filepath, delim = ";")
      }
    }
  },
  convertShares2Votes = function(election.df) {
    share.fields <- self$getSharesFields(names(election.df))
    rows.sums <- apply(election.df[, share.fields], MARGIN = 1, FUN = sum)
    rows.sums.min <- min(rows.sums, na.rm = TRUE)
    rows.sums.max <- max(rows.sums, na.rm = TRUE)
    if (rows.sums.max <= 1.01 & rows.sums.max / rows.sums.min < 1.01) {
      for (share.field in share.fields) {
        election.df[, share.field] <- election.df[, share.field] * election.df[, self$votes.field]
      }
    }
    election.df
  },
  convertVotes2Shares = function(election.df) {
    share.fields <- self$getSharesFields(names(election.df))
    total.votes <- rowSums(election.df[, share.fields])
    votes.rows <- which(total.votes > 0)
    for (share.field in share.fields) {
      election.df[votes.rows, share.field] <- election.df[votes.rows, share.field] / total.votes[votes.rows]
      if (length(votes.rows) < nrow(election.df)) {
        # Empty votes
        # Do nothing for empty votes
      }
    }
    election.df
  },
  getSharesFields = function(election.fields) {
    share.fields <- election.fields
    share.fields <- setdiff(share.fields, c(self$location.fields, self$votes.field, self$potential.votes.field))
    not.candidate.fields <- c("blanco_y_nulo", "ausente", "ausente-1", "ausente-2")
    candidate.fields <- setdiff(share.fields, not.candidate.fields)
    not.candidate.fields <- sort(intersect(not.candidate.fields, share.fields), decreasing = TRUE)
    ret <- c(sort(candidate.fields), not.candidate.fields)
    ret
  },
  fixLocationsAvailable = function() {
    logger <- getLogger(self)
    if (is.null(self$locations.available)) {
      self$locations.available <- self$input.election[, self$location.fields]
    }
    if (!is.null(self$locations.available)) {
      logger$info("Starting with",
                  input.locations = nrow(self$input.election),
                  output.locations = nrow(self$output.election),
                  locations.available = nrow(self$locations.available)
      )
      self$input.election %<>% arrange(across(self$location.fields))
      repeated.locations <- self$output.election %>%
        group_by_at(vars(self$location.fields)) %>%
        summarize(n = n()) %>%
        filter(n > 1)
      if (nrow(repeated.locations) > 0) {
        logger$error("Repeated locations",
                     nrow = nrow(repeated.locations),
                     repeated.locations = paste(names(repeated.locations), repeated.locations[1, ], sep = "= ", collapse = "|")
        )
      }
      self$output.election %<>% arrange(across(self$location.fields))
      self$locations.available %<>% arrange(across(self$location.fields))
      self$input.election %<>% inner_join(self$locations.available,
                                          by = self$location.fields
      )
      self$output.election %<>% inner_join(self$locations.available,
                                           by = self$location.fields
      )
      logger$info("After filtering locations",
                  input.locations = nrow(self$input.election),
                  output.locations = nrow(self$output.election)
      )
    }

    input.diff.output <- setdiff(
      self$input.election[, self$location.fields],
      self$output.election[, self$location.fields]
    )
    output.diff.input <- setdiff(
      self$output.election[, self$location.fields],
      self$input.election[, self$location.fields]
    )
    stopifnot(nrow(input.diff.output) == 0)
    stopifnot(nrow(output.diff.input) == 0)
    self
  },
  fixEmpty = function() {
    logger <- getLogger(self)
    empty.input.rows <- which(self$input.election[, self$votes.field] == 0)
    empty.output.rows <- which(self$output.election[, self$votes.field] == 0)
    empty.rows <- sort(c(empty.input.rows, empty.output.rows))
    if (length(empty.rows) > 0) {
      logger$warn("Removing locations without votes", empty.locations = length(empty.rows))
      for (empty.input.row in empty.input.rows) {
        location.id <- paste(self$input.election[empty.input.row, self$location.fields], collapse = "|")
        absents <- 0
        if ("ausente" %in% names(self$input.election)) {
          absents <- as.numeric(self$input.election[empty.input.row, "ausente"])
        }
        logger$warn("Input location without votes",
                    location.id = location.id,
                    absents = absents
        )
      }
      for (empty.output.row in empty.output.rows) {
        location.id <- paste(self$output.election[empty.output.row, self$location.fields], collapse = "|")
        absents <- 0
        if ("ausente" %in% names(self$output.election)) {
          absents <- as.numeric(self$output.election[empty.output.row, "ausente"])
        }
        logger$warn("Output location without votes",
                    location.id = location.id,
                    absents = absents
        )
      }
      self$input.election <- self$input.election[-empty.rows, ]
      self$output.election <- self$output.election[-empty.rows, ]
      self$locations.available <- self$locations.available[-empty.rows, ]
      logger$info("After filtering empty votes",
                  input.locations = nrow(self$input.election),
                  output.locations = nrow(self$output.election),
                  locations.available = nrow(self$locations.available)
      )
    }
  },
  runScenario = function(include.blancos = TRUE, include.ausentes = TRUE) {
    logger <- getLogger(self)
    logger$info("Setting seed", seed = self$seed)
    set.seed(self$seed)
    if (is.null(self$input.election.original)) {
      self$input.election.original <- self$input.election
    }
    if (is.null(self$output.election.original)) {
      self$output.election.original <- self$output.election
    }
    input.votes.col <- which(self$votes.field == names(self$input.election))
    output.votes.col <- which(self$votes.field == names(self$output.election))
    self$input.election[, self$ignore.fields] <- NULL
    self$output.election[, self$ignore.fields] <- NULL
    self$input.election <- self$convertShares2Votes(election.df = self$input.election)
    self$output.election <- self$convertShares2Votes(election.df = self$output.election)
    self$fixLocationsAvailable()
    if (include.ausentes) {
      input.ausente.col <- input.votes.col
      input.votes.col <- ncol(self$input.election) + 1
      self$input.election[, input.votes.col] <- self$input.election[, input.ausente.col]
      self$input.election[, input.ausente.col] <- 0
      names(self$input.election)[c(input.ausente.col, input.votes.col)] <- c("ausente", "votos")
      output.ausente.col <- output.votes.col
      output.votes.col <- ncol(self$output.election) + 1
      self$output.election[, output.votes.col] <- self$output.election[, output.ausente.col]
      self$output.election[, output.ausente.col] <- 0
      names(self$output.election)[c(output.ausente.col, output.votes.col)] <- c("ausente", "votos")
      total.votes.df <- cbind(self$input.election[, input.votes.col], self$output.election[, output.votes.col])
      names(total.votes.df) <- c("input.votes", "output.votes")
      total.votes.df[, "max.votes"] <- apply(total.votes.df, MARGIN = 1, FUN = max)
      total.votes.df <- as.data.frame(total.votes.df %>% select(max.votes))
      if (!is.null(self$potential.votes.field)) {
        stopifnot(self$potential.votes.field %in% names(self$input.election))
        stopifnot(self$potential.votes.field %in% names(self$output.election))
        total.votes.input.df <- self$input.election[, self$potential.votes.field]
        total.votes.output.df <- self$output.election[, self$potential.votes.field]
      } else {
        total.votes.input.df <- total.votes.df
        total.votes.output.df <- total.votes.df
      }
      self$input.election[, input.ausente.col] <- total.votes.input.df[, 1] - self$input.election[, input.votes.col]
      self$output.election[, output.ausente.col] <- total.votes.output.df[, 1] - self$output.election[, output.votes.col]
      # which(self$output.election$Departamento ==  "RO" &
      #         self$output.election$Serie == "ECE")
      # self$output.election %>% filter(Departamento == "RO" & Serie == "ECE")
      # self$output.election[281,]
      # self$input.election[281,]
    }

    self$fixEmpty()


    # input.shares.fields <- names(self$input.election)[2:input.ausente.col]
    # output.shares.fields <- names(self$output.election)[2:output.ausente.col]
    input.shares.fields <- self$getSharesFields(election.fields = names(self$input.election))
    output.shares.fields <- self$getSharesFields(election.fields = names(self$output.election))
    if (!include.blancos) {
      input.shares.fields <- input.shares.fields[input.shares.fields != "blanco_y_nulo"]
      output.shares.fields <- output.shares.fields[output.shares.fields != "blanco_y_nulo"]
    }
    self$ein.strategy$setProcessor(self)
    self$ein.strategy$runEcologicalInference(input.shares.fields,
                                             output.shares.fields)
    self$output.table
  },
  showResultsSummary = function(election.df, description) {
    logger <- getLogger(self)
    input.share.fields <- self$getSharesFields(names(election.df))
    results <- NULL
    for (share.field in input.share.fields) {
      results[share.field] <- sum(election.df[, share.field], na.rm = TRUE)
    }
    total.votes <- sum(election.df[, self$votes.field], na.rm = TRUE)
    results <- round(results / sum(results) * 100, 2)
    logger$info("Results for",
                description = description,
                results = paste(names(results), results, sep = "= ", collapse = "|"),
                total.votes = total.votes
    )
  }
)
)

#' loadPivotInput
#' @examples
#' loadPivotInput(file.path(costa.rica.ein.path, "2021-generales_pivot_candidatos_n4.csv"))
#' @author ken4rab
#' @import readr
#' @import readxl
#' @export
loadPivotInput <- function(input.filepath, col_types = cols(.default = col_number())) {
  input.election <- NULL
  if (grepl("\\.csv", input.filepath)) {
    input.election <-
      read_delim(
        input.filepath,
        delim = ";",
        col_types = col_types
      )
  }
  if (grepl("\\.xlsx?", input.filepath)) {
    input.election <-
      readxl::read_excel(
        input.filepath
      )
  }
  stopifnot(!is.null(input.election))
  input.election
}

#' EcologicalInferenceStrategy
#' @examples
#' ein <- EcologicalInferenceStrategy$new()
#' # Cannot execute an abstract class
#' # ein$runEcologicalInference(NULL, NULL)
#' @author ken4rab
#' @export
EcologicalInferenceStrategy <- R6Class("EcologicalInferenceStrategy",
public = list(
  #' @field seed for initializing random generator
  seed   = NA,
  #' @field processor eir processor
  processor = NULL,
  #' @field logger lgr configured for class
  logger = NA,
initialize = function(seed = 143324) {
  self$logger <- genLogger(self)
},
setProcessor = function(processor){
  self$processor <- processor
  self$processor
},
runEcologicalInference = function(input.shares.fields,
                                  output.shares.fields){
  stop("Abstract class")
}
))

#' EcologicalInferenceStrategyCalvoEtAl
#' @examples
#' ein <- EcologicalInferenceStrategyCalvoEtAl$new()
#' # Cannot run without having a processor
#' #ein$runEcologicalInference(NULL, NULL)
#' @import foreign
#' @import boot
#' @import networkD3
#' @import webshot
#' @author ecalvo
#' @export
EcologicalInferenceStrategyCalvoEtAl <- R6Class("EcologicalInferenceStrategyCalvoEtAl",
inherit = EcologicalInferenceStrategy,
 public = list(
   #' @field estsPG
   estsPG = NA,
   #' @field fracsPG
   fracsPG = NA,
   initialize = function(seed = 143324) {
     super$initialize(seed = seed)
   },
   #' @description
   #' CALL.DIFP
   #' Calculates penalty for given parameters
   #' @param p     - parameter vector R x (C-1)
   #' @param mx    - Column marginals
   #' @param my    - row marginals
   #' @param nR    - number of rows
   #' @param nC    - number of columns
   #' @param nP    - number of precincts
   #' @param const - weight for penalty
   callDifp = function(p, mx, my, covar, nR, nC, nP, const) {
     pen <- 0
     d <- seq(from = 0, to = 0, length = nR * (nC - 1))
     g <- p[1:(nR * (nC - 1))]
     if (is.numeric(covar)) {
       d <- p[(nR * (nC - 1) + 1):(2 * nR * (nC - 1))]
       gamma <- array(0, c(nR, nC - 1, nP))
       diff <- 0
       for (i in 1:nP) {
         temp <- 0
         if (is.numeric(covar)) temp <- covar[i]
         gamma[, , i] <- matrix(data = g + temp * d, nrow = nR, ncol = nC - 1, byrow = T)
         expo <- exp(gamma[, , i])
         if (nC != 2) {
           ebeta <- exp(gamma[, , i]) / (1 + apply(exp(gamma[, , i]), 1, sum))
         } else {
           ebeta <- exp(gamma[, , i]) / (1 + exp(gamma[, , i]))
         }
         yhat <- mx[i, ] %*% ebeta
         diff <- diff + sum((yhat - my[i, -C])^2)
         # diff <- diff + sum((yhat-my[i,-nC])^2) + (const*sum(gamma[,,i]^2));
         # diff <- diff + sum((yhat-my[i,-C])^2) + (10000*sum(gamma[,,i]^2));
       }
     } else {
       # debug
       # print(paste(nR, nC))
       gamma <- matrix(data = g, nrow = nR, ncol = nC - 1, byrow = T)
       expo <- exp(gamma)
       ebeta <- exp(gamma) / (1 + apply(exp(gamma), 1, sum))
       yhat <- mx %*% ebeta
       diff <- sum((yhat - my[, -nC])^2) + (const * sum(gamma^2))
       # is.numeric(my[483,])
     }
     return(diff)
   },
   #' @description
   #' Ecological Inference in the RxC case
   #' Penalized Least Square Minimizer
   #' PARAMS.ESTIM
   #' Estimates parameters minimizing the penalized least squares criterion
   #' @param data    - marginals (optionally with covariates)
   #' @param x       - index (optional, for bootstrapping)
   #' @param nR      - number of rows
   #' @param nC      - number of columns
   #' @param const   - weight for penalty
   #' @param parSeed - Seed for parameters (optional)
   paramsEstim = function(data, x = -1, nR, nC, const = 0.001, parSeed = -1) {
     if (x[1] == -1) x <- 1:nrow(data)

     mx <- data[x, 1:nR]
     my <- data[x, (nR + 1):(nR + nC)]
     nP <- nrow(data)
     covar <- F
     if (ncol(data) > nR + nC) {
       covar <- data[x, nR + nC + 1]
       if (parSeed[1] == -1) {
         parSeed <- rnorm(2 * nR * (nC - 1))
       }
     } else {
       if (parSeed[1] == -1) {
         parSeed <- rnorm(nR * (nC - 1))
       }
     }
     fit <- optim(parSeed, fn = self$callDifp, method = "L-BFGS-B", covar = covar, nR = nR, nC = nC, nP = nP, mx = mx, my = my, const = const)
     # , method="L-BFGS-B", method="SANN"
     return(fit$par)
   },
   #' @description
   #' Calculate Fractions
   #' CALC.FRACTIONS
   #' Calculate fractions from the parameters
   #' @param p     - parameters
   #' @param nR    - number of rows
   #' @param nC    - number of columns
   #' @param covar - (Optional) Vector of covariates
   calcFractions = function(p, nR, nC, covar = F) {
     d <- seq(from = 0, to = 0, length = nR * (nC - 1))
     g <- p[1:(nR * (nC - 1))]
     if (is.numeric(covar)) {
       nP <- length(covar)
       ests <- array(0, c(nR, nC, nP))
       d <- p[(nR * (nC - 1) + 1):(2 * nR * (nC - 1))]
       for (i in 1:nP) {
         p.exp <- exp(g + d * covar[i])
         p.matrix <- matrix(p.exp, nrow = nR, byrow = T)
         p.sums <- apply(p.matrix, 1, sum)
         p.sums <- p.sums + 1
         p.less <- p.matrix / p.sums
         ests[, , i] <- cbind(p.less, 1 - apply(p.less, 1, sum))
       }
     } else {
       p.exp <- exp(g)
       p.matrix <- matrix(p.exp, nrow = nR, byrow = T)
       p.sums <- apply(p.matrix, 1, sum)
       p.sums <- p.sums + 1
       p.less <- p.matrix / p.sums
       ests <- cbind(p.less, 1 - apply(p.less, 1, sum))
     }
     return(ests)
   },
   #' @description
   #' Bootstrapping
   #' PARAMS.BOOT
   #' @param data        - marginals (optionally, with covariates)
   #' @param nR          - number of rows
   #' @param nC          - number of columns
   #' @param bootSamples - number of bootstrap samples
   paramsBoot = function(data, nR, nC, bootSamples) {
     output <- boot(data = data, statistic = self$paramsEstim, R = bootSamples, nR = nR, nC = nC)
     return(output)
   },
   #' @description
   #' runEcologicalInference
   #' run ecological inference with current strategy
   #' @param input.shares.fields
   #' @param output.shares.fields
   runEcologicalInference = function(input.shares.fields,
                                     output.shares.fields){
     logger <- getLogger(self)
     processor <- self$processor
     stopifnot(!is.null(processor))

     dsINpre.zones <- self$input.election[, self$location.fields]
     # input
     dsINpre <- processor$input.election[, input.shares.fields]
     dsINpre <- processor$convertVotes2Shares(dsINpre)
     dsINpre <- cbind(dsINpre, 1 - rowSums(dsINpre))
     colnames(dsINpre)
     dsINpre <- as.matrix(dsINpre)
     # dsINpre <<- dsINpre
     # Emtpy rows
     input.check.col <- ncol(dsINpre)
     which(dsINpre[, input.check.col] == 1)
     empty.rows <- which(processor$input.election[, processor$votes.field] == 0)
     dsINpre[which(processor$input.election[, processor$votes.field] == 0), ]

     # output
     dsOUTpre <- processor$output.election[, output.shares.fields]
     dsOUTpre <- processor$convertVotes2Shares(dsOUTpre)
     dsOUTpre <- cbind(dsOUTpre, 1 - rowSums(dsOUTpre))
     # dsOUTpre %<>% filter(COD_ZONA %in% input.election$COD_ZONA)
     colnames(dsOUTpre)
     dsOUTpre <- as.matrix(dsOUTpre)

     if (length(empty.rows) > 0) {
       locations.empty <- apply(processor$input.election[empty.rows, processor$location.fields], MARGIN = 1, FUN = function(x) paste(x, collapse = "-"))
       logger$warn("Removing rows with no votes",
                   count = length(locations.empty),
                   locations = paste(locations.empty, collapse = ", ")
       )
       dsINpre <- dsINpre[-empty.rows, ]
       dsOUTpre <- dsOUTpre[-empty.rows, ]
     }
     output.check.col <- ncol(dsOUTpre)
     which(dsOUTpre[, output.check.col] == 1)

     nR <- ncol(dsINpre)
     nC <- ncol(dsOUTpre)


     # dsINpre.empty.rows <- apply(dsINpre, MARGIN = 1, FUN = function(x) max(is.na(x)))
     # stopifnot(length(dsINpre.empty.rows[dsINpre.empty.rows != 0]) == 0)
     #
     # dsOUTpre.empty.rows <- apply(dsOUTpre, MARGIN = 1, FUN = function(x) max(is.na(x)))
     # stopifnot(length(which(dsOUTpre.empty.rows != 0)) == 0)
     # dsOUTpre[which(dsOUTpre.empty.rows != 0), ]

     # General a Ballotage
     logger$debug("newdata = dsINpre+dsOUTpre",
                  nrow.dsINpre = nrow(dsINpre),
                  nrow.dsOUTpre = nrow(dsOUTpre)
     )
     newdata <- as.matrix(cbind(dsINpre, dsOUTpre))
     nd.input.check.col <- input.check.col
     nd.output.check.col <- nR + output.check.col
     ncol(newdata)
     # Empty rows
     new.data.empty.rows <- which(newdata[, nd.input.check.col] == 1 | newdata[, nd.output.check.col] == 1)
     if (length(new.data.empty.rows) > 0) {
       newdata <- newdata[-new.data.empty.rows, ]
     }
     logger$info("ParamsEstim", nR = nR, nC = nC)

     self$estsPG <- self$paramsEstim(newdata, nR = nR, nC = nC)
     logger$info("calcFractions")
     self$fracsPG <- self$calcFractions(self$estsPG, nR = nR, nC = nC)
     colnames(self$fracsPG) <- colnames(dsOUTpre)
     rownames(self$fracsPG) <- colnames(dsINpre)

     ## Datos Summary
     # VotosPaso <- data1[,7:16]
     # cols.general <- ncol(input.election)
     totals.input <- processor$input.election[, processor$votes.field]
     # VotosInput <- processor$input.election[, c(2:(cols.general - 1))]
     VotosInput <- processor$input.election[, input.shares.fields]
     # cols.ballotage <- ncol(processor$output.election)
     # VotosOutput <- processor$output.election[, c(2:3, cols.ballotage)]
     VotosOutput <- processor$output.election[, output.shares.fields]
     # totals.output <- apply(VotosOutput, MARGIN = 1, FUN = sum)
     totals.output <- rowSums(VotosOutput)

     if (length(empty.rows) > 0) {
       totals.input <- totals.input[-empty.rows, ]
       VotosInput <- VotosInput[-empty.rows, ]
       VotosOutput <- VotosOutput[-empty.rows, ]
       totals.output <- totals.output[-empty.rows]
     }

     # for (c in 1:(ncol(VotosInput))) {
     #   VotosInput[, c] <- VotosInput[, c] * totals.input
     # }
     VotosInput <- cbind(VotosInput, round(totals.input - rowSums(VotosInput), 3))
     VotosOutput <- cbind(VotosOutput, totals.output - rowSums(VotosOutput))
     colnames(VotosInput) <- colnames(dsINpre)
     colnames(VotosOutput) <- colnames(dsOUTpre)
     processor$output.table <- round(self$fracsPG * colSums(VotosInput), 0)
     if (processor$reverse.mapping) {
       colnames <- colnames(processor$output.table)
       for (j in seq_len(length(colnames))) {
         colname <- colnames[j]
         colname.mapping <- which(colname == processor$parties.mapping)
         if (length(colname.mapping) > 0) {
           inverse.mapping <- names(processor$parties.mapping)[colname.mapping]
           colnames(processor$output.table)[j] <- inverse.mapping
         }
       }
     }

     # Report
     total.input.votes <- sum(processor$input.election.original[, processor$votes.field])
     total.input.applied.votes <- sum(processor$input.election[, processor$votes.field])
     total.output.votes <- sum(processor$output.election.original[, processor$votes.field])
     total.output.applied.votes <- sum(processor$output.election[, processor$votes.field])
     processor$showResultsSummary(
       election.df = processor$input.election.original,
       description = "input.original"
     )
     processor$showResultsSummary(
       election.df = processor$input.election,
       description = "input"
     )
     processor$showResultsSummary(
       election.df = processor$output.election.original,
       description = "output.original"
     )
     processor$showResultsSummary(
       election.df = processor$output.election,
       description = "output"
     )
     logger$info("Votes",
                 total.input.votes = total.input.votes,
                 total.input.applied.votes = total.input.applied.votes,
                 total.output.votes = total.output.votes,
                 total.output.applied.votes = total.output.applied.votes,
                 change.input.output.votes = round(total.output.votes / total.input.votes, 4),
                 change.input.output.applied.votes = round(total.output.applied.votes / total.input.applied.votes, 4),
                 dismissed.input.votes = round(total.input.applied.votes / total.input.votes, 4),
                 dismissed.output.votes = round(total.output.applied.votes / total.output.votes, 4)
     )
     processor$output.table
   }))
