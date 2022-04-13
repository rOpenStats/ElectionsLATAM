#' mutate_cond
#' @description
#' Execute dplyr mutate only for rows satisfying condition
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr)
#' @param condition condition for
#' @param ... <data-masking> Name-value pairs. The name gives the name of the column in the output.
#' @param envir environment default( parent.frame())
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(a = c("X", "Y"), b = 0)
#' df <- df %>% mutate_cond(a == "X", b = 1)
#' df
#' @author ken4rab
#' @export
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#' getHost
#' @examples
#' getHost()
#' @author ken4rab
#' @export
getHost <- function() {
  Sys.info()[4]
}


#' genLogger
#' @description
#' Returns a configured logger with threshold according r6 object.
#' This function is usually called in class constructors
#' @param r6.object an r6.object
#' @examples
#' eis <- EcologicalInferenceStrategy$new()
#' eis$logger <- genLogger(eis)
#'
#' @author ken4rab
#' @export
genLogger <- function(r6.object) {
  lgr::get_logger(class(r6.object)[[1]])
}

#' getLogger
#' @description
#' Returns the configured lgr of an r6 object.
#' If the object don't have a lgr or is not initialized returns an error
#' @param r6.object an r6.object
#'
#' @examples
#' eis <- EcologicalInferenceStrategy$new()
#' getLogger(eis)
#' @author ken4rab
#' @export
getLogger <- function(r6.object) {
  ret <- r6.object$logger
  if (is.null(ret)) {
    class <- class(r6.object)[[1]]
    stop(paste("Class", class, "don't seems to have a configured logger"))
  } else {
    ret.class <- class(ret)[[1]]
    if (ret.class == "logical") {
      stop(paste("Class", ret.class, "needs to initialize logger: self$logger <- genLogger(self)"))
    }
  }
  ret
}


#' loggerSetupFile
#' @description
#' Setup logger filename with a LayoutFormat
#' @param log.filepath file path for setting a lgr::AppenderFile
#' @examples
#' log.filepath <- file.path(tempdir(), "lgr.log")
#' dir.create(tempdir(), recursive = TRUE, showWarnings = FALSE)
#' loggerSetupFile(log.filepath)
#'
#' @import lgr
#' @author ken4rab
#' @export
loggerSetupFile <- function(log.filepath) {
  lgr::basic_config()
  lgr::get_logger("root")$add_appender(AppenderFile$new(log.filepath,
    layout = LayoutFormat$new(
      fmt = "%L [%t] %m %j",
      timestamp_fmt = "%Y-%m-%d %H:%M:%OS3",
      colors = NULL,
      pad_levels = "right"
    )
  ))
}


#' getPackagePrefix
#' @author ken4rab
#' @noRd
getPackagePrefix <- function() {
  "ElectionsLATAM"
}

#' getEnv
#' @description
#' Returns the value of a variable name in .Renviron or in project .env file
#' @param variable.name the variable to be retrieved
#' @param package.prefix package prefix when retrieving from .Renviron (default getPackagePrefix())
#' @param fail.on.empty if an error should be returned when variable not found (default TRUE)
#' @param env.file  environment file (default "~/.Renviron")
#' @param call.counter Internal use (default 0)
#' @param refresh.env refresh environment before retrieving (default FALSE)
#' @param logger for inheriting logger threshold from caller (default lgr)
#' @examples
#' getEnv("variable", fail.on.empty = FALSE)
#' @export
#' @author ken4rab
getEnv <- function(variable.name, package.prefix = getPackagePrefix(), fail.on.empty = TRUE,
                   env.file = "~/.Renviron", call.counter = 0, refresh.env = FALSE,
                   logger = lgr) {
  if (refresh.env) {
    readRenviron(env.file)
    # this does not work
    # dotenv::load_dot_env()
    if (file.exists(".env")) {
      readRenviron(".env")
    }
  }
  prefixed.variable.name <- paste(package.prefix, variable.name, sep = "")
  # First look for parameter without prefix, expected in .env
  ret <- Sys.getenv(variable.name)
  if (nchar(ret) == 0) {
    # If not found, then look for parameter with prefix, expected in .Renviron
    ret <- Sys.getenv(prefixed.variable.name)
  }
  if (nchar(ret) == 0) {
    if (call.counter == 0) {
      readRenviron(env.file)
      ret <- getEnv(
        variable.name = variable.name, package.prefix = package.prefix,
        fail.on.empty = fail.on.empty, env.file = env.file,
        call.counter = call.counter + 1
      )
    } else {
      message <- paste(
        "Must configure variable",
        prefixed.variable.name,
        "in", env.file,
        "or", variable.name,
        "in", ".env"
      )
      if (fail.on.empty) {
        stop(message)
      } else {
        logger$warn(message)
        ret <- NULL
      }
    }
  }
  ret
}


#' getPackageDir
#' @examples
#' getPackageDir()
#' @author ken4rab
#' @export
getPackageDir <- function() {
  home.dir <- find.package("ElectionsLATAM", lib.loc = NULL, quiet = TRUE)
  data.subdir <- file.path("inst", "extdata")
  if (!dir.exists(file.path(home.dir, data.subdir))) {
    data.subdir <- "extdata"
  }
  file.path(home.dir, data.subdir)
}
