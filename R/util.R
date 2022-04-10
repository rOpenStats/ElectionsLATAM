#' mutate_cond
#' @export
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#' getHost
#' @author ken4rab
#' @export
getHost <- function() {
  Sys.info()[4]
}


#' genLogger
#' @export
genLogger <- function(r6.object) {
  lgr::get_logger(class(r6.object)[[1]])
}

#' getLogger
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
#' @param log.file
#' @import lgr
#' @export
loggerSetupFile <- function(log.file) {
  lgr::basic_config()
  lgr::get_logger("root")$add_appender(AppenderFile$new(log.file,
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
getPackagePrefix <- function() {
  "ElectionsLATAM"
}

#' getEnv
#' @export
#' @author ken4rab
getEnv <- function(variable.name, package.prefix = getPackagePrefix(), fail.on.empty = TRUE,
                   env.file = "~/.Renviron", call.counter = 0, refresh.env = FALSE,
                   logger = lgr) {
  if (refresh.env) {
    readRenviron(env.file)
    # this does not work
    # dotenv::load_dot_env()
    readRenviron(".env")
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
