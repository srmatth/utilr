#' Console log based on environment
#'
#' @inheritParams usethis::ui_info
#' @param log_type A character. This parameter will look for these options `c("info", "warn", "done", "error")` and will prefix the message, `x`.
#' @param display_function_name A logical. If `TRUE`, this will print out the calling function's name in the logging.
#' @param sys_call A numeric. Used to ensure `sys.call` looks at the right environment. For expert use only.
#'
#' @noRd
console_log <- function(x, log_type = "none", .envir = parent.frame(), sys_call = -1) {
  i <- interactive()

  calling_fun <- ifelse(is.null(sys.call(sys_call)), " ", paste0(" ", sys.call(sys_call), "(): "))
  dt <- as.POSIXlt(Sys.time(), tz = "UTC")

  old_x <- x

  x <- glue::glue_collapse(x, "\n")
  x <- glue::glue(x, .envir = .envir)
  x <- glue::glue("[{dt}]{calling_fun}{x}")

  if (isTRUE(i)) {
    if (log_type == "info") {
      x <- glue::glue("{cli::col_cyan('INFO')}  {x}")
    } else if (log_type == "warn") {
      x <- glue::glue("{cli::col_yellow('WARN')}  {x}")
    } else if (log_type == "done") {
      x <- glue::glue("{cli::col_green('DONE')}  {x}")
    } else if (log_type == "error") {
      x <- glue::glue("{cli::col_red('ERROR')} {x}")
    }

    print(x)

  } else {
    if (log_type == "info") {
      x <- glue::glue("INFO  {x}")
    } else if (log_type == "warn") {
      x <- glue::glue("WARN  {x}")
    } else if (log_type == "done") {
      x <- glue::glue("DONE  {x}")
    } else if (log_type == "error") {
      x <- glue::glue("ERROR {x}")
    }

    print(x)
  }

  if (log_type == "error") {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop(old_x)
  }

  return(invisible())
}

#' Log Info
#'
#' Wrapper function for `console_log`.
#'
#' @inheritParams console_log
#'
#' @noRd
log_info <- function(x, .envir = parent.frame()) {
  console_log(
    x = x,
    log_type = "info",
    .envir = .envir,
    sys_call = -2
  )
}

#' Log Warning
#'
#' Wrapper function for `console_log`.
#'
#' @inheritParams console_log
#'
#' @noRd
log_warn <- function(x, .envir = parent.frame()) {
  console_log(
    x = x,
    log_type = "warn",
    .envir = .envir,
    sys_call = -2
  )
}

#' Log Done
#'
#' Wrapper function for `console_log`.
#'
#' @inheritParams console_log
#'
#' @noRd
log_done <- function(x, .envir = parent.frame()) {
  console_log(
    x = x,
    log_type = "done",
    .envir = .envir,
    sys_call = -2
  )
}

#' Log Error
#'
#' Wrapper function for `console_log`. This function will throw an error and stop the code.
#'
#' @inheritParams console_log
#'
#' @noRd
log_error <- function(x, .envir = parent.frame()) {
  console_log(
    x = x,
    log_type = "error",
    .envir = .envir,
    sys_call = -2
  )
}
