#' Push Up Time
#'
#' Only run this function if you are 100% aware of what you are pushing up! Make sure no client sensitive information is not staged. To avoid pushing up certain files update the `.gitignore` file.
#'
#' @param message A character. Message to attach to all files being pushed.
#' @param tag A logical. If TRUE, this will ask to tag the push with the current package version.
#'
#' @export
git_push_all <- function(message = "update all via git_push_all()") {

  git_user_check()

  validate <- prompt_user("This will push up ALL changed and new files to Azure DevOps. Please make sure you are aware of the files you are pushing. Proceed?")

  if (validate == FALSE) {
    usethis::ui_stop("Git push cancelled.")
  }

  files <- gert::git_status()$file

  if (length(files) == 0) {
    usethis::ui_stop("There are no files to push.")
  }

  shh <- gert::git_add(files)

  gert::git_commit_all(message = message)

  gert::git_push()

  return(invisible(NULL))
}

#' Check if the current user has the correct git config setup
#'
#' @export
#'
git_user_check <- function() {
  row_check <-
    gert::git_config() %>%
    dplyr::filter(
      name %in% c("user.name", "user.email"),
      level == "global"
    ) %>%
    nrow() !=
    2

  if (isTRUE(row_check)) {
    usethis::ui_stop("{{gert}} config is not set for {usethis::ui_value('user.name')} or {usethis::ui_value('user.email')}. Check your config with {usethis::ui_code('gert::git_config()')}.")
  }

  return(invisible(TRUE))
}

#' Display Repo Information
#'
#' Displays the information of the repo based on the current working directory.
#' @export
repo_info <- function() {
  local <- getwd()

  git_config <- safe_git_config()

  if (length(git_config$error) > 0) {
    msg <- glue::glue("Report was knitted outside of an R project / Git project.")
    return(msg)
  }

  remote <-
    git_config %>%
    purrr::pluck("result") %>%
    dplyr::filter(name == "remote.origin.url") %>%
    dplyr::pull(value)

  commit_info <-  gert::git_commit_info()
  commit_code <- stringr::str_sub(commit_info$id, end = 7)
  commit_date <- lubridate::as_datetime(commit_info$time)
  commit_message <- commit_info$message

  git_msg <- glue::glue("
  Local:  {local}
  Remote: {remote}
  Head:   [{commit_code}] {commit_date} UTC: {commit_message}
")

  return(git_msg)
}

safe_git_config <-  purrr::safely(gert::git_config)
