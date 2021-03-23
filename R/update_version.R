#' Update a Package's Version
#'
#' Our R developers feel the shuffled multiple choice can sometimes be confusing and slows down the process. This function also does not ask to commit files.
#'
#' @inheritParams usethis::use_version
#'
#' @export
update_version <- function(which = NULL) {

  if (is.null(which) && !rlang::is_interactive()) {
    return(invisible(FALSE))
  }

  check_is_package("use_version()")

  new_ver <- choose_version(which)

  if (is.null(new_ver)) {
    return(invisible(FALSE))
  }

  if (!prompt_user("Do you want to update to this package to {new_ver}?")) {
    return(invisible(FALSE))
  }

  use_description_field("Version", new_ver, overwrite = TRUE)

  cla_use_news_heading(new_ver)

  if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
    rstudioapi::navigateToFile("NEWS.md")
  }
  else {
    utils::file.edit("NEWS.md")
  }

  return(invisible())
}

#' Simple Y/N Prompt
#'
#' @inheritParams usethis::ui_yeah
#'
#' @export
prompt_user <- function(x) {
  usethis::ui_yeah(
    x = x,
    yes = "Yes",
    no = "No",
    n_no = 1,
    shuffle = FALSE,
    .envir = parent.frame()
  )
}

cla_use_news_heading <- function(version) {
  news_path <- proj_path("NEWS.md")

  if (!fs::file_exists(news_path)) {
    return(invisible())
  }

  news <- read_utf8(news_path)
  title <- glue::glue("# {project_name()} {version}")

  if (title == news[[1]]) {
    return(invisible())
  }

  development_title <- glue::glue("# {project_name()} (development version)")

  if (development_title == news[[1]]) {
    news[[1]] <- title
    usethis::ui_done("Replacing development heading in NEWS.md")
    return(write_utf8(news_path, news))
  }

  usethis::ui_done("Adding new heading to NEWS.md")

  # time_name_tag <- glue::glue("[{lubridate::now('UTC')} UTC] {Sys.getenv('USERNAME')}")

  write_utf8(
    news_path,
    c(
      title,
      "",
      # time_name_tag,
      # "",
      "## Choose a title (Bug Fix, Clean Up, Hot Fix, Enhancement, Feature)",
      "",
      "- Log one change per line based on title (start with a '-' to denote a new line)",
      "",
      news
    )
  )
}

check_is_package      <- getFromNamespace("check_is_package", ns = "usethis")
choose_version        <- getFromNamespace("choose_version", ns = "usethis")
use_description_field <- getFromNamespace("use_description_field", ns = "usethis")
read_utf8             <- getFromNamespace("read_utf8", ns = "usethis")
write_utf8            <- getFromNamespace("write_utf8", ns = "usethis")
project_name          <- getFromNamespace("project_name", ns = "usethis")
proj_path             <- getFromNamespace("proj_path", ns = "usethis")
