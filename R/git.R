
## We can rely on callr and system git as both are installed by the
## dockertest bootstrap.
update_git_repo <- function(url, dest) {
  git <- callr::Sys_which("git")
  if (file.exists(dest)) {
    args <- c("-C", dest, "pull")
  } else {
    args <- c("clone", url, dest)
  }
  callr::call_system(git, args)
  sha <- callr::call_system(git, c("-C", dest, "rev-parse", "HEAD"))
  sha
}

update_data <- function() {
  update_git_repo("https://github.com/traitecoevo/mortality_data", "data")
}
update_models <- function() {
  update_git_repo("https://github.com/traitecoevo/mortality_models", "models")
}
