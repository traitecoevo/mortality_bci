
update_git_repo <- function(url, dest) {
  git <- Sys_which("git")
  if (file.exists(dest)) {
    args <- c("-C", dest, "pull")
  } else {
    args <- c("clone", url, dest)
  }
  call_system(git, args)
  sha <- call_system(git, c("-C", dest, "rev-parse", "HEAD"))
  sha
}

update_data <- function() {
  update_git_repo("https://github.com/traitecoevo/mortality_data", "data")
}
update_models <- function() {
  update_git_repo("https://github.com/traitecoevo/mortality_models", "models")
}
