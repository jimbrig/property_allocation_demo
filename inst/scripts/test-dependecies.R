
#  ------------------------------------------------------------------------
#
# Title : Dependency and Installation Tests
#    By : Jimmy Briggs <jimmy.briggs@oliverwyman.com>
#  Date : 2020-06-12
#
#  ------------------------------------------------------------------------

# first create a library folder for testing and build/git ignore it
fs::dir_create("test/test-lib", recurse = TRUE)
usethis::use_build_ignore("test/test-lib")
usethis::use_git_ignore("test/test-lib")

# initialize a devtools::dev_mode empty library to mimic a new user with a
# fresh R installation
devtools::dev_mode(on = TRUE, path = "tests/test-lib")

# user will need to setup a PAT on GH
if (usethis::github_token() == "") {

  usethis::browse_github_pat()

}
gh::gh_whoami()

# install propalloc
remotes::install_github("jimbrig/propalloc" #,
                        # dependencies = TRUE,
                        )
