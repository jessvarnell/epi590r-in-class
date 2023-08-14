#Getting started with GitHub

library("usethis")
use_git_config(user.name="jessvarnell", user.email="varnelljessica@gmail.com")

usethis::create_github_token()
gitcreds::gitcreds_set()

#Avoid using setwd

install.packages("here")
here::here("data", "raw", "data.csv")
#Can use library(here) or here::here()

getwd()

setwd("data")
here::here()

#gtsummary
install.packages("gtsummary")
library(gtsummary)



