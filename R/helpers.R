# Dependencies and helper functions for PLAY App

## Source libraries

require(tidyverse)

## Load data files

# TODO(ROG): Incorporate loading data from Databrary
play_data <- readr::read_csv('../csv/PLAY_non_mbcdi_all_databrary.csv')

## Global variables

age_groups_list <- c('12mo', '18mo', '24mo')

lang_groups_list <-
  c('English', 'Bilingual-English', 'Bilingual-Spanish')

selected_qa_list <- c('PLAY_Gold', 'PLAY_Silver', NA)

## Helper functions

make_site_choices_list <- function(x) {
  y <- 1:length(x)
  names(y) <- x
  as.list(y)
}