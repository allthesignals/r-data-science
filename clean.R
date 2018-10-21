library(tidyverse)
library(googlesheets)
library(stringdist)

googleSheet <- gs_url('https://docs.google.com/spreadsheets/d/1TB6BUQ21Jj4krguc7b0NiLdoOTluCq7hweML55r9gHw/edit#gid=340156199')
failedAddresses <- 
  gs_read(googleSheet, skip=6) %>%
  select(`Event Label`, `Total Events`)

# remove metadata Google appends to the bottom
failedAddresses <- failedAddresses[-(5001:nrow(failedAddresses)),]

failedAddresses <- failedAddresses %>%
  mutate(stringLength = nchar(`Event Label`))

# Group selection by class numbers or height 
num.height <- 0.3

# define names 
n <- failedAddresses$`Event Label`

# calculate distances
d <- stringdistmatrix(n, method="jw")

# cluster the stuff
h <- hclust(d)

# cut the cluster by height
p <- cutree(h, h = num.height)

# build the resulting frame
clusteredInputs <- tibble(`Event Label` = n, group.prob = p) %>% 
  left_join(failedAddresses, by='Event Label') %>%
  arrange(-stringLength) %>%
  distinct(group.prob, .keep_all=TRUE)
  
# gs_new('clusteredFailedAddresses', input=clusteredInputs)
