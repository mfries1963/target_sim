# place for target commands

#loading target verse
library(targets)
library(tarchetypes)
library(stantargets)

# other key packages
library(tidyverse)

tar_visnetwork()
tar_validate()

tar_make()

tar_objects()
tar_read(model_data_reg_1_220_3_2_0.3_0.8_0.6_1_1_fcc806ba6f4d123c)
tar_read(model_data_reg_1_220_f4d8eff9753f25db)
tar_read(mapped_models)
names(mapped_models)
## next steps: how to compile these results into a figure?
x <- tar_read(model_results)
head(x)
x$.dataset_id
tar_read(oc)
