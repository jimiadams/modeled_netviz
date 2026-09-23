# samples <- read_excel("[path]/LenaDissertationDataset.xlsx")

labs <- dimnames(samples)[[2]]

library(dplyr)
library(stringr)

samples <- samples |>
  rename(art_src = labs[[1]],    # sampled artist id
         src_gen = labs[[2]],    # genre of sampled song
         src_art = labs[[3]],    # Who's being sampled?
         src_id = labs[[4]],     # I'm going to carry an id @ song level too
         src_sng = labs[[5]],    # What song's being sampled?
         art_tar = labs[[7]],    # I'm using the 2-mode version here
         tar_art = labs[[8]],    # Who's doing the sampling?
         tar_sng = labs[[10]],   # In what song?
         yr = labs[[11]]) |>     # year of sample
  mutate(
    tar_key = str_squish(paste(tar_art, tar_sng, yr, sep = " | ")),
    tar_id = dense_rank(tar_key) + 2000
  ) |>
  select(
    src_id, src_gen, src_art, src_sng,
    tar_id, tar_key, tar_art, tar_sng, yr,
    art_tar, art_src
  )

## Just confirming that everything above resulted in expected counts, etc.
samples |>
  summarise(
    n_rows = n(),
    n_target_songs = n_distinct(tar_id),
    n_target_artists = n_distinct(art_tar),
    n_source_artists = n_distinct(art_src),
    n_source_songs = n_distinct(src_id)
  )

 saveRDS(samples, "JL_samples.rds")
 