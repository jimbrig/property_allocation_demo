
require(data.table)
require(dplyr)
require(forcats)
require(tibble)

count_buckets <- tibble::tribble(
     ~bucket,            ~name,      ~min,        ~max, ~percent_surcharge, ~dollar_surcharge,
  "bucket_1",     "Below $50K",        0L,      50000L,                 0L,             1000L,
  "bucket_2",  "$50K to $100K",    50000L,     100000L,                 0L,             1500L,
  "bucket_3", "$100K to $250K",   100000L,     250000L,                 0L,             2000L,
  "bucket_4", "$250K to $500K",   250000L,     500000L,                 0L,             2500L,
  "bucket_5",   "$500K to $1M",   500000L,    1000000L,                 0L,             5000L,
  "bucket_6",     "$1M to $5M",  1000000L,    5000000L,                 0L,            10000L,
  "bucket_7",    "$5M to $10M",  5000000L,   10000000L,                 0L,            20000L,
  "bucket_8",     "Above $10M", 10000000L, 1000000000L,                 0L,            30000L
  ) %>%
  dplyr::mutate_at(1:2, as.factor)

levels(count_buckets$name) <- c("Below $50K",
                                "$50K to $100K",
                                "$100K to $250K",
                                "$250K to $500K",
                                "$500K to $1M",
                                "$1M to $5M",
                                "$5M to $10M",
                                "Above $10M")

