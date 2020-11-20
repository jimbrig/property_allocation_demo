
require(tibble)
require(dplyr)

renewal_costs <- tibble::tribble(
  ~cost_type, ~description,   ~prior, ~current,
   "premium",   "all_risk", 63301745, 65200798,
   "premium",  "terrorism",  3909519,  4026804,
   "expense",      "taxes",   602831,   620916,
   "expense",       "fees",  4645560,  4784927,
   "expense",    "program",  1435010,  1478060
  ) %>%
   dplyr::mutate_if(is.numeric, as.integer)

glimpse(renewal_costs)

