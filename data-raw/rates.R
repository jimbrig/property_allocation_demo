require(tibble)
require(dplyr)

rates <- tibble::tribble(
   ~rate_type,                             ~rate_id, ~prior_rate, ~market_rate, ~model_rate,
        "aop",                               "APAC",      0.0449,       0.0449,     0.05191,
        "aop",                               "CALA",       0.075,        0.075,     0.02835,
        "aop",                             "Europe",       0.035,        0.035,     0.08815,
        "aop",             "Middle East and Africa",       0.075,        0.075,     0.09583,
        "aop",                       "Offices/Misc",       0.075,        0.075,      0.0831,
        "aop",                     "USA and Canada",     0.09052,      0.09052,     0.02899,
     "cat_eq",                           "EQ - Low",           0,            0,      0.0489,
     "cat_eq",                       "EQ - Minimal",           0,            0,      0.0251,
     "cat_eq",                          "EQ - None",           0,            0,           0,
     "cat_eq",      "EQ - Rest of World - Moderate",         0.1,          0.1,     0.22909,
     "cat_eq",        "EQ - Rest of World - Severe",       0.275,        0.275,     0.50699,
     "cat_eq",     "EQ - USA California - Moderate",       0.275,        0.275,      0.1567,
     "cat_eq",       "EQ - USA California - Severe",       0.425,        0.425,     0.37647,
     "cat_eq",                    "EQ - USA WA/PNW",        0.12,         0.12,     0.07231,
  "cat_flood",                              "Flood",       0.055,        0.055,      0.0536,
  "cat_flood",                       "Flood - None",           0,            0,           0,
   "cat_wind",     "Wind - All Other States - High",      0.3375,       0.3375,      0.9032,
   "cat_wind", "Wind - All Other States - Moderate",     0.18984,      0.18984,      0.2755,
   "cat_wind",             "Wind - APAC - Moderate",        1.42,         1.42,           0,
   "cat_wind",             "Wind - CALA - Moderate",     0.28249,      0.28249,      0.1643,
   "cat_wind",               "Wind - CALA - Severe",           1,            1,      0.9032,
   "cat_wind",   "Wind - CALA - Severe - Hurricane",           1,            1,     2.16505,
   "cat_wind",                        "Wind - None",           0,            0,           0,
   "cat_wind",     "Wind - Pacific Typhoon/Cyclone",        0.25,         0.25,      0.1434,
   "cat_wind",                 "Wind - USA Florida",     0.44543,      0.44543,     1.38038,
   "cat_wind",     "Wind - USA Florida - Hurricane",        0.45,         0.45,      1.9854,
   "cat_wind",                  "Wind - USA Hawaii",        0.25,         0.25,      0.4247,
     "terror",                               "APAC",     0.00751,      0.00751,     0.01876,
     "terror",                             "Canada",     0.01479,      0.01479,     0.02487,
     "terror",                         "Carribbean",      0.0075,       0.0075,     0.01876,
     "terror",                             "Europe",      0.0075,       0.0075,     0.02013,
     "terror",                             "Israel",      0.0175,       0.0175,     0.04247,
     "terror",                      "Latin America",      0.0075,       0.0075,      0.0263,
     "terror",                                "MEA",     0.01227,      0.01227,     0.03235,
     "terror",                "Rest of World - DIC",      0.0055,       0.0055,     0.00184,
     "terror",               "USA - High Terrorism",      0.0188,       0.0188,     0.01759,
     "terror",                "USA - Low Terrorism",      0.0075,       0.0075,     0.00142,
     "terror",           "USA - Moderate Terrorism",      0.0076,       0.0076,     0.00302
  )




