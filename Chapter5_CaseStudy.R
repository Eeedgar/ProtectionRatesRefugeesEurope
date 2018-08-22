#-------------------------------------------------------------------------------------------------------
# Visualise Eurostat asylum decisions

#-------------------------------------------------------------------------------------------------------
#--0-- install the relevant libraries
# for the joins and lots of other cool things
library(dplyr)
# for loading Json
library(jsonlite)
# for the string manipulation
library(stringr)

# for visualisation
# install.packages("leaflet")
library(leaflet)
library(ggplot2)

# for taking screenshots
#install.packages("mapview")
#webshot::install_phantomjs()
library(mapview)

# for reordering arrays
library(forcats)

# for pretty fonts - see https://cran.r-project.org/web/packages/extrafont/README.html
#install.packages('extrafont')
library(extrafont)
# One time hit
#font_import()
#fonts()
# Lets try to use Trebuchet for the charts
fontsForCharts <- c( "Trebuchet MS" )

# For formatting pretty numbers ...
library(scales)

# to read the png files
library(png)

# to render the images in a grob
library(grid)

# for the final arrangements
library(gridExtra)

# set the working directory
setwd("C:/Dropbox/Data Nirvana/R_Project/learn-chapter-5-master")


#-------------------------------------------------------------------------------------------------------
# Worker functions ...
#-------------------------------------------------------------------------------------------------------

#----- Lookup for the country of origin name
LabelGetCoOName <- 
  function(value) {
    message(str_c(value, "    ", citizenList[value]))
    value <- citizenList[value]
}  

#----- Generates a percentage
GetPercent <- 
  function(enum, denom, rounding) {
    pc <- 0
    rounding <- as.numeric(rounding)
    enum <- as.numeric(enum)
    denom <- as.numeric(denom)
    
    if(enum > 0 && denom > 0) {
      pc <- round(enum/denom*100,rounding)
    }
    
    returnValue <- as.numeric(pc)
}

#----- Percent label creator for the stacked bar chart
GetPercentLabel <- 
  function(pc, threshold) {

    pc <- as.numeric(pc)
    threshold <- as.numeric(threshold)

    percStr <- ifelse(
      (is.na(pc) == FALSE && pc >= threshold),
      str_c(as.character(pc), "%"),
      ""
    )
    
    #    message(str_c("\nEnum: ", enum, " Denom: ", denom, " Threshold:", threshold, " Percent:", pc, " Str:", percStr))
    returnValue <- percStr
}  


#-------------------------------------------------------------------------------------------------------
#--1-- Load the datasets - first the country data with the centroids of each

# Source: https://developers.google.com/public-data/docs/canonical/countries_csv
countryCentroids <- tibble::tribble(
  ~country,  ~latitude,  ~longitude,                                          ~name,
      "AD",  42.546245,    1.601554,                                      "Andorra",
      "AE",  23.424076,   53.847818,                         "United Arab Emirates",
      "AF",   33.93911,   67.709953,                                  "Afghanistan",
      "AG",  17.060816,  -61.796428,                          "Antigua and Barbuda",
      "AI",  18.220554,  -63.068615,                                     "Anguilla",
      "AL",  41.153332,   20.168331,                                      "Albania",
      "AM",  40.069099,   45.038189,                                      "Armenia",
      "AN",  12.226079,  -69.060087,                         "Netherlands Antilles",
      "AO", -11.202692,   17.873887,                                       "Angola",
      "AQ", -75.250973,   -0.071389,                                   "Antarctica",
      "AR", -38.416097,  -63.616672,                                    "Argentina",
      "AS", -14.270972, -170.132217,                               "American Samoa",
      "AT",  47.516231,   14.550072,                                      "Austria",
      "AU", -25.274398,  133.775136,                                    "Australia",
      "AW",   12.52111,  -69.968338,                                        "Aruba",
      "AZ",  40.143105,   47.576927,                                   "Azerbaijan",
      "BA",  43.915886,   17.679076,                       "Bosnia and Herzegovina",
      "BB",  13.193887,  -59.543198,                                     "Barbados",
      "BD",  23.684994,   90.356331,                                   "Bangladesh",
      "BE",  50.503887,    4.469936,                                      "Belgium",
      "BF",  12.238333,   -1.561593,                                 "Burkina Faso",
      "BG",  42.733883,    25.48583,                                     "Bulgaria",
      "BH",  25.930414,   50.637772,                                      "Bahrain",
      "BI",  -3.373056,   29.918886,                                      "Burundi",
      "BJ",    9.30769,    2.315834,                                        "Benin",
      "BM",  32.321384,   -64.75737,                                      "Bermuda",
      "BN",   4.535277,  114.727669,                                       "Brunei",
      "BO", -16.290154,  -63.588653,                                      "Bolivia",
      "BR", -14.235004,   -51.92528,                                       "Brazil",
      "BS",   25.03428,   -77.39628,                                      "Bahamas",
      "BT",  27.514162,   90.433601,                                       "Bhutan",
      "BV", -54.423199,    3.413194,                                "Bouvet Island",
      "BW", -22.328474,   24.684866,                                     "Botswana",
      "BY",  53.709807,   27.953389,                                      "Belarus",
      "BZ",  17.189877,   -88.49765,                                       "Belize",
      "CA",  56.130366, -106.346771,                                       "Canada",
      "CC", -12.164165,   96.870956,                      "Cocos [Keeling] Islands",
      "CD",  -4.038333,   21.758664,                                  "Congo [DRC]",
      "CF",   6.611111,   20.939444,                     "Central African Republic",
      "CG",  -0.228021,   15.827659,                             "Congo [Republic]",
      "CH",  46.818188,    8.227512,                                  "Switzerland",
      "CI",   7.539989,    -5.54708,                                "Côte d'Ivoire",
      "CK", -21.236736, -159.777671,                                 "Cook Islands",
      "CL", -35.675147,  -71.542969,                                        "Chile",
      "CM",   7.369722,   12.354722,                                     "Cameroon",
      "CN",   35.86166,  104.195397,                                        "China",
      "CO",   4.570868,  -74.297333,                                     "Colombia",
      "CR",   9.748917,  -83.753428,                                   "Costa Rica",
      "CU",  21.521757,  -77.781167,                                         "Cuba",
      "CV",  16.002082,  -24.013197,                                   "Cape Verde",
      "CX", -10.447525,  105.690449,                             "Christmas Island",
      "CY",  35.126413,   33.429859,                                       "Cyprus",
      "CZ",  49.817492,   15.472962,                               "Czech Republic",
      "DE",  51.165691,   10.451526,                                      "Germany",
      "DJ",  11.825138,   42.590275,                                     "Djibouti",
      "DK",   56.26392,    9.501785,                                      "Denmark",
      "DM",  15.414999,  -61.370976,                                     "Dominica",
      "DO",  18.735693,  -70.162651,                           "Dominican Republic",
      "DZ",  28.033886,    1.659626,                                      "Algeria",
      "EC",  -1.831239,  -78.183406,                                      "Ecuador",
      "EE",  58.595272,   25.013607,                                      "Estonia",
      "EG",  26.820553,   30.802498,                                        "Egypt",
      "EH",  24.215527,  -12.885834,                               "Western Sahara",
      "ER",  15.179384,   39.782334,                                      "Eritrea",
      "ES",  40.463667,    -3.74922,                                        "Spain",
      "ET",      9.145,   40.489673,                                     "Ethiopia",
      "FI",   61.92411,   25.748151,                                      "Finland",
      "FJ", -16.578193,  179.414413,                                         "Fiji",
      "FK", -51.796253,  -59.523613,            "Falkland Islands [Islas Malvinas]",
      "FM",   7.425554,  150.550812,                                   "Micronesia",
      "FO",  61.892635,   -6.911806,                                "Faroe Islands",
      "FR",  46.227638,    2.213749,                                       "France",
      "GA",  -0.803689,   11.609444,                                        "Gabon",
      "GB",  55.378051,   -3.435973,                               "United Kingdom",
      "GD",  12.262776,  -61.604171,                                      "Grenada",
      "GE",  42.315407,   43.356892,                                      "Georgia",
      "GF",   3.933889,  -53.125782,                                "French Guiana",
      "GG",  49.465691,   -2.585278,                                     "Guernsey",
      "GH",   7.946527,   -1.023194,                                        "Ghana",
      "GI",  36.137741,   -5.345374,                                    "Gibraltar",
      "GL",  71.706936,  -42.604303,                                    "Greenland",
      "GM",  13.443182,  -15.310139,                                       "Gambia",
      "GN",   9.945587,   -9.696645,                                       "Guinea",
      "GP",  16.995971,  -62.067641,                                   "Guadeloupe",
      "GQ",   1.650801,   10.267895,                            "Equatorial Guinea",
      "GR",  39.074208,   21.824312,                                       "Greece",
      "GS", -54.429579,  -36.587909, "South Georgia and the South Sandwich Islands",
      "GT",  15.783471,  -90.230759,                                    "Guatemala",
      "GU",  13.444304,  144.793731,                                         "Guam",
      "GW",  11.803749,  -15.180413,                                "Guinea-Bissau",
      "GY",   4.860416,   -58.93018,                                       "Guyana",
      "GZ",  31.354676,   34.308825,                                   "Gaza Strip",
      "HK",  22.396428,  114.109497,                                    "Hong Kong",
      "HM",  -53.08181,   73.504158,            "Heard Island and McDonald Islands",
      "HN",  15.199999,  -86.241905,                                     "Honduras",
      "HR",       45.1,        15.2,                                      "Croatia",
      "HT",  18.971187,  -72.285215,                                        "Haiti",
      "HU",  47.162494,   19.503304,                                      "Hungary",
      "ID",  -0.789275,  113.921327,                                    "Indonesia",
      "IE",   53.41291,    -8.24389,                                      "Ireland",
      "IL",  31.046051,   34.851612,                                       "Israel",
      "IM",  54.236107,   -4.548056,                                  "Isle of Man",
      "IN",  20.593684,    78.96288,                                        "India",
      "IO",  -6.343194,   71.876519,               "British Indian Ocean Territory",
      "IQ",  33.223191,   43.679291,                                         "Iraq",
      "IR",  32.427908,   53.688046,                                         "Iran",
      "IS",  64.963051,  -19.020835,                                      "Iceland",
      "IT",   41.87194,    12.56738,                                        "Italy",
      "JE",  49.214439,    -2.13125,                                       "Jersey",
      "JM",  18.109581,  -77.297508,                                      "Jamaica",
      "JO",  30.585164,   36.238414,                                       "Jordan",
      "JP",  36.204824,  138.252924,                                        "Japan",
      "KE",  -0.023559,   37.906193,                                        "Kenya",
      "KG",   41.20438,   74.766098,                                   "Kyrgyzstan",
      "KH",  12.565679,  104.990963,                                     "Cambodia",
      "KI",  -3.370417, -168.734039,                                     "Kiribati",
      "KM", -11.875001,   43.872219,                                      "Comoros",
      "KN",  17.357822,  -62.782998,                        "Saint Kitts and Nevis",
      "KP",  40.339852,  127.510093,                                  "North Korea",
      "KR",  35.907757,  127.766922,                                  "South Korea",
      "KW",   29.31166,   47.481766,                                       "Kuwait",
      "KY",  19.513469,  -80.566956,                               "Cayman Islands",
      "KZ",  48.019573,   66.923684,                                   "Kazakhstan",
      "LA",   19.85627,  102.495496,                                         "Laos",
      "LB",  33.854721,   35.862285,                                      "Lebanon",
      "LC",  13.909444,  -60.978893,                                  "Saint Lucia",
      "LI",     47.166,    9.555373,                                "Liechtenstein",
      "LK",   7.873054,   80.771797,                                    "Sri Lanka",
      "LR",   6.428055,   -9.429499,                                      "Liberia",
      "LS", -29.609988,   28.233608,                                      "Lesotho",
      "LT",  55.169438,   23.881275,                                    "Lithuania",
      "LU",  49.815273,    6.129583,                                   "Luxembourg",
      "LV",  56.879635,   24.603189,                                       "Latvia",
      "LY",    26.3351,   17.228331,                                        "Libya",
      "MA",  31.791702,    -7.09262,                                      "Morocco",
      "MC",  43.750298,    7.412841,                                       "Monaco",
      "MD",  47.411631,   28.369885,                                      "Moldova",
      "ME",  42.708678,    19.37439,                                   "Montenegro",
      "MG", -18.766947,   46.869107,                                   "Madagascar",
      "MH",   7.131474,  171.184478,                             "Marshall Islands",
      "MK",  41.608635,   21.745275,                            "Macedonia [FYROM]",
      "ML",  17.570692,   -3.996166,                                         "Mali",
      "MM",  21.913965,   95.956223,                              "Myanmar [Burma]",
      "MN",  46.862496,  103.846656,                                     "Mongolia",
      "MO",  22.198745,  113.543873,                                        "Macau",
      "MP",   17.33083,   145.38469,                     "Northern Mariana Islands",
      "MQ",  14.641528,  -61.024174,                                   "Martinique",
      "MR",   21.00789,  -10.940835,                                   "Mauritania",
      "MS",  16.742498,  -62.187366,                                   "Montserrat",
      "MT",  35.937496,   14.375416,                                        "Malta",
      "MU", -20.348404,   57.552152,                                    "Mauritius",
      "MV",   3.202778,    73.22068,                                     "Maldives",
      "MW", -13.254308,   34.301525,                                       "Malawi",
      "MX",  23.634501, -102.552784,                                       "Mexico",
      "MY",   4.210484,  101.975766,                                     "Malaysia",
      "MZ", -18.665695,   35.529562,                                   "Mozambique",
      "NA",  -22.95764,    18.49041,                                      "Namibia",
      "NC", -20.904305,  165.618042,                                "New Caledonia",
      "NE",  17.607789,    8.081666,                                        "Niger",
      "NF", -29.040835,  167.954712,                               "Norfolk Island",
      "NG",   9.081999,    8.675277,                                      "Nigeria",
      "NI",  12.865416,  -85.207229,                                    "Nicaragua",
      "NL",  52.132633,    5.291266,                                  "Netherlands",
      "NO",  60.472024,    8.468946,                                       "Norway",
      "NP",  28.394857,   84.124008,                                        "Nepal",
      "NR",  -0.522778,  166.931503,                                        "Nauru",
      "NU", -19.054445, -169.867233,                                         "Niue",
      "NZ", -40.900557,  174.885971,                                  "New Zealand",
      "OM",  21.512583,   55.923255,                                         "Oman",
      "PA",   8.537981,  -80.782127,                                       "Panama",
      "PE",  -9.189967,  -75.015152,                                         "Peru",
      "PF", -17.679742, -149.406843,                             "French Polynesia",
      "PG",  -6.314993,   143.95555,                             "Papua New Guinea",
      "PH",  12.879721,  121.774017,                                  "Philippines",
      "PK",  30.375321,   69.345116,                                     "Pakistan",
      "PL",  51.919438,   19.145136,                                       "Poland",
      "PM",  46.941936,   -56.27111,                    "Saint Pierre and Miquelon",
      "PN", -24.703615, -127.439308,                             "Pitcairn Islands",
      "PR",  18.220833,  -66.590149,                                  "Puerto Rico",
      "PS",  31.952162,   35.233154,                      "Palestinian Territories",
      "PT",  39.399872,   -8.224454,                                     "Portugal",
      "PW",    7.51498,   134.58252,                                        "Palau",
      "PY", -23.442503,  -58.443832,                                     "Paraguay",
      "QA",  25.354826,   51.183884,                                        "Qatar",
      "RE", -21.115141,   55.536384,                                      "Réunion",
      "RO",  45.943161,    24.96676,                                      "Romania",
      "RS",  44.016521,   21.005859,                                       "Serbia",
      "RU",   61.52401,  105.318756,                                       "Russia",
      "RW",  -1.940278,   29.873888,                                       "Rwanda",
      "SA",  23.885942,   45.079162,                                 "Saudi Arabia",
      "SB",   -9.64571,  160.156194,                              "Solomon Islands",
      "SC",  -4.679574,   55.491977,                                   "Seychelles",
      "SD",  12.862807,   30.217636,                                        "Sudan",
      "SE",  60.128161,   18.643501,                                       "Sweden",
      "SG",   1.352083,  103.819836,                                    "Singapore",
      "SH", -24.143474,  -10.030696,                                 "Saint Helena",
      "SI",  46.151241,   14.995463,                                     "Slovenia",
      "SJ",  77.553604,   23.670272,                       "Svalbard and Jan Mayen",
      "SK",  48.669026,   19.699024,                                     "Slovakia",
      "SL",   8.460555,  -11.779889,                                 "Sierra Leone",
      "SM",   43.94236,   12.457777,                                   "San Marino",
      "SN",  14.497401,  -14.452362,                                      "Senegal",
      "SO",   5.152149,   46.199616,                                      "Somalia",
      "SR",   3.919305,  -56.027783,                                     "Suriname",
      "ST",    0.18636,    6.613081,                        "São Tomé and Príncipe",
      "SV",  13.794185,   -88.89653,                                  "El Salvador",
      "SY",  34.802075,   38.996815,                                        "Syria",
      "SZ", -26.522503,   31.465866,                                    "Swaziland",
      "TC",  21.694025,  -71.797928,                     "Turks and Caicos Islands",
      "TD",  15.454166,   18.732207,                                         "Chad",
      "TF", -49.280366,   69.348557,                  "French Southern Territories",
      "TG",   8.619543,    0.824782,                                         "Togo",
      "TH",  15.870032,  100.992541,                                     "Thailand",
      "TJ",  38.861034,   71.276093,                                   "Tajikistan",
      "TK",  -8.967363, -171.855881,                                      "Tokelau",
      "TL",  -8.874217,  125.727539,                                  "Timor-Leste",
      "TM",  38.969719,   59.556278,                                 "Turkmenistan",
      "TN",  33.886917,    9.537499,                                      "Tunisia",
      "TO", -21.178986, -175.198242,                                        "Tonga",
      "TR",  38.963745,   35.243322,                                       "Turkey",
      "TT",  10.691803,  -61.222503,                          "Trinidad and Tobago",
      "TV",  -7.109535,   177.64933,                                       "Tuvalu",
      "TW",   23.69781,  120.960515,                                       "Taiwan",
      "TZ",  -6.369028,   34.888822,                                     "Tanzania",
      "UA",  48.379433,    31.16558,                                      "Ukraine",
      "UG",   1.373333,   32.290275,                                       "Uganda",
      "UM",         NA,          NA,                  "U.S. Minor Outlying Islands",
      "US",   37.09024,  -95.712891,                                "United States",
      "UY", -32.522779,  -55.765835,                                      "Uruguay",
      "UZ",  41.377491,   64.585262,                                   "Uzbekistan",
      "VA",  41.902916,   12.453389,                                 "Vatican City",
      "VC",  12.984305,  -61.287228,             "Saint Vincent and the Grenadines",
      "VE",    6.42375,   -66.58973,                                    "Venezuela",
      "VG",  18.420695,  -64.639968,                       "British Virgin Islands",
      "VI",  18.335765,  -64.896335,                          "U.S. Virgin Islands",
      "VN",  14.058324,  108.277199,                                      "Vietnam",
      "VU", -15.376706,  166.959158,                                      "Vanuatu",
      "WF", -13.768752, -177.156097,                            "Wallis and Futuna",
      "WS", -13.759029, -172.104629,                                        "Samoa",
      "XK",  42.602636,   20.902977,                                       "Kosovo",
      "YE",  15.552727,   48.516388,                                        "Yemen",
      "YT",   -12.8275,   45.166244,                                      "Mayotte",
      "ZA", -30.559482,   22.937506,                                 "South Africa",
      "ZM", -13.133897,   27.849332,                                       "Zambia",
      "ZW", -19.015438,   29.154857,                                     "Zimbabwe"
  )

# Europe tweaks - Use the correct Iso codes for UK and Greece
countryCentroids[countryCentroids=="GB"] <- "UK"
countryCentroids[countryCentroids=="GR"] <- "EL"


#-------------------------------------------------------------------------------------------------------
#--2-- Load the Eurostat data which is included in JSON Files

# So these are the EU 28 countries with the four additional EU+ countries (Norway, Iceland, Liechtenstein and Switzerland)
euPlusCountries <- tibble::tribble(
  ~Iso,    ~Name,
  "BE",    "Belgium",
  "BG",    "Bulgaria",
  "CZ",    "Czech Republic",
  "DK",    "Denmark",
  "DE",    "Germany",
  "EE",    "Estonia",
  "IE",    "Ireland",
  "EL",    "Greece",
  "ES",    "Spain",
  "FR",    "France",
  "HR",    "Croatia",
  "IT",    "Italy",
  "CY",    "Cyprus",
  "LV",    "Latvia",
  "LT",    "Lithuania",
  "LU",    "Luxembourg",
  "HU",    "Hungary",
  "MT",    "Malta",
  "NL",    "Netherlands",
  "AT",    "Austria",
  "PL",    "Poland",
  "PT",    "Portugal",
  "RO",    "Romania",
  "SI",    "Slovenia",
  "SK",    "Slovakia",
  "FI",    "Finland",
  "SE",    "Sweden",
  "UK",    "United Kingdom",
  "IS",    "Iceland",
  "LI",    "Liechtenstein",
  "NO",    "Norway",
  "CH",    "Switzerland"
  )

#-------------------------------------------------------------------------------------------------------
#  We need to go through and get data by building this URL multiple times using specific parameters
# https://ec.europa.eu/eurostat/wdds/rest/data/v2.1/json/en/migr_asydcfstq
#   ?freq=Q&unit=PER&citizen=VE&sex=T&age=TOTAL&decision=TOTAL&time=2016Q4

jsonURLStub <-"https://ec.europa.eu/eurostat/wdds/rest/data/v2.1/json/en/"
jsonDataset <-"migr_asydcfstq"
# There is a bug at the moment in the Eurostats data with data from this year and last year currently not available
jsonTimePeriod <- "2016Q4" # "2018Q1" 

#-------------------------------------------------------------------------------------------------------
# According to the mid year data factsheet for Europe, refugees and migrants from these five countries
# have submitted the most asylum application this year in Europe: Syrians, Iraqis, Afghans, Nigerians and Pakistanis
citizenList <- list()
citizenList[[ "TOTAL" ]] <- "Total"
citizenList[[ "SY" ]] <- "Syrian Arab Republic"
citizenList[[ "IQ" ]] <- "Iraq"
citizenList[[ "AF" ]] <- "Afghanistan"
citizenList[[ "NG" ]] <- "Nigeria"
citizenList[[ "PK" ]] <- "Pakistan"


#-------------------------------------------------------------------------------------------------------
# And here are the list of decisions we want to collect
decisionList <- data.frame(
  # Remember to set the levels as shown in this chart ...  THis is what enforces the order of the elements
  # https://stackoverflow.com/questions/31638771/r-reorder-levels-of-a-factor-alphabetically-but-one
  # Titles
  DecisionTitle=factor(c("Total", "Rejected", "Subsidiary", "Humanitarian", "Geneva convention"),
    levels=c("Total", "Rejected", "Subsidiary", "Humanitarian", "Geneva convention")),
  # Eurostat keys
  DecisionKey=factor(c("TOTAL", "REJECTED", "SUB_PROT", "HUMSTAT", "GENCONV"),
    levels=c("TOTAL", "REJECTED", "SUB_PROT", "HUMSTAT", "GENCONV")), 
  # Pretty legend colours
  DecisionLegend=factor(c("#505050", "#d23f67", "#f7bb16", "#e77b37", "#2c8ac1"),
    levels=c("#505050", "#d23f67", "#f7bb16", "#e77b37", "#2c8ac1"))

)



#-------------------------------------------------------------------------------------------------------
# declare our data cube as an empty data frame with the relevant column types
dataCube <- data.frame(
    Characters=character(),# Geo
    Characters=character(),# Citizen
    Characters=character(),# Decision
    Ints=integer())        # Count

# Set the counters
counter <- 1
countTotal <- length(citizenList) * length(decisionList$DecisionKey)
# Now loop through each citizen / nationality option
for( citizen in names(citizenList)) {
  
  # And an inner loop on the decisions
  for( decisionType in decisionList$DecisionKey) {

    # Write a message to the console so that the user can see that something is happening ...
    message(str_c(
      "Downloading ", counter, " of ", countTotal, 
      " JSON data from Eurostat for citizen ", citizen , " and decision type ", decisionType))
    
    jsonURL <- str_c(jsonURLStub, jsonDataset, 
        "?freq=Q&unit=PER&citizen=", citizen, 
        "&sex=T&age=TOTAL&decision=", decisionType, 
        "&time=", jsonTimePeriod )
    
    dataWrapper <- fromJSON(jsonURL)

    if(length(dataWrapper$dimension$geo$category$index) != length(dataWrapper$value) ) {
      warning( 
        str_c("Length of categories: ", length(dataWrapper$dimension$geo$category$index), 
            " is not the same as the length of values: ", length(dataWrapper$value), ".  Should be able to clean this up ..."))
    }
    
    # OK - now this is fiddly because of the way the JSON is structured
    # In order to reduce the download volume, missing data is not supplied 
    # This means that, not all GEO labels will be provided; and even not all of those will have values!!
    values <- c()
    
    i <- 1
    while(i <= length(euPlusCountries$Iso)) {
      # set the currentVal to NA
      currentVal <- as.integer(NaN)
      
      # See if the label exists
      cLab <- dataWrapper$dimension$geo$category$index[[ euPlusCountries$Iso[i] ]]
      
      if(is.na(cLab) == FALSE && is.null(cLab) == FALSE && is.null(cLab[0]) == FALSE) {
        # get the index as a character
        currentVal <- dataWrapper$value[ as.character(cLab) ]
        # Check for missing or bad data - the last clause seems to be the most useful one ...
        if(is.na(currentVal) || is.null(currentVal) || is.null(as.character(currentVal)) || as.character(currentVal) == "NULL") {
          message( "Found bad data and fixing it")
          currentVal <- as.integer(NaN)
        }
      }

      values[i] <- currentVal
      i <- i + 1
    }

    # now combine these two... THis is a bit hacky at the moment as if there is missing 
    # data the two arrays will be different lengths and everything country will be wonky
    dataCubeTemp <- do.call(rbind.data.frame, 
        Map('c', euPlusCountries$Iso, citizen, decisionType, values))
    
    colnames(dataCubeTemp) = c("GeoIso","Citizen","Decision","Count")
    
    # make the count list numeric ...
    dataCubeTemp  <- dataCubeTemp %>%
      mutate(Count=as.numeric(as.character(Count)))

    # remove the total counts (GeoIso == EU28 and TOTAL) - this should now be redundant due to the code changes above, but there is no harm in trying!
    dataCubeTemp <- filter(dataCubeTemp, GeoIso != "EU28" & GeoIso != "TOTAL")
    
    # Append to the global data cube ...
    ifelse(length(dataCube) == 0, 
           dataCube <- dataCubeTemp, 
           dataCube <- rbind( dataCube, dataCubeTemp))
    
    # Increment our process counter
    counter <- counter + 1
  }
}

View(dataCube)


#-------------------------------------------------------------------------------------------------------
# Then join the the country data to the data
dataCube <- left_join(dataCube, countryCentroids, by=c("GeoIso"="country") )
View(dataCube)


# This is a good point to save this data cube - so we can get back to it if needed ...
write.csv2(dataCube, str_c("EuroStatsData_", jsonTimePeriod, ".csv"))

# And read the data cube back out again
dataCube <- read.csv2(str_c("EuroStatsData_", jsonTimePeriod, ".csv"))
View(dataCube)




#-------------------------------------------------------------------------------------------------------
# Lets build the dataCube that will support our map and charts ...
dataCubeVis <- dataCube
dataCubeVis[is.na(dataCubeVis)] <- 0

# lets filter down to the 12 most common countries and then group all the others into an other category...
allCountries <- dataCubeVis %>% 
  filter(Citizen == "TOTAL" ) %>%
  filter(Decision == "TOTAL" ) %>%  
  group_by(GeoIso) %>%
  summarise(CountForOrder=sum(Count)) %>%
  ungroup() %>%
  arrange(desc(CountForOrder)) 
#View(allCountries)

# Get the counts of most common and others ...
totalCount <- sum( allCountries$CountForOrder)
# Here is our sliced data
mostCommonCountries <- allCountries[1:12,]
mostCommonCount <- sum( mostCommonCountries$CountForOrder)
otherCount <- totalCount - mostCommonCount

dcvSummary <- rbind( mostCommonCountries, data.frame(GeoIso="Other", CountForOrder=otherCount))

# Check it
View(dcvSummary)  


#-------------------------------------------------------------------------------------------------------
# Now lets create our actual data cube
# Lets join it to the most common countries to pull accross the CountForOrder col which will be NA fo other countres
dcvt <- left_join(dataCubeVis, mostCommonCountries, by=c("GeoIso"="GeoIso") )
# Important - ensure that the levels of the Citizen data are consistent with the order in the citizen list...
levels(dcvt$Citizen) <- citizenList

# Then we split our dataCube into two by filtering the other countries and set the GeoIso col and a new GeoName col to 0
# We also want to reset the CountForOrder as we want other to appear at the end...
dataCubeTemp <- filter(dcvt, is.na(CountForOrder)) %>% 
  mutate(GeoIso="Other", GeoName="Other", CountForOrder=0)

dcvt <- filter(dcvt, is.na(CountForOrder) == FALSE)  %>% mutate(GeoName=name)
# Then we join it back together again
dcvt <- rbind( dcvt, dataCubeTemp)
View(dcvt)
# Collapse all the "other" rows by grouping the data
dcvt <- dcvt %>% 
  group_by(GeoIso, GeoName, Citizen, Decision) %>%
  summarise(Count=sum(Count), CountForOrder=max(CountForOrder)) %>%
  ungroup() %>%
  arrange(desc(CountForOrder))
# Double check that the names are gucci  
names(dcvt) <- c("GeoIso", "GeoName", "Citizen","Decision","Count", "CountForOrder")

# Filter the data summary to include just the total counts
dcvtTotal <- dcvt %>% 
  filter(Citizen == "TOTAL") %>% 
  filter(Decision == "TOTAL")
# Filter the data summary to include just the total counts
dcvtTotalCitizens <- dcvt %>% 
  filter(Citizen != "TOTAL") %>% 
  filter(Decision == "TOTAL")

# And for the detailed views, remove the citizens and decisions total, which is not relevant for these charts
dcvtDetails <- dcvt %>% 
  filter(Citizen != "TOTAL") %>% 
  filter(Decision != "TOTAL")

#levels(dcvtDetails$Citizen) <- citizenList

# We're going to try to show a few totals on the chart directly, so lets create a well formatted total
dcvtTotal <- dcvtTotal %>% mutate(CountLabel=comma(Count))
dcvtTotalCitizens <- dcvtTotalCitizens %>% mutate(CountLabel=comma(Count))

# Check them
View(dcvtTotal)
View(dcvtTotalCitizens)
View(dcvtDetails)


#-------------------------------------------------------------------------------------------------------
# Summary 1 - the total number of decisions by the top 12 countries of asylum and the others grouped together

# Maybe use a log scale here?
plot1 <- ggplot(dcvtTotal, 
    aes(x=fct_reorder(GeoName, CountForOrder, desc=TRUE), 
    # Lets plot in '000s to reduce the number of zeros shown        
    y=(Count),
    label=CountLabel)) +
  geom_bar(stat="identity") +
  # And this is to present the labels ...
  geom_text(hjust=-0.2, size = 4, colour="#505050") + 
  scale_y_continuous(limits=c(0,max(dcvtTotal$Count)*1.15)) +
  coord_flip() +
  labs(
    title="Decisions by country of asylum", 
    y="", 
    x="", 
    #    caption="Source: Eurostat",
    caption="",
    family=fontsForCharts) +
  # set a very minimal theme
  theme_minimal(base_family=fontsForCharts) +   
  # Tweak the axis text
  theme(
      axis.text.x=element_text(family=fontsForCharts, colour="#aaaaaa", size=10), 
      axis.text.y=element_text(family=fontsForCharts, size=12))

  
plot1

#-------------------------------------------------------------------------------------------------------
# Summary 2 - the total number of decisions by the top 12 countries of asylum and top 5 countries of origin
# Maybe use a log scale here?
plot2 <- ggplot(dcvtTotalCitizens, 
      aes(x=fct_reorder(GeoName, CountForOrder, desc=TRUE), 
      y=Count,
      label=CountLabel)) +
  geom_bar(stat="identity") +
  # And this is to present the labels ...
  geom_text(hjust=-0.2, size = 3, colour="#505050") + 
  coord_flip() +
  facet_wrap(~Citizen, labeller=as_labeller(LabelGetCoOName), ncol=5) +
  labs(
    title="Number of decisions by nationality", 
    y="", 
    x="", 
#    caption="Source: Eurostat",
    caption="",
    family=fontsForCharts) +
  # set a very minimal theme
  theme_minimal(base_family=fontsForCharts) +   
  # Tweak the axis text
  theme(
    axis.text.x=element_text(family=fontsForCharts, colour="#aaaaaa", size=7), 
    axis.text.y=element_text(family=fontsForCharts, size=12))

plot2



#-------------------------------------------------------------------------------------------------------
# Summary 3 - the % of each decision type by country and broken out by country of origin

# Lets pull across the proper name for the Decisions from the decisionList data fram
dcvDecisions <- left_join(dcvtDetails, decisionList, by=c("Decision"="DecisionKey")) 
# And lets remove the totals as they are not necessary for this view
dcvDecisions <- filter(dcvDecisions, Decision != "TOTAL")

# This creates the percent and the percent label columns - it looks a little intense, 
dcvDecisions <- dcvDecisions %>% 
  group_by(GeoIso, Citizen) %>% 
  # okay, we've got the total, now we can do some math with mutate
  mutate( GeoCitizenTotal=sum(Count, na.rm=T)) %>%    
  ungroup() %>%    
  group_by(GeoIso, Citizen, Decision) %>% 
  # okay, now lets also create a label for all columns
  mutate(
    Percent=GetPercent(Count, GeoCitizenTotal, 0), 
    PercentLabel=GetPercentLabel(Percent, 15.0)) %>% 
  ungroup()

# Then strip out all the zeros - this is probably not necessary, but they will also not be shown..
dcvDecisions <- filter(dcvDecisions, Percent > 0 )
#warnings()  
# Good to have a quick look at the data here
View(dcvDecisions)



pos <- position_fill(vjust=0.47)

plot3 <- ggplot(dcvDecisions,
    # The x axis is the names of the countries ordered by the overall count                
    aes(x=fct_reorder(GeoName, CountForOrder, desc=TRUE),
    # and the y axis is the percentage based on the variable (with the zeros removed)
    y=Percent,
    # and the labels are the percentage label strings
    label=PercentLabel,    
    # and the fill is the decisions
    fill=DecisionTitle, 
    na.rm=TRUE)) +
  geom_bar(position=pos, stat="identity") +
  geom_text(position=pos, size = 3, colour="#ffffff") + 
  # Then set our colours and legend labels using the parameters of scale_fill_manual
  # note that we strim as needed to avoid the total count
  scale_fill_manual(values=as.vector(decisionList$DecisionLegend[2:5])) +
  # flip the coordinates
  coord_flip() +
  # set the labels
  labs(title="Type of decisions by nationality (%)", 
       y="", 
       x="", 
       fill="", 
       caption="Source: Eurostat", 
       family=fontsForCharts) +
  # set a very minimal theme
  theme_minimal(base_family=fontsForCharts) +   
  # These two lines tweak the positioning of the legend and hide the x axis ticks need to go AFTER the call to theme_minimal
  theme(
    axis.text.x=element_blank(), 
    axis.text.y=element_text(family=fontsForCharts, size=12)) +
  theme(legend.position="bottom" ) +
  facet_wrap(. ~Citizen, labeller=as_labeller(LabelGetCoOName), ncol=5)  
  
plot3



#-------------------------------------------------------------------------------------------------------
#cof <- colorFactor(c("#53a7dd"), domain=c("Total"))

dcvMap <- left_join(allCountries, countryCentroids, by=c("GeoIso"="country") )
View(dcvMap)

# Get the range of values ...
## Ok would need to logarithm the counts here ... to make them look prettier
#radiusFactor = (pi * 20 * 20)/log10(max(dcvMap$CountForOrder))
radiusFactor = (40)/log10(max(dcvMap$CountForOrder)) 

mapBoxURL <- "https://api.mapbox.com/styles/v1/edgarscrase/cjl1c78tn37v72sofn80jo9en/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiZWRnYXJzY3Jhc2UiLCJhIjoiY2pram90c3M3MWRxdjNxcWhzOXRzY3N6ZCJ9._HQxSBcViAYVr9Bg1OWI_A"
# old https://api.mapbox.com/styles/v1/edgarscrase/cjl1c78tn37v72sofn80jo9en.html?fresh=true&title=true&access_token=pk.eyJ1IjoiZWRnYXJzY3Jhc2UiLCJhIjoiY2pram90c3M3MWRxdjNxcWhzOXRzY3N6ZCJ9._HQxSBcViAYVr9Bg1OWI_A#3.4/57.750060/17.037912/0
# mapbox://styles/edgarscrase/cjl1c78tn37v72sofn80jo9en

# Try using leaflet but actually this is not going to look that clean!
m <- leaflet(dcvMap)  %>% 
  addTiles(urlTemplate = mapBoxURL,
           attribution="MapBox") %>%
  setView(5, 50, zoom = 4) %>% 
  addCircleMarkers(~longitude, ~latitude, popup=dcvMap$name, weight = 3, radius=round(radiusFactor * log10(dcvMap$CountForOrder),0), 
                   color="#e77b37", stroke = F, fillOpacity = 0.5) 

# Magic.  we got there finally - after 750 lines of code - lets take a screenshot and print out the map using the MapView library
mapshot(m,file="ScreenShot.png")
# and then read it back in.
mapImage <- readPNG("ScreenShot.png")

m

# Add the map image to another plot
plotMap <- qplot(1,1) + annotation_custom(rasterGrob(mapImage)) +
  labs(title="Number of decisions on asylum applications in Europe by country", 
     y="", 
     x="", 
     fill="", 
     caption="", 
     family=fontsForCharts) +
  # set a very minimal theme
  theme_minimal(base_family=fontsForCharts) +   
  # These two lines tweak the positioning of the legend and hide the x axis ticks need to go AFTER the call to theme_minimal
  theme(
    axis.ticks = element_blank(),  
    axis.line = element_blank(),
    axis.text.x=element_blank(), 
    axis.text.y=element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


plotMap



#-------------------------------------------------------------------------------------------------------
# Finally - output - lets bring it all together!

# Try summary chart and map and then the two detailed charts
firstRow <- grid.arrange(plot1, plotMap, ncol=2)
grid.arrange(firstRow, plot2, plot3, nrow=3)

# Other alternatives
chartsCol <- grid.arrange(plot1, plot2, plot3, nrow=3)
grid.arrange(plotMap, chartsCol, ncol=2)

grid.arrange(plot1, plotMap, plot2, plot3, nrow=4)

