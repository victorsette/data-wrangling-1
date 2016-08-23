# loading dplyr library
library("dplyr")

# 0: load the data into RStudio
refine_df <- read.csv("refine_original.csv", stringsAsFactors = FALSE)
wdf <- tbl_df(refine_df)

# 1: Clean up brand names
wdf$company <- sub("*.*.ps", "philips", wdf$company, ignore.case = TRUE)
wdf$company <- sub("^ak*.*.", "akzo", wdf$company, ignore.case = TRUE)
wdf$company <- sub("^van*.*.", "van houten", wdf$company, ignore.case = TRUE)
wdf$company <- sub("*.*.ver", "unilever", wdf$company, ignore.case = TRUE)

# 2: Separate product code and number
wdf <- mutate(wdf, product_code = substr(wdf$Product.code...number, 1, 1))

wdf <- mutate(wdf, product_number = substr(wdf$Product.code...number, 3,
                                           length(wdf$Product.code...number)))

# 3: Add product categories
# define a function that returns the category according to the code
product_category <- function(code) {
  if (code == "p"){
    return ("smartphone")
  } else if (code == "v") {
    return ("tv")
  } else if (code == "x") {
    return ("laptop")
  } else if (code == "q") {
    return ("tablet")
  } else {
    return ("NA")
  }
}

# add new column with the evaluation of the category function
# using lapply brought problems when writing the output csv, so I changed to sapply
wdf <- mutate(wdf, category = sapply(wdf$product_code, product_category))


# 4: Add full address for geocoding
wdf <- mutate(wdf, full_address = paste(wdf$address, wdf$city, 
                                        wdf$country, sep = ", "))

# 5: Create dummy variables for company and product category

# companies
wdf <- mutate(wdf, company_philips = (wdf$company == "philips"))
wdf <- mutate(wdf, company_akzo = (wdf$company == "akzo"))
wdf <- mutate(wdf, company_van_houten = (wdf$company == "van houten"))
wdf <- mutate(wdf, company_unilever = (wdf$company == "unilever"))

# products
wdf <- mutate(wdf, product_smartphone = (wdf$category == "smartphone"))
wdf <- mutate(wdf, product_tv = (wdf$category == "tv"))
wdf <- mutate(wdf, product_laptop = (wdf$category == "laptop"))
wdf <- mutate(wdf, product_tablet = (wdf$category == "tablet"))

# 6: writing on a new file the clean data
write.csv2(wdf, file = "refine_clean.csv")
