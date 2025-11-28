#' Detect Country Names in Text
#'
#' @param text A character string containing the text to search for country names
#' @param return_all Logical. If TRUE, returns all found countries. If FALSE, returns only the first match. Default is TRUE.
#' @param ignore_case Logical. If TRUE, performs case-insensitive matching. Default is TRUE.
#'
#' @return A character vector of detected country names, or NULL if no countries found
#' @export
#'
#' @examples
#' country_name("I visited France and Germany last summer")
#' country_name("The conference will be held in Japan")
country_name <- function(text, return_all = TRUE, ignore_case = TRUE) {

  # Validate input
  if (!is.character(text) || length(text) == 0) {
    stop("Input must be a non-empty character string")
  }

  # List of countries (all UN member states + some territories)
  countries <- c(
    "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda",
    "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain",
    "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
    "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria",
    "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada",
    "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros",
    "Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Czechia",
    "Democratic Republic of the Congo", "Denmark", "Djibouti", "Dominica",
    "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea",
    "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France",
    "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada",
    "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hungary",
    "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy",
    "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati",
    "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho",
    "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar",
    "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands",
    "Mauritania", "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco",
    "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia",
    "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria",
    "North Korea", "North Macedonia", "Norway", "Oman", "Pakistan", "Palau",
    "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines",
    "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis",
    "Saint Lucia", "Saint Vincent and the Grenadines", "Samoa", "San Marino",
    "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles",
    "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", "Somalia",
    "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka", "Sudan",
    "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania",
    "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia",
    "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates",
    "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu",
    "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe",
    # Common alternative names
    "USA", "UK", "UAE", "DRC", "South Korea", "North Korea"
  )

  # Find matching countries
  found_countries <- c()

  for (country in countries) {

    if (ignore_case) {
      if (grepl(country, text, ignore.case = TRUE)) {
        found_countries <- c(found_countries, country)
      }
    } else {
      if (grepl(country, text)) {
        found_countries <- c(found_countries, country)
      }
    }
  }

  if (length(found_countries) == 0) {
    return(NULL)
  }

  if (return_all) {
    return(found_countries)
  } else {
    return(found_countries[1])
  }
}

# Example usage:
# text1 <- "I have traveled to France, Germany, and Japan."
# country_name(text1)
#
# text2 <- "The United States and Canada are in North America."
# country_name(text2)
#
# text3 <- "No countries mentioned here."
# country_name(text3)

#' Count Number of Countries Mentioned in Text
#'
#' @param text A character string containing the text to search for country names
#' @param ignore_case Logical. If TRUE, performs case-insensitive matching. Default is TRUE.
#'
#' @return An integer representing the number of unique countries found
#' @export
#'
#' @examples
#' howmany("I visited France and Germany last summer")
#' howmany("The conference will be held in Japan")
howmany <- function(text, ignore_case = TRUE) {

  # Use the country_name function to get all countries
  found <- country_name(text, return_all = TRUE, ignore_case = ignore_case)

  # Return the count
  if (is.null(found)) {
    return(0)
  } else {
    return(length(found))
  }
}

# Example usage:
# text1 <- "I have traveled to France, Germany, and Japan."
# country_name(text1)
# howmany(text1)  # Returns: 3
#
# text2 <- "The United States and Canada are in North America."
# country_name(text2)
# howmany(text2)  # Returns: 2
#
# text3 <- "No countries mentioned here."
# country_name(text3)
# howmany(text3)  # Returns: 0
