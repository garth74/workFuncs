# install.packages("jsonlite")
# install.packages("stringi")
# install.packages("data.table")
# install.packages("fs")
# install.packages("rio")


# ============================================================================ #
# Helper Functions


#' @export
getFiles <- function() {
  pathsToCheck <- c(
    fs::path_expand(fs::path("~", "Desktop", "scans to run")),
    fs::path_expand(fs::path("~", "OneDrive", "Desktop", "scans to run"))
  )
  for (i in seq_along(pathsToCheck)) {
    path <- pathsToCheck[i]
    result <- list.files(path, "*.json", full.names = TRUE)
    if (length(result)) {
      return(result)
    }
  }
  stop(sprintf("Unable to find files. Checked in: \n%s", paste0(pathsToCheck, collapse = "\n")))
}

#' @export
#' @import data.table
readData <- function(file) {
  data <- read_utf16_json(file)
  tags <- data$OBJECTS
  header <- tags$`Tag n.`
  tags$`Tag n.` <- NULL

  ordered_tags <- unlist(lapply(getTagNames(tags), function(n) tags[[n]]))
  text <- paste(c(header, ordered_tags), collapse = "\n")
  df <- data.table::fread(text = text)
  df[, Source := getFileName(file)][]
}


#' @export
read_utf16_json <- function(path, ...) {
  stringi::stri_read_lines(
    con = path,
    encoding = "UTF-16LE"
  ) |>
    stringi::stri_join(collapse = "\n") |>
    jsonlite::parse_json()
}

#' @export
getTagNames <- function(tagObj) {
  tagNames <- names(tagObj)
  tagNames <- tagNames[tagNames != "Tag n."]
  as.character(sort(as.numeric(tagNames)))
}
#' @export
getFileName <- function(file) {
  gsub("[\\d_]+", "", as.character(fs::path_ext_remove(fs::path_file(file))), perl = TRUE)
}
#' @export
convertToInches <- function(vec) {
  if (is.character(vec)) {
    as.numeric(lapply(strsplit(vec, "['\"] ?", perl = TRUE), function(x) {
      x <- as.numeric(x)
      if (length(x) == 2) x[1] <- x[1] * 12
      sum(x)
    }))
  } else {
    vec
  }
}
#' @export
getAvgCalcDepth <- function(df) {
  mean(convertToInches(df[, "Calculated Depth [in]"][[1]]))
}
#' @export
#' @import data.table
getDistances <- function(df) {
  if (is.numeric(df[, "Distance X [ft]"][[1]])) {
    # horizontal data
    colName <- "Distance X [ft]"
  } else {
    colName <- "Distance Y [ft]"
  }
  convertToInches(df[, ..colName][[1]])
}

#' @export
getDifferences <- function(df) {
  distance <- getDistances(df)
  c(distance[1], distance[-1] - distance[-(length(distance))])
}

#' @export
getStuffCustomerWants <- function(df, file) {
  diffs <- getDifferences(df)
  list(
    Axis = tools::toTitleCase(stringi::stri_extract_first_regex(file, "(horizontal|vertical)")),
    `Scan Location` = tools::toTitleCase(df[, "Source"][[1]][1]),
    Start = diffs[1],
    `Average Calculated Depth` = round(getAvgCalcDepth(df), 2),
    `Average Spacing` = round(mean(diffs[-1]), 2),
    Spacing = paste0(round(diffs[-1], 2), collapse = ", "),
    Distances = paste0(getDistances(df), collapse = ", ")
  )
}
#' @export
writeOutputs <- function(df, directoryPath, overwrite) {
  outputFile <- fs::path(directoryPath, "scans.xlsx")
  if (isFALSE(overwrite) && fs::file_exists(outputFile)) {
    stop(sprintf("%s already exists!", outputFile))
  }
  rio::export(df, outputFile)
}
#' @export
createScansTable <- function(nodickplz = FALSE, folderName = "scans to run", overwrite = FALSE) {
  files <- getFiles()
  data <- lapply(files, readData)
  df <- data.table::data.table(t(mapply(getStuffCustomerWants, data, files)))[, lapply(.SD, "[[", 1), by = .I][, I := NULL]
  df <- df[order(df$Axis, df$`Scan Location`)]
  writeOutputs(df, fs::path_dir(files[1]), overwrite)
  if (!nodickplz) {
    cat("
    All done!

    ⣿⣿⣿⣿⣿⣿⣿⣿⣿⠟⠛⢉⢉⠉⠉⠻⣿⣿⣿⣿⣿⣿
    ⣿⣿⣿⣿⣿⣿⣿⠟⠠⡰⣕⣗⣷⣧⣀⣅⠘⣿⣿⣿⣿⣿
    ⣿⣿⣿⣿⣿⣿⠃⣠⣳⣟⣿⣿⣷⣿⡿⣜⠄⣿⣿⣿⣿⣿
    ⣿⣿⣿⣿⡿⠁⠄⣳⢷⣿⣿⣿⣿⡿⣝⠖⠄⣿⣿⣿⣿⣿
    ⣿⣿⣿⣿⠃⠄⢢⡹⣿⢷⣯⢿⢷⡫⣗⠍⢰⣿⣿⣿⣿⣿
    ⣿⣿⣿⡏⢀⢄⠤⣁⠋⠿⣗⣟⡯⡏⢎⠁⢸⣿⣿⣿⣿⣿
    ⣿⣿⣿⠄⢔⢕⣯⣿⣿⡲⡤⡄⡤⠄⡀⢠⣿⣿⣿⣿⣿⣿
    ⣿⣿⠇⠠⡳⣯⣿⣿⣾⢵⣫⢎⢎⠆⢀⣿⣿⣿⣿⣿⣿⣿
    ⣿⣿⠄⢨⣫⣿⣿⡿⣿⣻⢎⡗⡕⡅⢸⣿⣿⣿⣿⣿⣿⣿
    ⣿⣿⠄⢜⢾⣾⣿⣿⣟⣗⢯⡪⡳⡀⢸⣿⣿⣿⣿⣿⣿⣿
    ⣿⣿⠄⢸⢽⣿⣷⣿⣻⡮⡧⡳⡱⡁⢸⣿⣿⣿⣿⣿⣿⣿
    ⣿⣿⡄⢨⣻⣽⣿⣟⣿⣞⣗⡽⡸⡐⢸⣿⣿⣿⣿⣿⣿⣿
    ⣿⣿⡇⢀⢗⣿⣿⣿⣿⡿⣞⡵⡣⣊⢸⣿⣿⣿⣿⣿⣿⣿
    ⣿⣿⣿⡀⡣⣗⣿⣿⣿⣿⣯⡯⡺⣼⠎⣿⣿⣿⣿⣿⣿⣿
    ⣿⣿⣿⣧⠐⡵⣻⣟⣯⣿⣷⣟⣝⢞⡿⢹⣿⣿⣿⣿⣿⣿
    ⣿⣿⣿⣿⡆⢘⡺⣽⢿⣻⣿⣗⡷⣹⢩⢃⢿⣿⣿⣿⣿⣿
    ⣿⣿⣿⣿⣷⠄⠪⣯⣟⣿⢯⣿⣻⣜⢎⢆⠜⣿⣿⣿⣿⣿
    ⣿⣿⣿⣿⣿⡆⠄⢣⣻⣽⣿⣿⣟⣾⡮⡺⡸⠸⣿⣿⣿⣿
    ⣿⣿⡿⠛⠉⠁⠄⢕⡳⣽⡾⣿⢽⣯⡿⣮⢚⣅⠹⣿⣿⣿
    ⡿⠋⠄⠄⠄⠄⢀⠒⠝⣞⢿⡿⣿⣽⢿⡽⣧⣳⡅⠌⠻⣿
    ⠁⠄⠄⠄⠄⠄⠐⡐⠱⡱⣻⡻⣝⣮⣟⣿⣻⣟⣻⡺⣊
    ")
  }
}
