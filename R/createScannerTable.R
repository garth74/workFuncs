# install.packages("jsonlite")
# install.packages("stringi")
# install.packages("data.table")
# install.packages("fs")
# install.packages("rio")


# ============================================================================ #
# Helper Functions


.read_utf16_json <- function(path, ...) {
  stringi::stri_read_lines(
    con = path,
    encoding = "UTF-16LE"
  ) |>
    stringi::stri_join(collapse = "\n") |>
    jsonlite::parse_json()
}

.getTagNames <- function(tagObj) {
  tagNames <- names(tagObj)
  tagNames <- tagNames[tagNames != "Tag n."]
  as.character(sort(as.numeric(tagNames)))
}

.getFileName <- function(file) {
  gsub("[\\d_]+", "", as.character(fs::path_ext_remove(fs::path_file(file))), perl = TRUE)
}

#' @import data.table
.readData <- function(file) {
  data <- .read_utf16_json(file)
  tags <- data$OBJECTS
  header <- tags$`Tag n.`
  tags$`Tag n.` <- NULL

  ordered_tags <- unlist(lapply(.getTagNames(tags), function(n) tags[[n]]))
  text <- paste(c(header, ordered_tags), collapse = "\n")
  df <- data.table::fread(text = text)
  df[, Source := .getFileName(file)][]
}

.convertToInches <- function(vec) {
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

.getAvgCalcDepth <- function(df) {
  mean(.convertToInches(df[, "Calculated Depth [in]"][[1]]))
}

#' @import data.table
.getDistances <- function(df) {
  if (is.numeric(df[, "Distance X [ft]"][[1]])) {
    # horizontal data
    colName <- "Distance X [ft]"
  } else {
    colName <- "Distance Y [ft]"
  }
  .convertToInches(df[, ..colName][[1]])
}


.getDifferences <- function(df) {
  distance <- .getDistances(df)
  c(distance[1], distance[-1] - distance[-(length(distance))])
}

.getStuffCustomerWants <- function(df) {
  diffs <- .getDifferences(df)
  list(
    Axis = ifelse(is.numeric(df[, "Distance X [ft]"][[1]]), "Horizontal", "Vertical"),
    `Scan Location` = tools::toTitleCase(df[, "Source"][[1]][1]),
    Start = diffs[1],
    `Average Calculated Depth` = round(.getAvgCalcDepth(df), 2),
    `Average Spacing` = round(mean(diffs[-1]), 2),
    Spacing = paste0(round(diffs[-1], 2), collapse = ", "),
    Distances = paste0(.getDistances(df), collapse = ", ")
  )
}

.writeOutputs <- function(df, directoryPath, overwrite) {
  outputFile <- fs::path(directoryPath, "scans.xlsx")
  if (isFALSE(overwrite) && fs::file_exists(outputFile)) {
    stop(sprintf("%s already exists!", outputFile))
  }
  rio::export(df, outputFile)
  outputFile <- fs::path(directoryPath, "scans.tsv")
  if (isFALSE(overwrite) && fs::file_exists(outputFile)) {
    stop(sprintf("%s already exists!", outputFile))
  }
  rio::export(df, outputFile)
}


#' @export
createScansTable <- function(folderName = "scans to run", overwrite = FALSE) {
  directoryPath <- fs::path_expand(fs::path("~", "Desktop", folderName))
  outputFile <- fs::path(directoryPath, "scans.xlsx")
  if (isFALSE(overwrite) && fs::file_exists(outputFile)) {
    stop(sprintf("%s already exists!", outputFile))
  }
  data <- lapply(list.files(directoryPath, "*.json", full.names = TRUE), .readData)
  df <- data.table::rbindlist(lapply(data, .getStuffCustomerWants))
  df <- df[order(df$Axis, df$`Scan Location`)]
  .writeOutputs(df, directoryPath, overwrite)
}
