readTOA5 <- function (filename, nlabelline = 4, dateTimeMethod = c("lubridate", 
                                                       "POSIXct", "none"), posixctFormat = "%Y-%m-%d %H:%M", addFileName = TRUE, 
          rmDupDateTime = FALSE, ...) 
{
  dateTimeMethod <- match.arg(dateTimeMethod)
  getData <- function(fn) {
    h <- readLines(fn, n = nlabelline)
    h <- gsub(",([0-9])", "\\1", h)
    dat <- as.data.frame(fread(fn, skip = nlabelline, header = FALSE, 
                               stringsAsFactors = TRUE, na.strings = c("NAN", "NA", 
                                                                       "\"NAN\"", "\"NA\"")))
    colnames <- gsub("\"", "", strsplit(paste(h[2], collapse = ""), 
                                        ",")[[1]])
    names(dat) <- make.names(colnames)
    names(dat)[1] <- "DateTime"
    return(dat)
  }
  dat <- try(getData(filename))
  if (inherits(dat, "try-error")) {
    message("Could not read file ", filename, ". It is probably not TOA5 or otherwise corrupted.\n Returning NULL.")
    return(invisible(NULL))
  }
  if (dateTimeMethod != "none") {
    if (dateTimeMethod == "lubridate") 
      dat$DateTime <- ymd_hms(dat$DateTime, tz = "UTC", 
                              truncated = 1)
    if (dateTimeMethod == "POSIXct") 
      dat$DateTime <- as.POSIXct(dat$DateTime, format = posixctFormat, 
                                 tz = "UTC")
    dat$Date <- as.Date(dat$DateTime)
    if (rmDupDateTime) 
      dat <- dat[!duplicated(dat$DateTime), ]
  }
  else {
    dat$Date <- NA
  }
  if (addFileName) 
    dat$Source <- basename(filename)
  return(dat)
}