# The package estimates the home ranges size for each of the files in a folder:

homelist <- function(file_path,lon, lat, groups,
                          crs_n, h, p, unit_area) {
  # Reading files from a folder
  file_paths <- fs::dir_ls(file_path)
  file_paths
  #prepare function
  file_contents <- list()
  for (k in seq_along(file_paths)){
    file_contents[[k]] <- read.csv(
      file = file_paths[[k]]
    )
    # Subset each of the file in the folder
    dfile <- file_contents[[1]]

    # Changing date format
    library(lubridate)
    dfile$Date <- as.Date(dfile$Date, format = "%Y-%m-%d")

    # Creating new columns for day, month, and year
    dfile$day <- day(dfile$Date)
    dfile$month <- month(dfile$Date)
    dfile$year <- year(dfile$Date)

    # Subseting data
    dfile2 <- dfile[, c(lon, lat, groups)]
    names(dfile2) <- c("x", "y", "id")

    library(sp)
    library(sf)
    library(ade4)
    library(adehabitatMA)
    library(CircStats)
    library(adehabitatLT)
    library(adehabitatHR)

    # Setting names to the outputs
    homerange = c()

    # Use sp library to assign coordinates and projection
    coordinates(dfile2) <- c("x", "y")

    # Assign a crs projection
    sf_file <- st_as_sf(dfile2, coords=c("x", "y"), crs = crs_n)
    sf_points <- as(sf_file, "Spatial")

    #Calculations
    # Estimating the utilization distribution using "reference" bandwidth
    kud_points <- kernelUD(sf_points, h=h)

    # Display the utilization distribution
    image(kud_points)

    #Get the Volume
    vud_points <- getvolumeUD(kud_points)

    # Estimate the homerange from the utilization distribution
    homerange <- getverticeshr(kud_points, p, unout=unit_area)
    home1 <- kernel.area(kud_points, p, unout = unit_area)
    home2 <- as.data.frame(homerange)

    #Plotting
    image(vud_points)

    #Getting values
    homerange$Month <- paste(dfile$month)
    homerange$Year <- paste(dfile$year)
    rhom <- rbind(homerange)
    return(rhom)
  }
}
