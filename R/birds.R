#' Birds
#'
#' @description Detection / non-detection records of Indigo Bunting (Passerina Cyanea) on bird searches conducted in the conterminous United States during the year 2010 as submitted to eBird.org.
#'
#' @usage birds
#'
#' @format A data frame with 660 observations on 24 variables:
#' \describe{
#'   \item{detected}{Indigo Bunting detected on search?  1 - yes; 0 - no}
#'   \item{stationary}{Search protocol: 1 - stationary; 0 - traveling}
#'   \item{year}{Year of search}
#'   \item{month}{Month of year of search (1-12)}
#'   \item{time}{Time of search (24 hours)}
#'   \item{hours}{Number of hours spent searching for birds}
#'   \item{distance}{Distance traveled on the search (km)}
#'   \item{observers}{Number of observers present on the search}
#'   \item{elevation}{Elevation of search location (m)}
#'   \item{openwater}{Percentage of open water coverage}
#'   \item{perennialicesnow}{Percentage of perennial ice/snow coverage}
#'   \item{dev_open}{Percentage of developed, open space coverage}
#'   \item{dev_low}{Percentage of developed, low intensity coverage}
#'   \item{dev_medium}{Percentage of developed, medium intensity coverage}
#'   \item{dev_high}{Percentage of developed, high intensity coverage}
#'   \item{barren}{Percentage of barren land coverage}
#'   \item{deciduous}{Percentage of deciduous forest coverage}
#'   \item{evergreen}{Percentage of evergreen forest coverage}
#'   \item{mixed}{Percentage of mixed forest coverage}
#'   \item{scrubscrub}{Percentage of shrub/scrub coverage}
#'   \item{grasslandherbaceous}{Percentage of grassland/herbaceous coverage}
#'   \item{pasturehay}{Percentage of pasture/hay coverage}
#'   \item{cultivatedcrops}{Percentage of cultivated crops coverage}
#'   \item{woodywetlands}{Percentage of woody wetlands coverage}
#'   \item{emergentherbaceouswetlands}{Percentage of emergent herbaceous Wetlands coverage}
#' }
#'
#' @details This dataset contains a subset of detection / non-detection records of Indigo Bunting sightings in the conterminous United States on searches conducted between January 1 and December 31, 2010.  Data was collected as part of the eBird project (Sullivan et al. (2014); \url{http://ebird.org/content/ebird/about/}) hosted at the Cornell University Lab of Ornithology. 50 observations were randomly sampled from each month of 2010, after observations with NAs were removed.
#'
#' The binary response variable (detected) was associated with 23 potential explanatory variables. The effort variables (hours, distance, and observers) capture variation in detection rates. The variable time captures diurnal variation in bird behavior that makes species more or less conspicuous for detection. The stationary indicator captures systematic differences between the two search protocols. Temporal information comes from the month of the year on which the search was conducted. 
#'
#' To account for habitat-selectivity each eBird location was linked with a set of remotely-sensed elevation and land cover variables.  The U.S. 2006 National Land Cover Database (NLCD) classifies vegetation into one of 16 classes. Each NLCD land cover class was summarized as the percentage of each of the land cover in the 1.5km x 1.5km pixel (225 ha) centered at the search location.
#' The full version of this dataset consisting of more than 400,000 samples was used in Mentch and Hooker (2014) to build confidence intervals and test feature significance with bagged trees and random forests.
#'
#' @source This and more related data from the eBird project can be found at ebird.org.  NLCD variables and descriptions can be found at \url{http://www.mrlc.gov/nlcd2006.php}. A complete description of the elevation variable can be found at \url{http://eros.usgs.gov/#/Find_Data/Products_and_Data_Available/gtopo30_info}
#'
#' @references Mentch, Lucas, and Giles Hooker. (2014). Quantifying Uncertainty in Random Forests via Confidence Intervals and Hypothesis Tests. arXiv preprint arXiv:1404.6473. 
#' 
#' Sullivan, Brian .L, Jocelyn L. Aycrigg, Jessie H. Barry, Rick E. Bonney, Nicholas Bruns, Caren B. Cooper, Theo Damoulas, Andre A. Dhondt, Tom Dietterich, Andrew Farnsworth, Daniel Finka, John W. Fitzpatrick, Thomas Fredericks, Jeff Gerbracht, Carla Gomes, Wesley M. Hochachka, Marshall J. Iliff, Carl Lagoze, Frank A. La Sorte, Matthew Merrifield, Will Morris, Tina B. Phillips, Mark Reynolds, Amanda D. Rodewald, Kenneth V. Rosenberg, Nancy M. Trautmann, Andrea Wiggins, David W. Winkler, Weng-Keen Wong, Christopher L. Wood, Jun Yu, Steve Kelling. (2014). The eBird enterprise: An integrated approach to development and application of citizen science. Biological Conservation, 169(January), 31-40. \url{http://dx.doi.org/10.1016/j.biocon.2013.11.003}
"birds"