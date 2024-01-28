install.packages("usethis",repos = "http://cran.us.r-project.org")
usethis::use_mit_license()

#' Making a barplot from a random sample
#'
#' @param n A number.
#' @param iter A number.
#' @param time A number.
#' @returns A numeric vector.
#' @examples
#' myFunction(1, 1,1)
#' myFunction(10, 10,10)
myFunction <- function(n=5, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )
    print("It worked!")
    #release the table
    Sys.sleep(time)
  }
}

#' World Health Organization TB data
#'
#' A subset of data from the World Health Organization Global Tuberculosis
#' Report ...
#'
#' @format ## `who`
#' A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{iso2, iso3}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
#' @source <https://www.who.int/teams/global-tuberculosis-programme/data>
"fire"

usethis::use_data(fire)
