\name{ExampleSonde}
\alias{ExampleSonde}
\title{Example Sonde/Balloon/Profiler Data}

\description{
This data was collected by a radiosonde which was carried aloft by a 
weather balloon. This data was collected as part of a special field
program, but the instrument and data formats are \bold{very} common 
in the atmospheric community.\cr

Fundamentally; temperature, pressure, and humidity sensors are bundled 
with a radio transmitter and are either sent aloft on a balloon 
(rawindsonde) or tied to a small parachute and dropped from a plane 
(dropsonde). Either way, the location of the sonde is observed 
(either visually or with GPS), the data from the radio transmitter 
recorded, and the result is an ascii file that contains a header 
(of varying length) with descriptive information followed by a 
table of information.\cr

For rawindsondes, the first entry for the table is actually the 
surface data from the weather station, but the remaining entries 
are from the sonde. Also, the wind is inferred from the position 
of the balloon. Since this is \bold{real} data, there are missing
values -- which arise \emph{all the time}. If you get a profile
without missing values, you can be sure it is synthetic or interpolated.
The datastreme ends when the operator calls it quits. This happens when
a) the balloon breaks, or b) the radio reception has so many dropouts that
is is not worth it anymore. Different fields have different \code{missing}
flags. Where possible, we replace the "standard" missing flags with 
\code{NaN}
}

\format{
  \code{ExampleSonde} is the nominal output and is a mixture of ancillary
data (contained in the first ???? lines of the file), and the instrument 
measures and some derived fields that follow in a fixed-format table
with an unknown number of lines.

\tabular{lllllll}{
Field \tab varname \tab width \tab format \tab description \tab units \tab missing value \cr
 [ 1] \tab time    \tab 6 \tab F6.1 \tab Time                       \tab Seconds          \tab 9999.0 \cr
 [ 2] \tab press   \tab 6 \tab F6.1 \tab Pressure                   \tab Millibars        \tab 9999.0 \cr
 [ 3] \tab temp    \tab 5 \tab F5.1 \tab Dry-bulb Temperature       \tab Degrees C        \tab 999.0 \cr
 [ 4] \tab dewpt   \tab 5 \tab F5.1 \tab Dew Point Temperature      \tab Degrees C        \tab 999.0 \cr
 [ 5] \tab rhum    \tab 5 \tab F5.1 \tab Relative Humidity          \tab Percent          \tab 999.0 \cr
 [ 6] \tab uwind   \tab 6 \tab F6.1 \tab U Wind Component           \tab Meters/Second    \tab 9999.0 \cr
 [ 7] \tab vwind   \tab 6 \tab F6.1 \tab V Wind Component           \tab Meters/Second    \tab 9999.0 \cr
 [ 8] \tab wspd    \tab 5 \tab F5.1 \tab Wind Speed                 \tab Meters/Second    \tab 999.0 \cr
 [ 9] \tab wdir    \tab 5 \tab F5.1 \tab Wind Direction             \tab Degrees          \tab 999.0 \cr
 [10] \tab dz      \tab 5 \tab F5.1 \tab Ascension Rate             \tab Meters/Second    \tab 999.0 \cr
 [11] \tab lon     \tab 8 \tab F8.3 \tab Longitude                  \tab Degrees          \tab 9999.0 \cr
 [12] \tab lat     \tab 7 \tab F7.3 \tab Latitude                   \tab Degrees          \tab 999.0 \cr
 [13] \tab range   \tab 5 \tab F5.1 \tab Variable (see below)       \tab                  \tab 999.0 \cr
 [14] \tab angle   \tab 5 \tab F5.1 \tab Variable (see below)       \tab                  \tab 999.0 \cr
 [15] \tab alt     \tab 7 \tab F7.1 \tab Altitude                   \tab Meters           \tab 99999.0 \cr
 [16] \tab qp      \tab 4 \tab F4.1 \tab QC flag for Pressure       \tab Code (see below) \tab 99.0 \cr
 [17] \tab qt      \tab 4 \tab F4.1 \tab QC flag for Temperature    \tab Code (see below) \tab 99.0 \cr
 [18] \tab qh      \tab 4 \tab F4.1 \tab QC flag for Humidity       \tab Code (see below) \tab 99.0 \cr
 [19] \tab qu      \tab 4 \tab F4.1 \tab QC flag for U Component    \tab Code (see below) \tab 99.0 \cr
 [20] \tab qv      \tab 4 \tab F4.1 \tab QC flag for V Component    \tab Code (see below) \tab 99.0 \cr
 [21] \tab quv     \tab 4 \tab F4.1 \tab QC flag for Ascension Rate \tab Code (see below) \tab 99.0 \cr
}

The \code{missing value} flags are in brackets [].
Fields 13 and 14 are `variable' because depending on the sounding
system the variables used in these positions can vary.\cr

Fields 16 through 21 contain the Quality Control information (flags) 
generated at JOSS. These flags are based on the automated or 
visual checks made.\cr

The JOSS QC flag codes are as follows:\cr
\item{99.0}{Unchecked (QC information is `missing.')  (`UNCHECKED')}
\item{1.0}{Checked, datum seems physically reasonable.  (`GOOD')}
\item{2.0}{Checked, datum seems questionable on physical basis.  (`MAYBE')}
\item{3.0}{Checked, datum seems to be in error.  (`BAD')}
\item{4.0}{Checked, datum is interpolated.  (`ESTIMATED')}
\item{9.0}{Checked, datum was missing in original file.  (`MISSING')}

}

\source{
  \url{http://www.joss.ucar.edu/cgi-bin/codiac/dss?2.151}
}

\seealso{ \code{\link{getsonde}},
          \code{\link{plotsonde}},
          \code{\link{skewt.lines}},
          \code{\link{skewt.points}} }

\examples{
filename <- system.file("exampleData", "ExampleSonde.txt", package = "RadioSonde")   
sample.sonde <- getsonde(filename)
}

\keyword{datasets}
