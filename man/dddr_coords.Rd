% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coord-look.R
\name{dddr_coords}
\alias{dddr_coords}
\alias{coord_look_at}
\alias{coord_look_at_front}
\alias{coord_look_at_back}
\alias{coord_look_at_top}
\alias{coord_look_at_bottom}
\alias{coord_look_at_port}
\alias{coord_look_at_starboard}
\title{Spatial Plotting (Coordinates)}
\usage{
coord_look_at(direction, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")

coord_look_at_front(...)

coord_look_at_back(...)

coord_look_at_top(...)

coord_look_at_bottom(...)

coord_look_at_port(...)

coord_look_at_starboard(...)
}
\arguments{
\item{direction}{String representing the face being looked at.}

\item{xlim}{Limits for the x and y axes.}

\item{ylim}{Limits for the x and y axes.}

\item{expand}{If \code{TRUE}, the default, adds a small expansion factor to
the limits to ensure that data and axes don't overlap. If \code{FALSE},
limits are taken exactly from the data or \code{xlim}/\code{ylim}.}

\item{clip}{Should drawing be clipped to the extent of the plot panel? A
setting of \code{"on"} (the default) means yes, and a setting of \code{"off"}
means no. In most cases, the default of \code{"on"} should not be changed,
as setting \code{clip = "off"} can cause unexpected results. It allows
drawing of data points anywhere on the plot, including in the plot margins. If
limits are set via \code{xlim} and \code{ylim} and some data points fall outside those
limits, then those data points may show up in places such as the axes, the
legend, the plot title, or the plot margins.}

\item{...}{Values passed along to `coord_look_at`}
}
\description{
To render the view of some spatial object, one must select the way to view
it. This is performed using a ggplot2 coord.
}
