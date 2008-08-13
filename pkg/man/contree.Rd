\name{contree}
\alias{contree}
\title{MrBayes Contree Plot}
\description{
	
	Reads in a .con file from the sumt command in MrBayes and plots the  consensus tree with posterior probabilities with an option to save it to a .pdf file.

  }
\usage{
contree(confile = confile, brlens = F, savepdf = F)
}

\arguments{
  \item{confile}{ A .con file from MrBayes in the working directory. }
  \item{brlens}{ If brlens=T then uses branchlengths from file.  If brlens=F then plots ultrametric tree with grafen branchlengths.  }
  \item{savepdf}{ If savepdf=T then the plot is saved to a .pdf named "file name.pdf" }
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{A plot.}
  \item{comp2 }{A plot sent to .pdf.}
  ...
}
\references{ Ronquist F. and Huelsenbeck J.P. (2003)  MrBayes 3: Bayesian phylogenetic inference under mixed models.  Bioinformatics 19(12)1572-1574}
\author{ Stacey D. Smith \email{sds21@duke.edu} and Zofia A. Kaliszewska \email{zakalisz@fas.harvard.edu} }

\examples{
	
	contree(confile="carnivore.con", brlens=F, savepdf=T)
	
	}
\keyword{ print }
\keyword{ hplot }
