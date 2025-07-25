% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RUI.R
\name{runModel}
\alias{runModel}
\title{Run Model}
\usage{
runModel(
  workspace,
  mod,
  X,
  na.value = -666.666,
  run = TRUE,
  preClean = FALSE,
  dir.exe = .BAM_PATH,
  name.exe = "BaM",
  stout = "",
  Inputs_fname = "Config_Inputs.txt",
  X_fname = "X.txt",
  Y_fname = "Y.txt"
)
}
\arguments{
\item{workspace}{Character, directory where config and result files are stored.
workspace = tempdir() is recommended.}

\item{mod}{model object, the model to be run.}

\item{X}{data frame, containing the inputs of the model}

\item{na.value}{numeric, value used by BaM to denote impossible runs, that will be changed to NA in RBaM.}

\item{run}{Logical, run the model? if FALSE, just write config files and returns NULL.}

\item{preClean}{Logical, start by cleaning up workspace?
Be careful, this will delete all files in the workspace, including old results!}

\item{dir.exe}{Character, directory where BaM executable stands.}

\item{name.exe}{Character, name of the executable without extension ('BaM' by default).}

\item{stout}{Character string, standard output (see ?system2).
In particular, stout="" (default) shows BaM messages in the console,
stout=NULL discards BaM messages, stout='log.txt' saves BaM messages in file "log.txt".}

\item{Inputs_fname}{Character, name of configuration file used to specify the format of X.}

\item{X_fname}{Character, name of file containing a copy of X.}

\item{Y_fname}{Character, name of file where simulations are written.}
}
\value{
A data frame containing the outputs simulated by the model.
}
\description{
Perform a single run of a model, using the initial values specified for the
model's parameters.
}
\examples{
# Rating curve model - see https://github.com/BaM-tools/RBaM
# Parameters of the low flow section control: activation stage k, coefficient a and exponent c
k1=parameter(name='k1',init=-0.5)
a1=parameter(name='a1',init=50)
c1=parameter(name='c1',init=1.5)
# Parameters of the high flow channel control: activation stage k, coefficient a and exponent c
k2=parameter(name='k2',init=1)
a2=parameter(name='a2',init=100)
c2=parameter(name='c2',init=1.67)
# Define control matrix: columns are controls, rows are stage ranges.
controlMatrix=rbind(c(1,0),c(0,1))
# Stitch it all together into a model object
M=model(ID='BaRatin',
        nX=1,nY=1, # number of input/output variables
        par=list(k1,a1,c1,k2,a2,c2), # list of model parameters
        xtra=xtraModelInfo(object=controlMatrix)) # use xtraModelInfo() to pass the control matrix
# Define the model input
X=data.frame(stage=seq(-1,7,0.1))
# Write the config files used to run the model. To actually run it,
# use run=TRUE, but BaM executable needs to be downloaded first (use downloadBaM())
runModel(workspace=tempdir(),mod=M,X=X,run=FALSE)
}
