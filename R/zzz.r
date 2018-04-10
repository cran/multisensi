# Multisensi R package ; file zzz.r (last modified: 2018-04-04) 
# Authors: C. Bidot, M. Lamboni, H. Monod
# Copyright INRA 2011-2018 
# MaIAGE, INRA, Univ. Paris-Saclay, 78350 Jouy-en-Josas, France
#
# More about multisensi in https://CRAN.R-project.org/package=multisensi
#
# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software.  You can  use, 
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info". 
#
# As a counterpart to the access to the source code and  rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty  and the software's author,  the holder of the
# economic rights,  and the successive licensors  have only  limited
# liability. 
#
# In this respect, the user's attention is drawn to the risks associated
# with loading,  using,  modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean  that it is complicated to manipulate,  and  that  also
# therefore means  that it is reserved for developers  and  experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or 
# data to be ensured and,  more generally, to use and operate it in the 
# same conditions as regards security. 
#
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.
#
#===========================================================================
.onAttach <- function(libname, pkgname)
#===========================================================================
{
  # If not interactive, hide message
  if (!interactive()){
    return()
  }
  startup_message=paste("Loaded ", pkgname," ",as.character(utils::packageVersion(pkgname)),".",sep="")
  # Create a list of helpful tips
  pkg_hints = c(
    paste("Check for updates and report bugs at `https://CRAN.R-project.org/package=",pkgname,"`.",sep=""),
    "Use `suppressPackageStartupMessages()` to remove package startup messages.",
    paste("To see the user guides use `browseVignettes('",pkgname,"')`.",sep=""),
    paste("To see how to cite the package use `citation('",pkgname,"')`.",sep=""),
    paste("To run the examples use `example('",pkgname,"')`.",sep=""),
    "To see loaded packages use `sessionInfo()`."
  )
  if (stats::runif(1) > 0.5){
    # Randomly pick one hint
    startup_hint = sample(pkg_hints, 1)
    startup_message=paste(startup_message,startup_hint,sep="\n")
  }
  packageStartupMessage(startup_message,"\n")
}

