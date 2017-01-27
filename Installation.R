# Did this on work computer


# INSTALL RSTAN ===============================================================

# https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows

# installed Rtools per instructions

# seems sketchy that this goes to firm.seyfarth.com / ... / My Documents
# 	instead of on an actual disk...
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) 
  dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) 
  file.create(M)
cat("\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function", 
    file = M, sep = "\n", append = TRUE)

cat('Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")',
    file = file.path(Sys.getenv("HOME"), ".Rprofile"), 
    sep = "\n", append = TRUE)

# Didn't run this
cat("\nCXXFLAGS += -Wno-ignored-attributes -Wno-deprecated-declarations", 
    file = M, sep = "\n", append = TRUE)

cat(readLines(M), sep = "\n")
cat(M)


# note: omit the 's' in 'https' if you cannot handle https downloads
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)


# lazyeval and ggplot2 didn't install / can't load for some reason...
# 	think they were built under R 3.2 and I'm running 3.3


fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
    return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
' )
fx( 2L, 5 ) # should be 10
# works

library("rstan")
# had to delete and reinstall ggplot2 and lazyeval, but this somehow actually
# 	worked

# INSTALL RSTAN ===============================================================



# INSTALL rethinking ==========================================================
install.packages(c('coda', 'mvtnorm', 'devtools'))
library('devtools')
devtools::install_github('rmcelreath/rethinking')

library(rethinking)
# actually works
# INSTALL rethinking ==========================================================

