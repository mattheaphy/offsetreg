.onLoad <- function(libname, pkgname) {
  make_poisson_reg_offset()
  make_poisson_reg_glm_offset()
  make_poisson_reg_glmnet_offset()
}
