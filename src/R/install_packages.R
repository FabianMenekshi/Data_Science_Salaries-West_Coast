packages <- c("ggplot2", "glmnet", "dplyr", "MASS", "car")
installed <- rownames(installed.packages())
for (p in packages) {
  if (!(p %in% installed)) install.packages(p)
}