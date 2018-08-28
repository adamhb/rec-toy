##DMAX##
#The Dmax is the mean of the six largest trees in each species across all census in BCI.

dmax_early <- bci.full %>%
  select(dbh, sp) %>%
  filter(sp %in% Early) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.))

dmax_early_PFT <- mean(dmax_early$dbh)


dmax_late <- bci.full %>%
  select(dbh, sp) %>%
  filter(sp %in% Late) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.))

dmax_late_PFT <- mean(dmax_late$dbh)

Dmax <- c(dmax_early_PFT, dmax_late_PFT)
names(Dmax) <- c("early", "late")



