##DMAX##
#The Dmax is the mean of the six largest trees in each species across all censuses in BCI.


dmax_late_PFT <- bci.full %>%
  select(dbh, sp) %>%
  filter(sp %in% Late) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.)) %>% group_by(sp) %>%
  summarise(dmax = mean(dbh)) %>%
  arrange(., dmax) %>%
  .$dmax %>% mean(.)

dmax_early_PFT <- bci.full %>%
  select(dbh, sp) %>%
  filter(sp %in% Early) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.)) %>% group_by(sp) %>%
  summarise(dmax = mean(dbh)) %>%
  arrange(., dmax) %>%
  filter(dmax < 1000) %>% #even if we filter out the largest individuals dmax is still bigger for the early pfts.
  .$dmax %>% mean(.)


Dmax <- c(dmax_early_PFT, dmax_late_PFT)
names(Dmax) <- c("early", "late")





#creating histogram of early dmax
bci.full %>%
  select(dbh, sp) %>%
  filter(sp %in% Early) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.)) %>% group_by(sp) %>%
  summarise(dmax = mean(dbh)) %>%
  arrange(., dmax) %>%
  ggplot(mapping = aes(x = dmax)) +
  geom_histogram(bins = 10)

#creating histogram of early dmax
bci.full %>%
  select(dbh, sp) %>%
  filter(sp %in% Late) %>%
  na.omit(.) %>%
  group_by(sp) %>%
  .[order(.$dbh, decreasing = TRUE),] %>%
  do(head(.)) %>% group_by(sp) %>%
  summarise(dmax = mean(dbh)) %>%
  arrange(., dmax) %>%
  ggplot(mapping = aes(x = dmax)) +
  geom_histogram(bins = 10)





