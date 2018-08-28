


#converting date to class POSIXct
date1 <- as.POSIXct(strptime(as.character(com_sdlg$date1), format = "%Y-%m-%d"))
date2 <- as.POSIXct(strptime(as.character(com_sdlg$date2), format = "%Y-%m-%d"))

com_sdlg$date1 <- date1
com_sdlg$date2 <- date2

#deleting observations where dates are not recorded
com_sdlg <- com_sdlg[!is.na(date2) & !is.na(date1),]

#adding a quadrat variable
com_sdlg$quadrat <- colsplit(string= com_sdlg$id, pattern="-", names=c("Part1", "Part2"))$Part1

com_sdlg$ind <- colsplit(string= com_sdlg$id, pattern="-", names=c("Part1", "Part2"))$Part2

quad <- com_sdlg %>%
  filter(quadrat == 10030) %>%
  select(date1, date2, status, ind) %>%
  arrange(., date1)

alive <- com_sdlg %>%
  filter(quadrat == 11641) %>%
  select(date1, date2, status, ind) %>%
  arrange(., date1) %>%
  filter(date1 > strptime("2009-01-01",format = "%Y-%m-%d") & date1 < strptime("2009-12-31",format = "%Y-%m-%d")) %>%
  filter(status == "A") %>%
  select(ind)

censused_start <- com_sdlg %>%
  filter(quadrat == 18675) %>%
  select(date1, date2, status, ind) %>%
  arrange(., date1) %>%
  filter(date1 > strptime("2011-01-01",format = "%Y-%m-%d") & date1 < strptime("2011-12-31",format = "%Y-%m-%d")) %>%
  filter(status == "A")


com_sdlg %>%
  filter(quadrat == 19935) %>%
  select(date1, date2, status, ind) %>%
  arrange(., date1) %>%
  filter(ind == 328)
  
  group_by(ind) %>%
  summarise(count = length(ind))
  
  
  
  filter(date1 > strptime("2009-01-01",format = "%Y-%m-%d") & date1 < strptime("2009-12-31",format = "%Y-%m-%d")) %>%
  filter(status == "A") %>%
  select(ind)


str(com_sdlg)


alive_2008_end <- com_sdlg[substring(com_sdlg$date2,1,4) == "2008" & com_sdlg$status == "A",]
cen_2008_beg <- com_sdlg[substring(com_sdlg$date1,1,4) == "2008",] 
not_missing <- alive_2008_end$id %in% cen_2008_beg$id
missing <- as.logical((not_missing*-1)+1)


sum(missing)

com_sdlg$id[missing]

com_sdlg[missing,]



length(alive_2008_end$sp)

length(cen_2008_beg$sp)




sum(missing)


alive_2008_end$id[missing]




com_sdlg$id[missing]






alive$ind !%in% censused_start$ind







