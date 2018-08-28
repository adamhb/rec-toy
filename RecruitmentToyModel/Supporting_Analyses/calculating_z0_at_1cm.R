
#calculating the carbon in leaves of new 1 cm recruit

d = 1
p1 = 0.07
p2 = 1.3
c2b = 2
#fates_allom_d2bl3 = 0.55
blmax    = p1*d*p2 / c2b


#calculating the diameter to height from FATES allom

p1 = 0.64
p2 = 0.37
p3 = -999.9

p1 = p1e

h = exp(p1e + p2*log(d) + p3*log(d)*2.0) #line 1298 of allom mod


#calculating the structural carbon above ground
#option1
#bagw <- Chave.AGB(1)
#option 2 (FATES)
wood_density <- wsg.ctfs3$wsg %>% mean(.,na.rm = T)

p1 = 0.06896
p2 = 0.572

bagw   = (p1*(wood_density*d*2.0*h)*p2)/c2b


#calculating below ground biomass (from FATES allom bgbw subroutine)
agb_fraction <- 0.6
bbgw = (1.0/agb_fraction-1.0)*bagw 


total_biomass <- bagw + bbgw + 2*blmax #we are counting fine roots as equal to the leaf biomass
Z0 <- total_biomass * 1000
Z0











