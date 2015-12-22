#Tukaj urejam tabele poleg osnovnega oblikovanja in uvoza

#National razdelim na urne in letne tabele
national_h <- na.omit(subset(national, select = names(national)[c(1,3,5:9)]))
national_a <- na.omit(subset(national, select = names(national)[c(1,4,10:14)]))
employment <- subset(national, select = names(national)[1:2])
#Odstranim originalno tabelo, ker je vec ne rabim, ker je zdruzena v teh zgornjih
rm(national)
# Uredimo po desc(mean, median, PCT_10)
national_h <- arrange(national_h, desc(Hourly_mean), desc(Hourly_median), desc(H_PCT10))
national_a <- arrange(national_a, desc(Annual_mean), desc(Annual_median), desc(A_PCT10))
employment <- arrange(employment, desc(Total_employment))

####################################################################################

#Tabelco per_capita priredimo za inflacijo, da bomo lahko primerjali vrednosti skozi leta med sabo(NPV bo v 2015)
inflacije = c(2.880,1.816,1.557,1.378,1.290,1.177,1.106,1.034)
per_capita[2:9] <- round(data.frame(mapply(`*`,per_capita[2:9],inflacije)),0)

#Uredimo tabelci per_capita in per_capita2
per_capita2 <- arrange(per_capita2,desc(`Per capita income`),desc(`Median household income`))
per_capita <- arrange(per_capita,desc(`2012`),desc(`2009`))


write.csv2(national_h, "podatki/national_h.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv2(national_a, "podatki/national_a.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv2(employment, "podatki/employment.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv2(per_capita, "podatki/per_capita.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv2(per_capita2, "podatki/per_capita2.csv", fileEncoding = "UTF-8", row.names = FALSE)
