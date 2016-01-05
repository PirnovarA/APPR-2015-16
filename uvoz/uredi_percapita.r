#Tukaj urejam tabele poleg osnovnega oblikovanja in uvoza

####################################################################################

#Tabelco per_capita priredimo za inflacijo, da bomo lahko primerjali vrednosti skozi leta med sabo(NPV bo v 2015)
inflacije = c(2.880,1.816,1.557,1.378,1.290,1.177,1.106,1.034)
per_capita[2:9] <- round(data.frame(mapply(`*`,per_capita[2:9],inflacije)),0)

#Uredimo tabelci per_capita in per_capita2
per_capita2 <- arrange(per_capita2,desc(`Per capita income`),desc(`Median household income`))
per_capita <- arrange(per_capita,desc(`2012`),desc(`2009`))


#write.csv2(national_h_2014, "podatki/national_h_2014.csv", fileEncoding = "UTF-8", row.names = FALSE)
#write.csv2(national_a_2014, "podatki/national_a_2014.csv", fileEncoding = "UTF-8", row.names = FALSE)
#write.csv2(employment, "podatki/employment.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv2(per_capita, "podatki/per_capita.csv", fileEncoding = "UTF-8", row.names = FALSE)
write.csv2(per_capita2, "podatki/per_capita2.csv", fileEncoding = "UTF-8", row.names = FALSE)
