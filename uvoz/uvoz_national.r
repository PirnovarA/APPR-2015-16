#Uvoz tabel national in state, s podatki o placah iz "Bureau of Labor statistics"

#2014
national_2014 <- naredi("podatki/national_2014.csv")
national_2014 <- naredi.urne(national_2014,"national","2014")

state_2014 <- naredi("podatki/state_2014.csv", "state")
state_2014 <- naredi.urne(state_2014,"state","2014")

#2012
national_2012 <- naredi("podatki/national_2012.csv","national","2012")
national_2012 <- naredi.urne(national_2012,"national","2012")

state_2012 <- naredi("podatki/state_2012.csv", "state","2012")
state_2012 <- naredi.urne(state_2012,"state","2012")

#2010
national_2010 <- naredi("podatki/national_2010.csv","national","2010")
national_2010 <- naredi.urne(national_2010,"national","2010")

state_2010 <- naredi("podatki/state_2010.csv", "state","2010")
state_2010 <- naredi.urne(state_2010,"state","2010")

#2008
national_2008 <- naredi("podatki/national_2008.csv","national","2008")
national_2008 <- naredi.urne(national_2008,"national","2008")

state_2008 <- naredi("podatki/state_2008.csv", "state","2008")
state_2008 <- naredi.urne(state_2008,"state","2008")

#2006
national_2006 <- naredi("podatki/national_2006.csv","national","2006")
national_2006 <- naredi.urne(national_2006,"national","2006")

state_2006 <- naredi("podatki/state_2006.csv", "state","2006")
state_2006 <- naredi.urne(state_2006,"state","2006")

#2004
national_2004 <- naredi("podatki/national_2004.csv","national","2004")
national_2004 <- naredi.urne(national_2004,"national","2004")

state_2004 <- naredi("podatki/state_2004.csv", "state","2004")
state_2004 <- naredi.urne(state_2004,"state","2004")

#2002
national_2002 <- naredi("podatki/national_2002.csv","national","2002")
national_2002 <- naredi.urne(national_2002,"national","2002")

state_2002 <- naredi("podatki/state_2002.csv", "state","2002")
state_2002 <- naredi.urne(state_2002,"state","2002")

##########################################################################################
#Skupaj sem dal vse national in vse state tabele, da dobim 2 veliki tabeli z vsemi stvarcami
national <- rbind(national_2014,national_2012,national_2010,national_2008,national_2006,national_2004,national_2002)
state <- rbind(state_2014,state_2012,state_2010,state_2008,state_2006,state_2004,state_2002)
##########################################################################################
#Tabele, v katerih so spravljeni podatki o stevilu zaposlenih
zaposlitev_nat <- subset(national,select=c(Occupation,Total_employment,Year))
zaposlitev_st <- subset(state, select=c(State,Occupation,Total_employment,Year))

national <- subset(national, select=-Total_employment)
state <- subset(state, select=-Total_employment)

#########################################################################################
#Podatke dam v tidy data
national_tidy <- as.data.frame(melt(national, id=c("Occupation","Year")))
national_tidy <- rename(national_tidy,Wage=value,Type=variable)

state_tidy <- melt(state, id=c("State", "Occupation","Year"))
state_tidy <- rename(state_tidy,Wage=value,Type=variable)
