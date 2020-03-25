'''llamo otros Paquetes necesarios'''
library(dplyr)
library(RefManageR)
library(bibliometrix)
library(quanteda)
library(ggplot2)
library(ggpubr)

'''antes de iniciar con el analisis es bueno leer todo este documento 
de ayuda de bibliometrix y seguirlo paso a paso en caso de ser necesario'''
help(bibliometrix)

'''abro mis archivos con sus rutas y con objetos d1 y d2'''
'''mucho ojo, el caomando que debe abrir los documentos
es readFiles de bibliometrix, si no se usa este comando, despues el 
computador, va a dar errores de lectura en los dataframes'''
d1 <- readFiles("/home/alrier/Documents/AAA DOCTORADO/Base de datos consumo y drogas/bib/full.bib")
d2 <- readFiles("/home/alrier/Documents/AAA DOCTORADO/Base de datos consumo y drogas/bib/full2.bib")

'''convierto los archivos a dataframes, esto toma 
algunos minutos dependiento del equipo que se use'''
df1 <- convert2df(d1, dbsource = "scopus", format = "bibtex")
df2 <- convert2df(d2, dbsource = "scopus", format = "bibtex")
D <- readFiles("/home/alrier/Documents/AAA DOCTORADO/Base de datos consumo y drogas/bib/fullext.bib")
M <- convert2df(D, dbsource = "scopus", format = "bibtex")
results <- biblioAnalysis(M, sep=";")
options(width = 100)
S <- summary(object = results, k = 10, pause = FALSE)
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
S <- normalizeSimilarity(NetMatrix, type = "association")
net <- networkPlot(S, n = 200, Title = "co-occurrence network", type = "fruchterman", 
                   labelsize = 0.7, halo = FALSE, cluster = "walktrap",
                   remove.isolates = FALSE, remove.multiple = FALSE, noloops = TRUE, weighted = TRUE)



'''procedo a mezclar mis dataframes con el comando siguiente'''
m <- mergeDbSources(df1,df2, remove.duplicated = FALSE)
M <- convert2df(m, dbsource = "scopus", format = "bibtex")
'''compruebo las dimensiones de mi nuevo dataframe llamado "full" que está listo para ser usado'''
dim(M)
''' reviso mi dataframe para ver en qué me voy a centrar en el análisis '''
View(M)
''' Procedo a realizar mi biblioanálisis del dataframe total '''
Analyse <- biblioAnalysis(full)

'''veo mi compendio de "analyse" '''
View(Analyse)

'''proceso a sacar el summary de los resultados, esta etapa es muy
importante porque resume todos los resultados en categiróas'''
s <- summary(object = Analyse, K=10, pause = FALSE)
View(s)
plot(x = Analyse, k = 10, pause = FALSE)


'''ahora voy a crear una red de colaoración cientifica que contenga los autores AU y la colaboración CO'''
M <- metaTagExtraction(full, Field = "AU_CO", sep = ";")

'''una vez creada la red de colaboración y autores, proceso a generar aplicar la funcion para crear una red matricial'''
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

'''una vez creada esta red puedo proceder a poltear mi red'''
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.8)





options(width = 100)
S <- summary(object = results, k = 10, pause = FALSE)
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "author_keywords", sep = ";")
S <- normalizeSimilarity(NetMatrix, type = "association")
net <- networkPlot(S, n = 200, Title = "co-occurrence network", type = "fruchterman", 
                   labelsize = 0.7, halo = FALSE, cluster = "walktrap",
                   remove.isolates = FALSE, remove.multiple = FALSE, noloops = TRUE, weighted = TRUE)





'''Del total de resultados, extraigo los papers más citados'''
AU <- Analyse$MostCitedPapers
AUT <- AU[1:2]
View(AUT)
Analyse$MainInformation
'''Lets see most productive countries'''
Paises <- Analyse$MostProdCountries
'''Lets keep the first and thirs column of this dataframe'''
Paises <- Paises[c(1, 3)]
'''Lets change the name of the first column'''
names(Paises)[1] <- "Country"
'''Pongamos los nombres en Español'''
'''Paises$Country <- c("USA", "Taiwan", "Korea",  "Reino Unido", "Alemania", "Holanda", "Italia", "Canada", "España", "China")'''
Paises$Freq <- as.numeric(Paises$Freq)
'''Lets see the production'''
Produccion <- Resultados$AnnualProduction
 '''Lets change the name of the first column'''
names(Produccion)[1] <- "Year"
'''Lets set as numeric the records of the second column'''
Produccion$Articles <- as.numeric(Produccion$Articles)

'''graficas y plots'''

Fig1A <- ggplot(Paises, aes(x=reorder(Country, Freq) , y=Freq)) + geom_bar(stat = "identity", fill="blue") + coord_flip() + xlab("Country") + ylab("Relative Frequency")
Fig1B <- ggplot(Produccion, aes(x=Year , y=Articles)) + geom_bar(stat = "identity", fill="blue") + xlab("Year") + ylab("Articles") + theme(axis.text.x = element_text(angle = 90, hjust = 1)
ggarrange(Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)

print(Fig1A)
