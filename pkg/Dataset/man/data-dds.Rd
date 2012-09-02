\name{dds}
\alias{dds}
\docType{data}
\title{
Data sample for the Dataset package
}
\description{
A small data base for testing purpose only. These data are under license, you can't use it in your work. Thank you for your comprehension.
}
\usage{data(dds)}
\format{
  The format is:
Formal class 'Dataset' [package "Dataset"] with 7 slots
  ..@ name       : chr "Dataset-data-sample"
  ..@ description: chr(0) 
  ..@ variables  :List of 10
  .. ..$ idpers                  :Formal class 'ScaleVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 3 8 13 15 19 21 25 29 33 37 ...
  .. .. .. ..@ missings   : num(0) 
  .. .. .. ..@ values     : num(0) 
  .. .. .. ..@ description: Named chr "Identifiant du répondant"
  .. .. .. .. ..- attr(*, "names")= chr "idpers"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ sexe                    :Formal class 'BinaryVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 0 0 0 1 1 1 1 0 1 0 ...
  .. .. .. ..@ missings   : Named num -2
  .. .. .. .. ..- attr(*, "names")= chr "N'a pas répondu"
  .. .. .. ..@ values     : Named num [1:2] 0 1
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "Homme" "Femme"
  .. .. .. ..@ description: Named chr "Sexe du répondant"
  .. .. .. .. ..- attr(*, "names")= chr "sexe"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ annenais                :Formal class 'ScaleVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 1982 1979 1978 1983 1984 ...
  .. .. .. ..@ missings   : Named num -2
  .. .. .. .. ..- attr(*, "names")= chr "N'a pas répondu"
  .. .. .. ..@ values     : Named num(0) 
  .. .. .. .. ..- attr(*, "names")= chr(0) 
  .. .. .. ..@ description: Named chr "Année de naissance du répondant"
  .. .. .. .. ..- attr(*, "names")= chr "annenais"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ datedip                 :Formal class 'ScaleVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 200907 200906 200906 200906 200807 ...
  .. .. .. ..@ missings   : Named num -2
  .. .. .. .. ..- attr(*, "names")= chr "N'a pas répondu"
  .. .. .. ..@ values     : Named num(0) 
  .. .. .. .. ..- attr(*, "names")= chr(0) 
  .. .. .. ..@ description: Named chr "Année et mois d'obtention du diplôme du répondant"
  .. .. .. .. ..- attr(*, "names")= chr "datedip"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ etatcivil               :Formal class 'NominalVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 0 0 1 0 0 0 0 0 1 0 ...
  .. .. .. ..@ missings   : Named num -2
  .. .. .. .. ..- attr(*, "names")= chr "N'a pas répondu"
  .. .. .. ..@ values     : Named num [1:5] 0 1 2 3 4
  .. .. .. .. ..- attr(*, "names")= chr [1:5] "Célibataire" "Marié/e (yc. séparé/e) /partenariat enregistré" "Union libre" "Veuf/veuve" ...
  .. .. .. ..@ description: Named chr "Etat civil du répondant"
  .. .. .. .. ..- attr(*, "names")= chr "etatcivil"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ respdenfant             :Formal class 'BinaryVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 1 1 0 1 1 1 1 1 0 1 ...
  .. .. .. ..@ missings   : Named num -2
  .. .. .. .. ..- attr(*, "names")= chr "N'a pas répondu"
  .. .. .. ..@ values     : Named num [1:2] 0 1
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "Oui" "Non"
  .. .. .. ..@ description: Named chr "Le répondant a ou partage la responsabilité d'enfants"
  .. .. .. .. ..- attr(*, "names")= chr "respdenfant"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ connaissancetcompétence1:Formal class 'OrdinalVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 2 2 2 3 3 2 4 3 2 3 ...
  .. .. .. ..@ missings   : Named num -2
  .. .. .. .. ..- attr(*, "names")= chr "N'a pas répondu"
  .. .. .. ..@ values     : Named num [1:5] 1 2 3 4 5
  .. .. .. .. ..- attr(*, "names")= chr [1:5] "Pas du tout" "Très peu" "Dans une certaine mesure" "Dans une large mesure" ...
  .. .. .. ..@ description: Named chr "La mesure à laquelle le répondant a acquis pendant ses études des aptitudes à utiliser defférents outils informatiques"
  .. .. .. .. ..- attr(*, "names")= chr "connaissancetcompétence1"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ conaisetcomp.exigé1     :Formal class 'OrdinalVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 2 1 4 4 3 3 4 3 3 2 ...
  .. .. .. ..@ missings   : Named num -2
  .. .. .. .. ..- attr(*, "names")= chr "N'a pas répondu"
  .. .. .. ..@ values     : Named num [1:5] 1 2 3 4 5
  .. .. .. .. ..- attr(*, "names")= chr [1:5] "Pas du tout" "Très peu" "Dans une certaine mesure" "Dans une large mesure" ...
  .. .. .. ..@ description: Named chr "La mesure à laquelle est exigée dans l'activité professionnelle actuelle du répondant l'aptitude à utiliser différents outils i"| __truncated__
  .. .. .. .. ..- attr(*, "names")= chr "conaisetcomp.exigé1"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ txocupationcontrat1     :Formal class 'ScaleVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 100 50 50 100 50 100 100 100 50 100 ...
  .. .. .. ..@ missings   : Named num [1:2] -2 -4
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "N'a pas répondu" "Inapplicable"
  .. .. .. ..@ values     : Named num(0) 
  .. .. .. .. ..- attr(*, "names")= chr(0) 
  .. .. .. ..@ description: Named chr "Le taux d'occupation fixé dans le contrat de travail principal du répondant"
  .. .. .. .. ..- attr(*, "names")= chr "txocupationcontrat1"
  .. .. .. ..@ weights    : num(0) 
  .. ..$ durélimitéouillimité    :Formal class 'BinaryVariable' [package "Dataset"] with 5 slots
  .. .. .. ..@ codes      : num [1:15] 1 0 1 0 1 1 1 0 0 1 ...
  .. .. .. ..@ missings   : Named num [1:2] -2 -4
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "N'a pas répondu" "Inapplicable"
  .. .. .. ..@ values     : Named num [1:2] 0 1
  .. .. .. .. ..- attr(*, "names")= chr [1:2] "Pour une durée limitée" "Pour une durée illimitée"
  .. .. .. ..@ description: Named chr "Le répondant est engagé pour une durée limitée ou illimitée"
  .. .. .. .. ..- attr(*, "names")= chr "durélimitéouillimité"
  .. .. .. ..@ weights    : num(0) 
  ..@ row.names  : chr [1:15] "3" "8" "13" "15" ...
  ..@ weights    : chr(0) 
  ..@ checkvars  : chr(0) 
  ..@ infos      : list()
}
\details{
...
}
\source{
...
}
\references{
...
}
\examples{
data(dds)
## maybe str(dds) ; plot(dds) ...
}
\keyword{datasets}
\keyword{data}
\keyword{Dataset}
\keyword{dds}
