citHeader("To cite Dataset in publications please use:")

p1 <- person("Emmanuel", "Rousseaux", email = "emmanuel.rousseaux@unige.ch")
p2 <- person("Gilbert", "Ritschard", email = "gilbert.ritschard@unige.ch")

citEntry(
  entry = "INPROCEEDINGS",
  title  = "The Dataset project: handling survey data in {R}",
  author = personList(
    as.person("Emmanuel Rousseaux"),
    as.person("Gilbert Ritschard")
  ),
  booktitle = "Lausanne conference on sequence analysis (LaCOSA) 2012",
  year = "2013",
  url = "http://www3.unil.ch/wpmu/sequences2012/",
  textVersion = paste(
    "Emmanuel Rousseaux, Gilbert Ritschard.",
    "The Dataset project: handling survey data in R.",
    "Lausanne conference on sequence analysis (LaCOSA) 2012.",
    "URL http://www3.unil.ch/wpmu/sequences2012/."
  ),
  header = "For the Dataset project and the Dataset package:\n"
)

citFooter("See",sQuote("citation()"),"for citing R itself.")