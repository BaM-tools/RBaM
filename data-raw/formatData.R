# twoPopulations
X=read.table('twoPopulations.txt',header=T)
twoPopulations=X[c(1:3,6:7)]
twoPopulations[twoPopulations==-9999] <- NA
save(twoPopulations,file='../data/twoPopulations.RData')
