sfotos<-read.csv2(sfotos_csv<-'..\\dados\\fotos_discos.csv');
sfotos<-subset(sfotos,foto==1);

narvs<-sort(unique(sfotos$arv));


dir.create(paste('..\\dados\\desenhos\\tif'));
for(arv in narvs){dir.create(paste('..\\dados\\desenhos\\tif\\',arv,sep=''))}

dir.create(paste('..\\dados\\desenhos\\jpg2'));
for(arv in narvs){dir.create(paste('..\\dados\\desenhos\\jpg2\\',arv,sep=''))}

dir.create(paste('..\\dados\\desenhos\\shp2'));
for(arv in narvs){dir.create(paste('..\\dados\\desenhos\\shp2\\',arv,sep=''))}

dir.create(paste('..\\dados\\desenhos\\jpg3'));
for(arv in narvs){dir.create(paste('..\\dados\\desenhos\\jpg3\\',arv,sep=''))}

dir.create(paste('..\\dados\\fotos\\jpg2'));
for(arv in narvs){dir.create(paste('..\\dados\\fotos\\jpg2\\',arv,sep=''))}

dir.create(paste('..\\dados\\fotos\\jpg3'));
for(arv in narvs){dir.create(paste('..\\dados\\fotos\\jpg3\\',arv,sep=''))}

dir.create(paste('..\\dados\\fotos\\shp2'));
for(arv in narvs){dir.create(paste('..\\dados\\fotos\\shp2\\',arv,sep=''))}

