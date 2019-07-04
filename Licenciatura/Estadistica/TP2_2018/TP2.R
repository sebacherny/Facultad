# install.packages("RColorBrewer")
library(RColorBrewer)
plot.col<-function(x,y,factor,data,title.legend,pch=16,...){
	require(RColorBrewer)
	#get all elements as vectors arguments x y factor
	if(class(factor)!="factor"){
		factor<-factor(factor)
	}
	lvl<-length(levels(factor))
	if(lvl>7){
		cat("More than 7 levels in the provided factor, not enough colors available\n")
		break
	}
#	if(lvl==2){
#		pal<-brewer.pal(lvl+1,"Set1")
#		pal<-pal[-3]
#	}
#	else{
#		pal<-brewer.pal(lvl,"Set1")
#	}
	par(xpd=TRUE,mar=c(5,4,4,10))
	pal=c("blue", "green", "red")
	plot(x,y,col=pal[factor],pch=pch,...)
	xcoord<-max(x)+((max(x)-min(x))/20)
	ycoord<-max(y)
	print(pal)
	legend(xcoord,ycoord,legend=levels(factor),col=pal,pch=pch,title=title.legend)
	par(xpd=FALSE,mar=c(5,4,4,2))
}

obtener_primer_momento_de_muestra <- function(muestra) {
	return(2*sum(muestra)/length(muestra));
}

obtener_EMV_uniforme <- function(muestra) {
	return(max(muestra));
}

obtener_EMV_modificado <- function(muestra) {
	return( max(muestra) + min(muestra) );
}


ejd <- function() {
	n_vals = c(6, 10, 20, 40, 80, 200, 500);
	tita0 = 3;
	mrep = 1000;
	ECMs_para_grafico = c();
	FILES_FOLDER = '/home/cherny/';
	for (n in n_vals){
		print(n);
		ECM_primer_momento = 0;
                ECM_sesgado = 0;
                ECM_modif = 0;
		titas_primer_momento = c()
		titas_sesgados = c()
		titas_modif = c()
		for (rep in 1:mrep) {
			muestra = runif(n, 0, tita0);
			tita_primer_momento = obtener_primer_momento_de_muestra(muestra);
			titas_primer_momento = c(titas_primer_momento, tita_primer_momento)
			tita_sesgado = obtener_EMV_uniforme(muestra);
			titas_sesgados = c(titas_sesgados, tita_sesgado)
			tita_modif = obtener_EMV_modificado(muestra);
			titas_modif = c(titas_modif, tita_modif)
			ECM_primer_momento = ECM_primer_momento + (tita_primer_momento - tita0)*(tita_primer_momento - tita0);
                        ECM_sesgado = ECM_sesgado + (tita_sesgado - tita0)*(tita_sesgado - tita0);
                        ECM_modif = ECM_modif + (tita_modif - tita0)*(tita_modif - tita0);
		}
		png(paste(FILES_FOLDER, "boxplot_", n, ".png", sep=""))
		boxplot(titas_primer_momento, titas_sesgados, titas_modif, names = c("1ER MOMENTO", "MAX", "MAX + MIN"), main=paste("N =", n))
		ECM_primer_momento = ECM_primer_momento/mrep;
                ECM_sesgado = ECM_sesgado/mrep;
                ECM_modif = ECM_modif/mrep;
		if(n!=500){
			ECMs_para_grafico = c(ECMs_para_grafico, ECM_primer_momento, ECM_sesgado, ECM_modif)
		}
	}
	n_vec = c()
	for (n in n_vals) {
		if(n!=500){
			n_vec = c(n_vec, rep(n, 3))
		}
	}
	png(paste(FILES_FOLDER, 'ejd.png', sep=""))
	plot.col(x=n_vec,y=ECMs_para_grafico, factor=rep(c("primer momento","max","max+min"),length(n_vals)-1), title.legend="Referencia",pch=1,cex=1.4,xlab="N",ylab="ECM",main="Error Cuadratico Medio segun n de cada estimador")
####	plot(n_vec, ECMs_para_grafico, col = colors);
	dev.off();
}

ejd();
