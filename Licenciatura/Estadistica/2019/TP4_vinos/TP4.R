data = read.csv("/home/cherny/Downloads/vinos_A.csv", header=T)

model0 = function(datos){
        return (lm(quality ~ ., data = datos));
}

model0_important = function(datos){
	return (lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + sulphates + alcohol, data = datos));
}


model1 = function(datos){
	return (lm(quality ~ volatile.acidity  + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + pH + sulphates + alcohol, data = datos));
}

model1_important = function(datos){
        return (lm(quality ~ volatile.acidity + chlorides + total.sulfur.dioxide + pH + sulphates + alcohol, data = datos));
}


model2 = function(datos){
        return (lm(quality ~ fixed.acidity + I(fixed.acidity**2) + volatile.acidity + residual.sugar + I(residual.sugar**2) + chlorides + free.sulfur.dioxide + sulphates + density + I(density**2) + citric.acid + I(citric.acid**2) + alcohol, data = datos));
}

model2_important = function(datos){
        return (lm(quality ~ fixed.acidity + volatile.acidity + chlorides + sulphates + alcohol, data = datos));
}


model3 = function(datos){
        return (lm(quality ~ fixed.acidity + I(fixed.acidity**2) + volatile.acidity + residual.sugar + I(residual.sugar**2) + chlorides + free.sulfur.dioxide + sulphates + density + I(density**2) + citric.acid + I(citric.acid**2) + alcohol + pH + total.sulfur.dioxide, data = datos));
}

model3_important = function(datos){
        return (lm(quality ~ volatile.acidity + chlorides + sulphates + alcohol + pH + total.sulfur.dioxide, data = datos));
}


model4 = function(datos){
	return (lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol + I(fixed.acidity**2) + I(volatile.acidity**2) + I(citric.acid**2) + I(residual.sugar**2) + I(chlorides**2) + I(free.sulfur.dioxide**2) + I(total.sulfur.dioxide**2) + I(density**2) + I(pH**2) + I(sulphates**2) + I(alcohol**2), data = datos));
}

model4_important = function(datos){
        return (lm(quality ~ sulphates + total.sulfur.dioxide + I(volatile.acidity**2) + I(sulphates**2), data = datos));
}


model5 = function(datos){
	return (lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol + I(fixed.acidity**2) + I(volatile.acidity**2) + I(citric.acid**2) + I(residual.sugar**2) + I(chlorides**2) + I(free.sulfur.dioxide**2) + I(total.sulfur.dioxide**2) + I(density**2) + I(pH**2) + I(sulphates**2) + I(alcohol**2) + I(fixed.acidity*volatile.acidity) + I(fixed.acidity*residual.sugar) + I(fixed.acidity*free.sulfur.dioxide) + I(fixed.acidity*total.sulfur.dioxide) + I(fixed.acidity*pH) + I(fixed.acidity*sulphates) + I(citric.acid*fixed.acidity) + I(citric.acid*volatile.acidity) + I(citric.acid*residual.sugar) + I(citric.acid*free.sulfur.dioxide) + I(citric.acid*total.sulfur.dioxide) + I(citric.acid*density) + I(citric.acid*pH) + I(citric.acid*sulphates) + I(residual.sugar*volatile.acidity) + I(residual.sugar*total.sulfur.dioxide) + I(residual.sugar*sulphates) + I(chlorides*fixed.acidity) + I(chlorides*volatile.acidity) + I(chlorides*citric.acid) + I(chlorides*residual.sugar) + I(chlorides*free.sulfur.dioxide) + I(chlorides*total.sulfur.dioxide) + I(chlorides*density) + I(chlorides*pH) + I(chlorides*sulphates) + I(free.sulfur.dioxide*volatile.acidity) + I(free.sulfur.dioxide*residual.sugar) + I(free.sulfur.dioxide*total.sulfur.dioxide) + I(free.sulfur.dioxide*pH) + I(free.sulfur.dioxide*sulphates) + I(total.sulfur.dioxide*volatile.acidity) + I(density*fixed.acidity) + I(density*volatile.acidity) + I(density*residual.sugar) + I(density*free.sulfur.dioxide) + I(density*total.sulfur.dioxide) + I(density*pH) + I(density*sulphates) + I(pH*volatile.acidity) + I(pH*residual.sugar) + I(pH*total.sulfur.dioxide) + I(pH*sulphates) + I(sulphates*volatile.acidity) + I(sulphates*total.sulfur.dioxide) + I(alcohol*fixed.acidity) + I(alcohol*volatile.acidity) + I(alcohol*citric.acid) + I(alcohol*residual.sugar) + I(alcohol*chlorides) + I(alcohol*free.sulfur.dioxide) + I(alcohol*total.sulfur.dioxide) + I(alcohol*density) + I(alcohol*pH) + I(alcohol*sulphates), data = datos));
}

model5_important = function(datos){
        return (lm(quality ~ free.sulfur.dioxide   + total.sulfur.dioxide   + pH   + I(sulphates^2)   + I(fixed.acidity * free.sulfur.dioxide) + I(citric.acid * volatile.acidity) + I(citric.acid * residual.sugar) + I(citric.acid * pH) + I(chlorides * fixed.acidity) + I(chlorides * free.sulfur.dioxide) + I(total.sulfur.dioxide * volatile.acidity) + I(density * free.sulfur.dioxide) + I(density * pH) + I(alcohol * citric.acid) + I(alcohol * free.sulfur.dioxide), data = datos));
}


getFit = function(model, datos){
	si = 1000
	d5 = si/5
	WS = c()
	for (i in c(1, 1+d5, 1+2*d5, 1+3*d5, 1+4*d5)){
		train=datos[-(i:(i+d5-1)),]
		test=datos[(i:(i+d5-1)),]
		fit=model(train)
		pred=predict(fit, test)
		W=0
		for(j in 1:d5){
			W = W + (round(pred[j])-test$quality[j])^2
		}
		WS = c(WS, W)
	}
	return(list(model=model(datos), W=mean(WS)))
}



best_model = ""
best_W = -1
for (model_str in c("model0", "model0_important", "model1", "model1_important", "model2", "model2_important", "model3", "model3_important", "model4", "model4_important", "model5", "model5_important")){
	print(paste("Modelo", model_str))
	m = get(model_str)
	fit_and_W = getFit(m, data);
	fit = fit_and_W$model
	W = fit_and_W$W
	print(paste("5-fold CV, W =", W))
	print(paste("p-value shapiro.test:", shapiro.test(fit$residuals)$p.value))
	print("------------------------------------------")
	if (best_W == -1 | best_W > W){
		best_W = W
		best_model = model_str
	}
}

print(paste("El mejor modelo de estos es el", best_model))
chosen_model = get(best_model)
fit = chosen_model(data)

output_file = "predictions_output_file.txt"
print(paste("Los puntajes de los vinos a predecir los guardo en el archivo", output_file))
how_many_wines = 599
data_B = read.csv("/home/cherny/Downloads/vinos_B.csv", header=T)
for (i in 1:how_many_wines){
	write(round(predict(fit, data_B[i,])), output_file, append = (i > 1))
}

