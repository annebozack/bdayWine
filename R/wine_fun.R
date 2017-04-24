#' Plot function
#'
#' This function generates a plot with linear or exponential growth
#' @param type Linear or exponential? Default is type = 'linear' (options are type='linear' or type='exponential')
#' @param growth How much does wine effect happiness? Default is growth = 1
#' @keywords wine
#' @export
#' @examples wine_fun(type='exponential', growth='5')
#' wine_fun()

wine_fun = function(type='linear', growth=1){
	funLinear = function(x){
		x*growth
	}
	funExp = function(x){
		x^growth
	}
	p <- ggplot2::ggplot(data = data.frame(x = 0), mapping = aes(x = x))
	if (type == 'linear'){
		p + stat_function(fun = funLinear) + xlim(0,100) + labs(x='Wine (glasses)', y='Happiness (N smiles)', title='Relationship between wine consumption and happiness')
	} else if (type == 'exponential'){
		p + stat_function(fun = funExp) + xlim(0,100)
	}
}
