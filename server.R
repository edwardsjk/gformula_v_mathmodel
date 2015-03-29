library(shiny)
library(rstan)
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

	newData <- reactive({
		inFile <- input$indata
		if(is.null(inFile)) return(NULL)
		read.csv(inFile$datapath)
	})
	output$importData <- renderDataTable({
		dat <- newData()
		if(is.null(dat)) return(NULL)
		newData()
	},options = list(pageLength = 5)
	)
	output$priorPlot <- renderPlot({
		dat <- newData()
		if(is.null(dat)) return(NULL)
		plot(as.data.frame(dat))
		lim1=c(input$b0mean-3*input$b0var, input$b0mean+3*input$b0var)
		lim2=c(input$b1mean-3*input$b1var, input$b1mean+3*input$b1var)
		lim3=c(input$b2mean-3*input$b2var, input$b2mean+3*input$b2var)
		par(mfrow=c(1,3))
		curve(dnorm(x, input$b0mean, input$b0var), xlim=lim1, xlab="b0")
		curve(dnorm(x, input$b1mean, input$b1var), xlim=lim1, xlab="bx")
		curve(dnorm(x, input$b2mean, input$b2var), xlim=lim1, xlab="bz")
		
	})

	
	
	crudeModel <- reactive({
		dat <- newData()
		if(is.null(dat)) return(NULL)
		lm(dat$y ~ dat$x + dat$z, family=binomial)
	})

	stanModel <- reactive({
		paste0(
		'data {
		int<lower=0> N;
		
		vector[N] x;
		vector[N] z;
		vector[N] y;
		}
		
		parameters {
		real b0;
		real bx;
		real bz;
		real sigma;
		}
		model{
		 vector[N] mu;
		 sigma ~ inv_gamma(',input$sigmaparm, ',',input$sigmaparm,');
		 b0 ~ normal(',input$b0mean,',', input$b0var,');
		 bx ~ normal(',input$b1mean,',', input$b1var,');
		 bz ~ normal(',input$b2mean,',', input$b2var,');
		 mu <- b0  + bx * x + bz * z;
		 y ~ normal(mu, sqrt(sigma));
		}
		generated quantities{
		  real fy;
		  fy <- mean(b0  + bx * ',input$xset,' + bz * z);

		}')
	})
	

	stanData <- reactive({
		dat <- newData()
		if(is.null(dat)) return(NULL)
		list(N=length(dat$x), x=dat$x, y=dat$y, z=dat$z)
	})
	
	output$stanDatatext <- renderText({
		stanModel()
	})
	output$stanModtext <- renderText({
		stanModel()
	})
	
	bayesModel <- reactive({
		dat <- newData()
		if(is.null(dat)) return(NULL)
		#model
		mod <- stanModel();
		#data
		datastan <- stanData()
		#stan code
		stan(model_code = mod, data = datastan, 
			iter=input$niterations, 
			chains=input$nchains, pars=c("fy", "bx", "bz", "b0"), verbose=FALSE)
	})
	
	getBayesresults <- reactive({
		mod <- bayesModel()
		if(is.null(mod)) return(NULL)
		data.frame(extract(mod))
	})
	
	output$crudemodData <- renderTable({
		crudeModel()
	})
	output$bayesmodData <- renderDataTable({
		posterior <- getBayesresults()
		if(is.null(posterior)) return(NULL)
		df <- data.frame(apply(posterior, 2, function(x) c(mean=mean(x), std=sd(x), median=median(x), p=quantile(x, 0.025), p=quantile(x, 0.975))))
		df$stat = rownames(df)
	})
	
	output$postPlot <- renderPlot({
		posterior <- getBayesresults()
		if(is.null(posterior)) return(NULL)
		par(mfrow=c(1,3))
		plot(density(posterior$b0))
		plot(density(posterior$bx))
		plot(density(posterior$bz))
	})
	output$tracePlot <- renderPlot({
		posterior <- getBayesresults()
		if(is.null(posterior)) return(NULL)
		iter = 1:length(posterior$b0)
		par(mfrow=c(1,3))
		plot(x=iter, y=(posterior$b0), type="l")
		plot(x=iter, y=(posterior$bx), type="l")
		plot(x=iter, y=(posterior$bz), type="l")
	})


})