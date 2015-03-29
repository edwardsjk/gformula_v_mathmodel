library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel(h1("The g-formula slider, v0.1 (logistic model, 3 binary variables)"), windowTitle="The g-formula slider"),

  # Sidebar with a slider input for the number of bins
    sidebarLayout(
    sidebarPanel(
      h1("G-formula slider: Rstan"),
      h3("Priors"),
      sliderInput("sigmaparm",
                  "sigma: prior for scale parameter (inverse gamma parameters)",
                  min = 0.0001,
                  max = 10,
                  value = 0.1),
      sliderInput("b0mean",
                  "beta_0: prior mean",
                  min = -20,
                  max = 20,
                  value = 0),
      sliderInput("b0var",
                  "beta_0: prior variance",
                  min = 0.0001,
                  max = 100,
                  value = 1),
      sliderInput("b1mean",
                  "beta_x: prior mean",
                  min = -20,
                  max = 20,
                  value = 0),
      sliderInput("b1var",
                  "beta_x: prior variance",
                  min = 0.0001,
                  max = 100,
                  value = 1),
      sliderInput("b2mean",
                  "beta_z: prior mean",
                  min = -20,
                  max = 20,
                  value = 0),
      sliderInput("b2var",
                  "beta_z: prior variance",
                  min = 0.0001,
                  max = 100,
                  value = 1),
      h3("MCMC settings"),
      numericInput("nchains",
      			   "Number of chains",
      			   min=1,
      			   value=1,
      			   step=1
      				),
      numericInput("niterations",
      			   "Number of iterations",
      			   min=100,
      			   max=20000,
      			   value=500,
      			   step=100
      				),
      h3("Intervention values"),
      numericInput("xset",
      			   "Set x to:",
      			   min=0,
      			   max=1,
      			   value=1,
      			   step=1
      				)
    ),

    # Show a plot of the generated distribution
	mainPanel(
		tabsetPanel(
    		tabPanel("Logistic model",
    			fileInput('indata', 'Upload data (csv, three bernoulli variables named x,y,z)', 
    				accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
    			h4("Frequentist results"),
				tableOutput("crudemodData"),
    			h4("Bayesian results"),
				dataTableOutput("bayesmodData"),
				h4("Prior and posterior distributions"),
				fluidRow(
					column(6,plotOutput("priorPlot", width=400, height=400)),
					column(6, plotOutput("postPlot", width=400, height=400))
					),
				h4("Trace plots"),
				plotOutput("tracePlot", width=800, height=400)
    		),
    		tabPanel("Stan model, data",
    			fluidRow(
				h4("Model statement"),
    				column(12,textOutput("stanModtext")),
				h4("Imported data"),
				dataTableOutput("importData")
    			)
    		)
)))))