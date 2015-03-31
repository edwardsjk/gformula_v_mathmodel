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
      sliderInput("b0mean",
                  "beta_0: prior mean",
                  min = -20,
                  max = 20,
                  value = 0),
      sliderInput("b0var",
                  "beta_0: prior variance",
                  min = 0.0001,
                  max = 10000,
                  value = 1),
      sliderInput("b1mean",
                  "beta_x: prior mean",
                  min = -20,
                  max = 20,
                  value = 0),
      sliderInput("b1var",
                  "beta_x: prior variance",
                  min = 0.0001,
                  max = 10000,
                  value = 1),
      sliderInput("b2mean",
                  "beta_z: prior mean",
                  min = -20,
                  max = 20,
                  value = 0),
      sliderInput("b2var",
                  "beta_z: prior variance",
                  min = 0.0001,
                  max = 10000,
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
      h3("Utilities"),
      numericInput("xcost",
      			   "Cost of X:",
      			   min=0,
      			   max=100000000,
      			   value=1,
      			   step=1
      				)
      numericInput("ycost",
      			   "Cost of Y:",
      			   min=0,
      			   max=100000000,
      			   value=1,
      			   step=1
      				)
    ),

    # Show a plot of the generated distribution
	mainPanel(
		tabsetPanel(
			tabPanel("Quick start instructions",
			br(),
			p("0. Install 'shiny' and 'Rstan' packages in R"),
			p("\n\n1. Download sample data from here: https://github.com/alexpkeil1/gformula_v_mathmodel/blob/master/data/testdata.csv"),
			p("2. Go to logistic model tab"),
			p("3. Upload data from step 1, be a little patient"),
			p("4. Profit"),
			p("5. Play around with priors, MCMC settings in left tab (better to do this before uploading data, once you have this figured out)")

			),

    		tabPanel("Logistic model",
    			fileInput('indata', 'Upload data (csv, three bernoulli variables named x,y,z)',
    				accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
    			h4("Frequentist results"),
				tableOutput("crudemodData"),
    			h4("Bayesian results"),
				dataTableOutput("bayesmodData"),
				h4("Prior and posterior distributions"),
				fluidRow(
					column(6,plotOutput("priorPlot", width=300, height=300)),
					column(6, plotOutput("postPlot", width=300, height=300))
					),
                h4("Risks and Costs"),
    				fluidRow(
    					column(6,plotOutput("riskPlot", width=300, height=300)),
    					column(6, plotOutput("costPlot", width=300, height=300))
    					),
				h4("Trace plots"),
				plotOutput("tracePlot", width=600, height=300)
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
