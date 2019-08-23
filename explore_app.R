library(shiny)
library(ggplot2)

datasetLists <- data.frame(
	dataset = c("mtcars", "iris", "ToothGrowth", "PlantGrowth", "USArrests"),
	summaryText = c(
		"<b>mtcars: Motor Trend Car Road Tests</b><br>The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models)",
		"<b>iris</b><br>iris data set gives the measurements in centimeters of the variables sepal length, sepal width, petal length and petal width, respectively, for 50 flowers from each of 3 species of iris. The species are Iris setosa, versicolor, and virginica.",
		"<b>ToothGrowth</b><br>ToothGrowth data set contains the result from an experiment studying the effect of vitamin C on tooth growth in 60 Guinea pigs. Each animal received one of three dose levels of vitamin C (0.5, 1, and 2 mg/day) by one of two delivery methods, (orange juice or ascorbic acid (a form of vitamin C and coded as VC).",
		"<b>PlantGrowth</b><br>Results obtained from an experiment to compare yields (as measured by dried weight of plants) obtained under a control and two different treatment condition.",
		"<b>USArrests</b><br>This data set contains statistics about violent crime rates by us state."
	),
	stringsAsFactors = FALSE
)

ui <- fluidPage(
	fluidRow(column(12, selectInput("data_source", "Select your data", datasetLists$dataset))),
	fluidRow(style = "padding: 0px 30px 30px 30px; background-color: #e6e6e6", column(12, HTML("<h2>Summary</h2>"), htmlOutput("summary"))),
	uiOutput("options_ui"),
	plotOutput("plot")
)

server <- function(input, output, session) {
	output$summary <-renderText({
		req(input$data_source)
		datasetLists[datasetLists$dataset == input$data_source, "summaryText"]
	})
	observeEvent(input$data_source, {
		req(input$data_source)
		data <- eval(parse(text = input$data_source))
		output$options_ui <- renderUI({
			fluidRow(
				column(3, selectInput("x_axis", "Select X axis", names(data))),
				column(3, selectInput("y_axis", "Select Y axis", names(data))),
				column(3, selectInput("color_axis", "Select Color axis", names(data))),
				column(3, style = "margin-top: 20px", checkboxInput("factorise_color", "Factorise the color axis", FALSE))
			)
		})
		output$plot <- renderPlot({
			req(input$x_axis)
			req(input$y_axis)
			req(input$color_axis)
			if (input$x_axis %in% names(data)) {
				if (input$factorise_color) {
					data[[input$color_axis]] <- as.factor(data[[input$color_axis]])
				}
				ggplot(data, aes(x = data[[input$x_axis]], y = data[[input$y_axis]], color = data[[input$color_axis]])) +
					geom_point(size = 3) +
					theme(panel.background = element_blank()) +
					labs(x = input$x_axis, y = input$y_axis, color = input$color_axis)
			}
		})
	})
}

shinyApp(ui, server)