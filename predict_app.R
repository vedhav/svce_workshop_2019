library(shiny)
library(randomForest)

ui <- fluidPage(
	fluidRow(
		column(6, fileInput("input_file", "Select your file")),
		fluidRow(
			style = "margin-top: 25px",
			column(3, actionButton("predict_button", "Predict")),
			column(3, downloadButton("export_button", "Export"))
		)
	),
	fluidRow(column(6, HTML("<h2>Input Table</h2>")), column(6, HTML("<h2>Predicted Output</h2>"))),
	fluidRow(
		column(6, tableOutput("input_table")),
		column(6, tableOutput("predict_table"))
	)
)

server <- function(input, output, session) {
	input_data <- data.frame()
	predicted_data <- data.frame()
	# Training using Random Forest
	model <- randomForest(Species ~ ., data = iris)
	output$input_table <- renderTable({
		inFile <- input$input_file
		if (is.null(inFile)) {
			return()
		}
		input_data <<- read.csv(inFile$datapath)
	})
	observeEvent(input$predict_button, {
		if (nrow(input_data) == 0) {
			return()
		}
		predicted_data <<- input_data
		predicted_data$Species <<- predict(model, input_data)
		output$predict_table <- renderTable({
		if (nrow(predicted_data) == 0) {
				return(NULL)
			}
			predicted_data
		})
	})
	output$export_button <- downloadHandler(
        filename = function() {
            paste("iris_predict_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(predicted_data, file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)