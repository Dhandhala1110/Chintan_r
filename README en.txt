Notes on the assistance system and program code

IMPORTANT!!!
When executing the program for the first time, set variable "firstuse" in file app.R line 2 to TRUE. This is used to install the required libraries. At further starts the variable can be set to FALSE.

Starting the application from R-Studio
    Start in a R-Studio window, execute the file app.R
    Start in a browser window, execute the file start.R

Start the application via icon
	Execute 'KnowledgeStore.cmd' --> Adapt file paths within the command file!!!

    
User interface:
    Split into 4 tabs(overall structure, process intervention, error overview, parameter learning).
    Tab Descriptions:
        - Overall structure: displaying the two main Bayes nets "preform" and "blow mold part", updating the graph by button 'Refresh' or tab change, Interactive graph thus zooming and moving nodes is possible, by clicking on a node all other nodes with direct edge will be highlighted and all others will be greyed out.
        
        - Process intervention: Determination of error cause, selection option of cause type most probable (default) or most influential, selection of required error image via two drop-down menus, start of prediction via button 'Error cause determination', cancel process via button 'Cancel', predicted cause is output textually and displayed graphically on the right side (green error nodes, red cause node), bottom left table output with descending causes, feedback to system whether error has been corrected or further cause is required, error not corrected (button 'No') and a manual cause System queries node state before intervention, error corrected (button 'Yes') System queries open remaining manual cause states via a pop-up window
            At the start of the pre-prediction as well as every further activation of the buttons 'No' and 'Yes' a data extraction of the CPU data is triggered --> Currently not implemented. Random data is generated and used.
            Data preparation must still be implemented, divide received CPU data into the intended classes.
        
        - Defect overview: Display of single defect image, selection of the defect image via two drop-down menus (preform/blow molding and defect image) possible, the graphic is refreshed via the 'Display' button, the defect node is highlighted in blue, all other cause nodes are displayed with a white background color, no interactive graph, therefore zoom and panning are not possible.
        
        - Parameter learning: Bayes net can be trained with data set, Bayes net selection between the two main nets "preform" and "blow moulded part", via button 'Browse' select corresponding data set (layout format separate columns by ; and no line numbering) for training, uploaded data are displayed as table and saved as csv file under "Data_archive/data_download.csv", training is started via the 'Learn parameters' button (accesses the saved data set, after training a message appears indicating that the parameters of the Bayesian network have changed).
        
        
Program code is divided into two sections.
    Section 1: ui; There the layout is defined and designed --> placing inputs and outputs on the screen.
                For each tab there is a separate section tabItem(tabName = <tabname>...
                By means of inputID/ outputID the objects are accessed within the server section.
    Section 2: server; This is where functions and algorithms, the actual logic is executed. There is access to data and defined functions (functions.R).
                Optical division/ delimitation of the individual areas of the tabs
                
    Functions are defined in the functions.R file. There the functions are summarized and structured in content sections.

    In the main.R file libraries and own defined functions are loaded, Python is started and if not available the two txt files for the model strings of the two main Bayes nets are created.
