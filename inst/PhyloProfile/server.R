#' Import function files
sourceFiles = list.files( path = "R", pattern = "*.R$", full.names = TRUE)
lapply(sourceFiles, source, .GlobalEnv)
library(PhyloProfile)

#' set size limit for input (9999mb)
options(
    shiny.maxRequestSize = 9999 * 1024 ^ 2 # size limit for input 9999mb
)

#' MAIN SERVER =================================================================
shinyServer(function(input, output, session) {
    # Automatically stop a Shiny app when closing the browser tab
    session$allowReconnect(TRUE)

    # ========================= INITIAL PARAMETERS  ============================
    mainInput <- "coronavirus/coronavirus.phyloprofile"
    var1ID <- "FAS_F"
    var2ID <- "FAS_B"
    rankSelect <- "strain"
    inSelect <- "SARS-CoV-2_GCF_009858895.2"
    
    var1AggregateBy <- "max"
    var2AggregateBy <- "max"
    var1Relation <- "protein"
    var2Relation <- "protein"

    # =========================== RENDER FILTER SLIDEBARS ======================

    # * render filter slidebars for Main plot ----------------------------------
    output$var1Cutoff.ui <- renderUI({
        createSliderCutoff(
            "var1", paste(var1ID, "cutoff:"), 0.0, 1.0, var1ID
        )
    })

    output$var2Cutoff.ui <- renderUI({
        createSliderCutoff(
            "var2", paste(var2ID, "cutoff:"), 0.0, 1.0, var2ID
        )
    })

    output$percentCutoff.ui <- renderUI({
        createSliderCutoff(
            "percent", "% of present taxa:", 0.0, 1.0, "percent"
        )
    })

    # * render filter slidebars for Customized plot ----------------------------
    output$var1Filter.ui <- renderUI({
        req(input$var1)
        createSliderCutoff(
            "var1cus",
            paste(var1ID, "cutoff:"),
            input$var1[1], input$var1[2], var1ID
        )
    })

    output$var2Filter.ui <- renderUI({
        req(input$var2)
        createSliderCutoff(
            "var2cus",
            paste(var2ID, "cutoff:"),
            input$var2[1], input$var2[2], var2ID
        )
    })

    output$percentFilter.ui <- renderUI({
        req(input$percent)
        createSliderCutoff(
            "percent2",
            "% of present taxa:",
            input$percent[1], input$percent[2], "percent"
        )
    })

    output$coorthologFilter.ui <- renderUI({
        numericInput(
            "coortholog2",
            "Max co-orthologs",
            min = 1,
            max = 999,
            step = 1,
            value = input$coortholog,
            width = 150
        )
    })

    # * render filter slidebars for Distribution plot --------------------------
    output$var1Dist.ui <- renderUI({
        createSliderCutoff(
            "var1Dist",
            paste(var1ID, "cutoff:"),
            input$var1[1], input$var1[2], var1ID
        )
    })

    output$var2Dist.ui <- renderUI({
        createSliderCutoff(
            "var2Dist",
            paste(var2ID, "cutoff:"),
            input$var2[1], input$var2[2], var2ID
        )
    })

    output$percentDist.ui <- renderUI({
        createSliderCutoff(
            "percentDist",
            "% of present taxa:",
            input$percent[1], input$percent[2], "percent"
        )
    })

    # * render filter slidebars for Core gene finding function -----------------
    output$var1Core.ui <- renderUI({
        createSliderCutoff(
            "var1Core", paste(var1ID, "cutoff:"), 0.0, 1.0,
            var1ID
        )
    })

    output$var2Core.ui <- renderUI({
        createSliderCutoff(
            "var2Core", paste(var2ID, "cutoff:"), 0.0, 1.0,
            var2ID
        )
    })

    output$percentCore.ui <- renderUI({
        createSliderCutoff(
            "percentCore",
            "% of present taxa:",
            0, 1, "percent"
        )
    })

    # * update value for filter slidebars of Main Plot -------------------------
    # ** based on customized profile
    observe({
        newVar1 <- input$var1cus
        updateSliderCutoff(
            session,
            "var1", paste(var1ID, "cutoff:"), newVar1, var1ID
        )
    })

    observe({
        newVar2 <- input$var2cus
        updateSliderCutoff(
            session,
            "var2", paste(var2ID, "cutoff:"), newVar2, var2ID
        )
    })

    observe({
        newPercent <- input$percent2
        updateSliderCutoff(
            session,
            "percent", "% of present taxa:", newPercent, "percent"
        )
    })

    observe({
        newCoortholog <- input$coortholog2
        updateNumericInput(
            session,
            "coortholog",
            value = newCoortholog
        )
    })

    # ** based on "Distribution analysis"
    observe({
        newVar1 <- input$var1Dist
        updateSliderCutoff(
            session,
            "var1", paste(var1ID, "cutoff:"), newVar1, var1ID
        )
    })

    observe({
        newVar2 <- input$var2Dist
        updateSliderCutoff(
            session,
            "var2", paste(var2ID, "cutoff:"), newVar2, var2ID
        )
    })

    observe({
        newPercent <- input$percentDist
        updateSliderCutoff(
            session,
            "percent", "% of present taxa:", newPercent, "percent"
        )
    })

    # * reset cutoffs of Main plot ---------------------------------------------
    observeEvent(input$resetMain, {
        shinyjs::reset("var1")
        shinyjs::reset("var2")
        shinyjs::reset("percent")
        shinyjs::reset("coortholog")
    })

    # * reset cutoffs of Customized plot ---------------------------------------
    observeEvent(input$resetSelected, {
        shinyjs::reset("var1")
        shinyjs::reset("var2")
        shinyjs::reset("percent")
        shinyjs::reset("coortholog")
    })

    # ====================== PROCESSING INPUT DATA =============================
    # * convert main input file in any format into long format dataframe -------
    getMainInput <- reactive({
        withProgress(message = 'Reading main input...', value = 0.5, {
            inFile <- system.file(
                "extdata", "coronavirus/coronavirus.phyloprofile",
                package="PhyloProfile"
            )
            longDataframe <- createLongMatrix(inFile)
            
            # convert geneID, ncbiID and orthoID into factor and
            # var1, var2 into numeric
            for (i in seq_len(3)) {
                longDataframe[, i] <- as.factor(longDataframe[, i])
            }
            if (ncol(longDataframe) > 3) {
                for (j in seq(4, ncol(longDataframe))){
                    longDataframe[,j] <- suppressWarnings(
                        as.numeric(as.character(longDataframe[,j]))
                    )
                }
            }
            
            # remove duplicated lines
            longDataframe <- longDataframe[!duplicated(longDataframe),]
            # update number of genes to plot based on input
            if (nlevels(as.factor(longDataframe$geneID)) <= 1500) {
                updateNumericInput(
                    session, 
                    "endIndex", value = nlevels(as.factor(longDataframe$geneID))
                )
            }
            # return
            return(longDataframe)
        })
    })
    
    # * parse domain info into data frame --------------------------------------
    getDomainInformation <- reactive({
        withProgress(message = 'Reading domain input...', value = 0.5, {
            domainFile <- system.file(
                "extdata", "coronavirus/coronavirus.domains",
                package="PhyloProfile"
            )
            domainDf <- parseDomainInput(NULL, domainFile, "file")
            return(domainDf)
        })
    })
    
    # * get ID list of input taxa from main input ------------------------------
    inputTaxonID <- reactive({
        withProgress(message = 'Getting input taxon IDs...', value = 0.5, {
            longDataframe <- getMainInput()
            inputTaxa <- getInputTaxaID(longDataframe)
            return(inputTaxa)
        })
    })
    
    # * get NAME list of all (super)taxa ---------------------------------------
    inputTaxonName <- reactive({
        req(getMainInput())
        if (rankSelect == "") return()
        withProgress(message = 'Getting input taxon names...', value = 0.5, {
            inputTaxaName <- getInputTaxaName(rankSelect, inputTaxonID())
            return(inputTaxaName)
        })
    })

    # * sort taxonomy data of input taxa ---------------------------------------
    sortedtaxaList <- reactive({
        withProgress(message = 'Sorting input taxa...', value = 0.5, {
            # get input taxonomy tree
            treeIn <- system.file(
                "extdata", "coronavirus/coronavirus.nwk",
                package="PhyloProfile"
            )
            inputTaxaTree <- read.tree(file = treeIn)

            # sort taxonomy matrix based on selected refTaxon
            sortedOut <- sortInputTaxa(
                taxonIDs = inputTaxonID(),
                rankName = rankSelect,
                refTaxon = inSelect,
                taxaTree = inputTaxaTree
            )
            # return
            return(sortedOut)
        })
    })
    
    # * count taxa for each supertaxon -----------------------------------------
    getCountTaxa <- reactive({
        taxaCount <- plyr::count(sortedtaxaList(), "supertaxon")
        return(taxaCount)
    })
    
    # * get subset data for plotting (default 30 genes if > 50 genes) ----------
    preData <- reactive({
        longDataframe <- getMainInput()
        req(longDataframe)
        # isolate start and end gene index
        input$updateBtn
        if (input$autoUpdate == TRUE) {
            startIndex <- input$stIndex
            endIndex <- input$endIndex
        } else {
            startIndex <- isolate(input$stIndex)
            endIndex <- isolate(input$endIndex)
        }
        
        if (is.na(endIndex)) endIndex <- 1000

        withProgress(message = 'Subseting data...', value = 0.5, {
            longDataframe <- unsortID(longDataframe, FALSE)
            listIn <- input$list
            if (!is.null(listIn)) {
                list <- read.table(file = listIn$datapath, header = FALSE)
                listGeneOri <- list$V1
                if (startIndex <= length(listGeneOri)) {
                    listGene <- listGeneOri[listGeneOri[startIndex:endIndex]]
                } else listGene <- listGeneOri
                data <- longDataframe[longDataframe$geneID %in% listGene, ]
            } else {
                subsetID <-
                    levels(longDataframe$geneID)[startIndex:endIndex]
                data <- longDataframe[longDataframe$geneID %in% subsetID, ]
            }
            
            if (ncol(data) < 5) {
                for (i in seq_len(5 - ncol(data))) {
                    data[paste0("newVar", i)] <- 1
                }
            }
            
            # return preData
            if (nrow(data) == 0) return()
            colnames(data) <- c("geneID", "ncbiID", "orthoID", "var1", "var2")
            return(data)
        })
    })
    
    # * creating main dataframe for subset taxa (in species/strain level) ------
    getFullData <- reactive({
        req(preData())
        req(getCountTaxa())
        req(sortedtaxaList())
        {
            input$plotCustom
            input$updateBtn
        }
        withProgress(message = 'Parsing profile data...', value = 0.5, {
            if (input$autoUpdate == TRUE) {
                coorthologCutoffMax <- input$coortholog
            } else {
                coorthologCutoffMax <- isolate(input$coortholog)
            }
            fullMdData <- parseInfoProfile(
                inputDf = preData(),
                sortedInputTaxa = sortedtaxaList(),
                taxaCount = getCountTaxa(),
                coorthoCOMax = coorthologCutoffMax
            )
            return(fullMdData)
        })
    })
    
    # * filter full data -------------------------------------------------------
    filteredDataHeat <- reactive({
        {
            input$plotCustom
            input$updateBtn
        }
        # check input file
        filein <- mainInput
        req(filein)
        withProgress(message = 'Creating data for plotting...', value = 0.5, {
            # get all cutoffs
            if (input$autoUpdate == TRUE) {
                percentCutoff <- input$percent
                coorthologCutoffMax <- input$coortholog
                var1Cutoff <- input$var1
                var2Cutoff <- input$var2
                colorByGroup <- FALSE #input$colorByGroup
            } else {
                percentCutoff <- isolate(input$percent)
                coorthologCutoffMax <- isolate(input$coortholog)
                var1Cutoff <- isolate(input$var1)
                var2Cutoff <- isolate(input$var2)
                colorByGroup <- FALSE #isolate(input$colorByGroup)
            }
            
            # get selected supertaxon name
            split <- strsplit(as.character(inSelect), "_")
            inSelect <- as.character(split[[1]][1])
            
            # get gene categories
            inputCatDt <- NULL
            
            # create data for heatmap plotting
            filteredDf <- filterProfileData(
                DF = getFullData(),
                taxaCount = getCountTaxa(),
                refTaxon = inSelect,
                percentCutoff,
                coorthologCutoffMax,
                var1Cutoff,
                var2Cutoff,
                var1Relation,
                var2Relation,
                groupByCat = colorByGroup,
                catDt = inputCatDt,
                var1AggregateBy = var1AggregateBy,
                var2AggregateBy = var2AggregateBy
            )
            return(filteredDf)
        })
    })
    
    # * heatmap data input -----------------------------------------------------
    dataHeat <- reactive({
        req(filteredDataHeat())
        dataHeat <- reduceProfile(filteredDataHeat())
        return(dataHeat)
    })
    
    # =========================== MAIN PROFILE TAB =============================
    
    # * get total number of genes ----------------------------------------------
    output$totalGeneNumber.ui <- renderUI({
        geneList <- getMainInput()
        out <- as.list(levels(factor(geneList$geneID)))
        
        listIn <- input$list
        if (!is.null(listIn)) {
            list <- read.table(file = listIn$datapath, header = FALSE)
            out <- as.list(list$V1)
        }
        if (length(out) > 0) {
            strong(paste0("Total number of genes:  ", length(out)))
        }
    })
    
    # * get list of taxa for highlighting --------------------------------------
    output$highlightTaxonUI <- renderUI({
        choice <- inputTaxonName()
        out <- as.list(levels(factor(choice$fullName)))
        out <- append("none", out)
        
        selectInput("taxonHighlight", "Select (super)taxon to highlight:",
                    out, selected = out[1])
    })
    
    # * get list of genes for highlighting -------------------------------------
    output$highlightGeneUI <- renderUI({
        geneList <- dataHeat()
        out <- as.list(levels(factor(geneList$geneID)))
        out <- append("none", out)
        selectInput("geneHighlight", "Highlight:", out, selected = out[1])
    })
    
    # * update plot size based on input ----------------------------------------
    # observe({
    #     longDataframe <- getMainInput()
    #     req(longDataframe)
    #     req(inputTaxonName())
    #     if (input$autoSizing) {
    #         # nrTaxa <- 1
    #         inputSuperTaxon <- inputTaxonName()
    #         # if (nrow(inputSuperTaxon) > 0)
    #             nrTaxa <- nlevels(as.factor(inputSuperTaxon$fullName))
    #         # nrGene <- 1
    #         # if (!is.na(input$endIndex)) 
    #             nrGene <- input$endIndex
    #         # print(nrTaxa)
    #         # print(nrGene)
    #         # adapte to axis type
    #         if (input$xAxis == "taxa") {
    #             h <- nrGene
    #             w <- nrTaxa
    #         } else {
    #             w <- nrGene
    #             h <- nrTaxa
    #         }
    #         # adapt to dot zoom factor
    #         if (input$dotZoom < -0.5){
    #             hv <- (400 + 12 * h) * (1 + input$dotZoom) + 500
    #             wv <- (400 + 12 * w) * (1 + input$dotZoom) + 500
    #         }  else if ((input$dotZoom < 0)) {
    #             hv <- (400 + 12 * h) * (1 + input$dotZoom) + 200
    #             wv <- (400 + 12 * w) * (1 + input$dotZoom) + 200
    #         } else {
    #             hv <- (400 + 12 * h) * (1 + input$dotZoom)
    #             wv <- (41500 + 12 * w) * (1 + input$dotZoom)
    #         }
    #         # minimum size
    #         if (hv < 300) hv <- 300
    #         if (wv < 300) wv <- 300
    #         # update plot size based on number of genes/taxa
    #         if (h <= 20) {
    #             updateSelectInput(
    #                 session, "mainLegend",
    #                 label = "Legend position:",
    #                 choices = list("Right" = "right",
    #                                "Left" = "left",
    #                                "Top" = "top",
    #                                "Bottom" = "bottom",
    #                                "Hide" = "none"),
    #                 selected = "top"
    #             )
    #             updateNumericInput(
    #                 session, 
    #                 "width", value = wv  + 50
    #             )
    #         } else if (h <= 30) {
    #             updateNumericInput(
    #                 session, 
    #                 "width", value = wv + 50
    #             )
    #         } else {
    #             updateNumericInput(
    #                 session, 
    #                 "width", value = wv
    #             )
    #         }
    #         updateNumericInput(
    #             session, 
    #             "height", value = hv
    #         )
    #     }
    # })
    
    # * reset configuration windows of Main plot -------------------------------
    observeEvent(input$resetMainConfig, {
        shinyjs::reset("xSize")
        shinyjs::reset("ySize")
        shinyjs::reset("legendSize")
        shinyjs::reset("xAngle")
        shinyjs::reset("dotZoom")
    })
    
    # * close configuration windows of Main plot -------------------------------
    observeEvent(input$applyMainConfig, {
        toggleModal(session, "mainPlotConfigBs", toggle = "close")
    })
    
    # * parameters for the main profile plot -----------------------------------
    getParameterInputMain <- reactive({
        input$updateBtn
        if (input$autoUpdate == TRUE) {
            inputPara <- list(
                "xAxis" = input$xAxis,
                "var1ID" = var1ID,
                "var2ID"  = var2ID,
                "lowColorVar1" =  input$lowColorVar1,
                "highColorVar1" = input$highColorVar1,
                "lowColorVar2" = input$lowColorVar2,
                "highColorVar2" = input$highColorVar2,
                "paraColor" = input$paraColor,
                "xSize" = input$xSize,
                "ySize" = input$ySize,
                "legendSize" = input$legendSize,
                "mainLegend" = input$mainLegend,
                "dotZoom" = input$dotZoom,
                "xAngle" = input$xAngle,
                "guideline" = 1,
                "width" = input$width,
                "height" = input$height,
                "colorByGroup" = FALSE #input$colorByGroup
            )
        } else {
            inputPara <- isolate(
                list(
                    "xAxis" = input$xAxis,
                    "var1ID" = var1ID,
                    "var2ID"  = var2ID,
                    "lowColorVar1" =  input$lowColorVar1,
                    "highColorVar1" = input$highColorVar1,
                    "lowColorVar2" = input$lowColorVar2,
                    "highColorVar2" = input$highColorVar2,
                    "paraColor" = input$paraColor,
                    "xSize" = input$xSize,
                    "ySize" = input$ySize,
                    "legendSize" = input$legendSize,
                    "mainLegend" = input$mainLegend,
                    "dotZoom" = input$dotZoom,
                    "xAngle" = input$xAngle,
                    "guideline" = 1,
                    "width" = input$width,
                    "height" = input$height,
                    "colorByGroup" = FALSE #input$colorByGroup
                )
            )
        }
        return(inputPara)
    })
    
    # * render dot size to dotSizeInfo ---------------------------------------
    output$dotSizeInfo <- renderUI({
        dataHeat <- dataHeat()
        dataHeat$presSpec[dataHeat$presSpec == 0] <- NA
        presentVl <- dataHeat$presSpec[!is.na(dataHeat$presSpec)]
        
        minDot <- (floor(min(presentVl) * 10) / 10 * 5) * (1 + input$dotZoom)
        maxDot <- (floor(max(presentVl) * 10) / 10 * 5) * (1 + input$dotZoom)
        
        em(paste0("current point's size: ", minDot, " - ", maxDot))
    })
    
    # * plot main profile ------------------------------------------------------
    mainpointInfo <- callModule(
        createProfilePlot, "mainProfile",
        data = dataHeat,
        # clusteredDataHeat = clusteredDataHeat,
        # applyCluster = reactive(input$applyCluster),
        parameters = getParameterInputMain,
        inSeq = reactive(input$inSeq),
        inTaxa = reactive(input$inTaxa),
        rankSelect = reactive(rankSelect),
        inSelect = reactive(inSelect),
        taxonHighlight = reactive(input$taxonHighlight),
        geneHighlight = reactive(input$geneHighlight),
        typeProfile = reactive("mainProfile")
    )

    # ======================== CUSTOMIZED PROFILE TAB ==========================
    
    # * get list of all sequence IDs for customized profile -----
    output$geneIn <- renderUI({
        filein <- mainInput
        fileCustom <- input$customFile
        data <- getFullData()
        outAll <- c("all", as.list(levels(factor(data$geneID))))
        if (input$addCoreGeneCustomProfile == TRUE) {
            outAll <- as.list(coreGeneDf())
        } else {
            if (!is.null(fileCustom)) {
                customList <- read.table(
                    file = fileCustom$datapath, header = FALSE
                )
                customList$V1 <- as.factor(customList$V1)
                outAll <- as.list(levels(customList$V1))
            }
        }
        if (outAll[1] == "all") {
            createSelectGene("inSeq", outAll, "all")
        } else {
            createSelectGene("inSeq", outAll, outAll)
        }
    })
    
    # * render popup for selecting rank and return list of subset taxa ---------
    cusTaxaName <- callModule(
        selectTaxonRank,
        "selectTaxonRank",
        rankSelect = reactive(rankSelect),
        inputTaxonID = inputTaxonID
    )
    
    # * get list of all taxa for customized profile ----------------------------
    output$taxaIn <- renderUI({
        filein <- mainInput
        if (is.null(filein)) return(selectInput("inTaxa", "", "all"))
        choice <- inputTaxonName()
        out <- c("all", as.list(levels(factor(choice$fullName))))
        selectInput("inTaxa", "",
                    out,
                    selected = out[1],
                    multiple = TRUE,
                    selectize = FALSE)
    })
    
    # * check if all genes and all species are selected ------------------------
    output$sameProfile <- reactive({
        if (length(input$inSeq[1]) == 0) return(FALSE)
        else {
            if (input$inSeq[1] == "all" & input$inTaxa[1] == "all") return(TRUE)
        }
    })
    outputOptions(output, "sameProfile", suspendWhenHidden = FALSE)
    
    # * update customized plot size based on input -----------------------------
    observe({
        longDataframe <- getMainInput()
        req(longDataframe)
        req(input$inTaxa)
        req(input$inSeq)
        if (input$selectedAutoSizing) {
            nrTaxa <- length(input$inTaxa)
            nrGene <- length(input$inSeq)
            if (input$inTaxa[1] == "all") {
                inputSuperTaxon <- inputTaxonName()
                nrTaxa <- nlevels(as.factor(inputSuperTaxon$fullName))
            }
            if (input$inSeq[1] == "all") {
                nrGene <- input$endIndex
            }
            # adapte to axis type
            if (input$xAxisSelected == "taxa") {
                h <- nrGene
                w <- nrTaxa
            } else {
                w <- nrGene
                h <- nrTaxa
            }
            # adapt to dot zoom factor
            if (input$dotZoomSelect < -0.5){
                hv <- (200 + 12 * h) * (1 + input$dotZoomSelect) + 500
                wv <- (200 + 12 * w) * (1 + input$dotZoomSelect) + 500
            }  else if ((input$dotZoomSelect < 0)) {
                hv <- (200 + 12 * h) * (1 + input$dotZoomSelect) + 200
                wv <- (200 + 12 * w) * (1 + input$dotZoomSelect) + 200
            } else {
                hv <- (200 + 12 * h) * (1 + input$dotZoomSelect)
                wv <- (200 + 12 * w) * (1 + input$dotZoomSelect)
            }
            # minimum size
            if (hv < 300) hv <- 300
            if (wv < 300) wv <- 300
            # update plot size based on number of genes/taxa
            if (h <= 20) {
                updateSelectInput(
                    session, "selectedLegend",
                    label = "Legend position:",
                    choices = list("Right" = "right",
                                   "Left" = "left",
                                   "Top" = "top",
                                   "Bottom" = "bottom",
                                   "Hide" = "none"),
                    selected = "top"
                )
                updateNumericInput(
                    session, 
                    "selectedWidth", value = wv  + 50
                )
            } else if (h <= 30) {
                updateNumericInput(
                    session, 
                    "selectedWidth", value = wv + 50
                )
            } else {
                updateNumericInput(
                    session, 
                    "selectedWidth", value = wv
                )
            }
            updateNumericInput(
                session, 
                "selectedHeight", value = hv
            )
        }
    })
    
    # * reset configuration windows of Customized plot -------------------------
    observeEvent(input$resetSelectedConfig, {
        shinyjs::reset("xSizeSelect")
        shinyjs::reset("ySizeSelect")
        shinyjs::reset("legendSizeSelect")
        shinyjs::reset("xAngleSelect")
        shinyjs::reset("dotZoomSelect")
    })
    
    # ** close configuration windows of Customized plot ------------------------
    observeEvent(input$applySelectedConfig, {
        toggleModal(session, "selectedPlotConfigBs", toggle = "close")
    })
    
    # * parameters for the customized profile plot -----------------------------
    getParameterInputCustomized <- reactive({
        input$plotCustom
        if (input$autoUpdateSelected == TRUE) {
            inputPara <- list(
                "xAxis" = input$xAxisSelected,
                "var1ID" = var1ID,
                "var2ID"  = var2ID,
                "lowColorVar1" =  input$lowColorVar1,
                "highColorVar1" = input$highColorVar1,
                "lowColorVar2" = input$lowColorVar2,
                "highColorVar2" = input$highColorVar2,
                "paraColor" = input$paraColor,
                "xSize" = input$xSizeSelect,
                "ySize" = input$ySizeSelect,
                "legendSize" = input$legendSizeSelect,
                "mainLegend" = input$selectedLegend,
                "dotZoom" = input$dotZoomSelect,
                "xAngle" = input$xAngleSelect,
                "guideline" = 0,
                "width" = input$selectedWidth,
                "height" = input$selectedHeight,
                "colorByGroup" = FALSE #input$colorByGroup
            )
        } else {
            inputPara <- isolate(
                list(
                    "xAxis" = input$xAxisSelected,
                    "var1ID" = var1ID,
                    "var2ID"  = var2ID,
                    "lowColorVar1" =  input$lowColorVar1,
                    "highColorVar1" = input$highColorVar1,
                    "lowColorVar2" = input$lowColorVar2,
                    "highColorVar2" = input$highColorVar2,
                    "paraColor" = input$paraColor,
                    "xSize" = input$xSizeSelect,
                    "ySize" = input$ySizeSelect,
                    "legendSize" = input$legendSizeSelect,
                    "mainLegend" = input$selectedLegend,
                    "dotZoom" = input$dotZoomSelect,
                    "xAngle" = input$xAngleSelect,
                    "guideline" = 0,
                    "width" = input$selectedWidth,
                    "height" = input$selectedHeight,
                    "colorByGroup" = FALSE #input$colorByGroup
                )
            )
        }
        return(inputPara)
    })
    
    # * plot customized profile ------------------------------------------------
    selectedpointInfo <- callModule(
        createProfilePlot, "customizedProfile",
        data = dataHeat,
        # clusteredDataHeat = clusteredDataHeat,
        # applyCluster = reactive(input$applyCluster),
        parameters = getParameterInputCustomized,
        inSeq = reactive(input$inSeq),
        inTaxa = reactive(input$inTaxa),
        rankSelect = reactive(rankSelect),
        inSelect = reactive(inSelect),
        taxonHighlight = reactive("none"),
        geneHighlight = reactive("none"),
        typeProfile = reactive("customizedProfile")
    )

    # ============================== POINT INFO ================================

    # * get status of pointInfo for activating Detailed Plot button -----------
    output$pointInfoStatus <- reactive({
        if (input$tabs == "Main profile") {
            # info contains groupID,orthoID,supertaxon,mVar1,%spec,var2
            info <- mainpointInfo()
        } else if (input$tabs == "Customized profile") {
            info <- selectedpointInfo()
        } else info <- NULL
        return(is.null(info))
    })
    outputOptions(output, "pointInfoStatus", suspendWhenHidden = FALSE)

    # * show info into "point's info" box --------------------------------------
    output$pointInfo <- renderText({
        # GET INFO BASED ON CURRENT TAB
        if (input$tabs == "Main profile") {
            # info contains groupID,orthoID,supertaxon,mVar1,%spec,var2
            info <- mainpointInfo()
        } else if (input$tabs == "Customized profile") {
            info <- selectedpointInfo()
        } else return()

        req(info)
        orthoID <- info[2]

        if (is.na(orthoID)) return()
        else {
            a <- toString(paste("Seed-ID:", info[1]))
            b <- toString(paste0(
                "Hit-ID: ", orthoID,
                " (", info[3], ")"
            ))
            c <- ""
            if (var1ID != "") {
                c <- toString(paste(
                    var1AggregateBy, var1ID, ":", info[4]
                ))
            }
            d <- ""
            if (var2ID != "") {
                d <- toString(paste(
                    var2AggregateBy, var2ID, ":", info[6]
                ))
            }
            e <- toString(paste("% present taxa:", info[5]))
            paste(a, b, c, d, e, sep = "\n")
        }
    })

    # ============================= DETAILED PLOT ==============================
    # * data for detailed plot -------------------------------------------------
    detailPlotDt <- reactive({
        # GET INFO BASED ON CURRENT TAB
        if (input$tabs == "Main profile") {
            # info contains groupID,orthoID,supertaxon,mVar1,%spec,var2
            info <- mainpointInfo()
        } else if (input$tabs == "Customized profile") {
            info <- selectedpointInfo()
        }
        
        req(info)
        withProgress(message = 'Getting data for detailed plot...', value=0.5, {
            ### get refspec name 
            split <- strsplit(as.character(inSelect), "_")
            inSelect <- as.character(split[[1]][1])
            
            ### get info for present taxa in selected supertaxon (1)
            fullDf <- getFullData()
            ### filter data if needed
            if  (input$detailedFilter == TRUE) {
                fullDf <- filteredDataHeat()
                if (info[3] == inSelect) {
                    fullDf <- fullDf[
                        fullDf$var1 >= input$var1[1] 
                        & fullDf$var1 <= input$var1[2], 
                    ]
                    fullDf <- fullDf[
                        fullDf$var2 >= input$var2[1] 
                        & fullDf$var2 <= input$var2[2], 
                    ]
                }
                updateCheckboxInput(
                    session, "detailedRemoveNA", value = TRUE
                )
            }
            plotTaxon <- unique(
                fullDf$supertaxon[grep(info[3], fullDf$supertaxon)]
            )
            plotGeneID <- info[1]
            selDf <- fullDf[fullDf$geneID == plotGeneID
                            & fullDf$supertaxon == plotTaxon, ]
            ### get all taxa of this supertaxon (2)
            allTaxaDf <- sortedtaxaList()
            allTaxaDf <- allTaxaDf[allTaxaDf$supertaxon == plotTaxon,
                                   c("abbrName", "fullName")]
            
            ### merge (1) and (2) together
            joinedDf <- merge(selDf, allTaxaDf, by = c("abbrName"), all.y =TRUE)
            joinedDf <- subset(
                joinedDf,
                select = c(
                    "abbrName", "fullName.y", "geneID", "orthoID", "var1","var2"
                )
            )
            names(joinedDf)[names(joinedDf) == "fullName.y"] <- "fullName"
            
            # replace var1/var2 as NA for all "NA orthologs"
            joinedDf$var1[is.na(joinedDf$orthoID)] <- NA
            joinedDf$var2[is.na(joinedDf$orthoID)] <- NA
            
            # remove NA orthologs if required
            if (input$detailedRemoveNA == TRUE) {
                joinedDf <- joinedDf[!is.na(joinedDf$orthoID), ]
            }
            
            ### return data for detailed plot
            return(joinedDf)
        })
    })
    
    # * render detailed plot ---------------------------------------------------
    pointInfoDetail <- callModule(
        createDetailedPlot, "detailedPlot",
        data = detailPlotDt,
        var1ID = reactive(var1ID),
        var2ID = reactive(var2ID),
        detailedText = reactive(input$detailedText),
        detailedHeight = reactive(input$detailedHeight)
    )
    
    # * render FASTA sequence --------------------------------------------------
    output$fasta <- renderText({
        info <- pointInfoDetail() # info = seedID, orthoID, var1
        req(info)
        seqID <- toString(info[2])
        fastain <- system.file(
            "extdata", "coronavirus/coronavirus.fasta",
            package="PhyloProfile"
        )
        fastaOut <- getFastaFromFile(seqID, fastain)
        return(paste(fastaOut[1]))
    })

    # ======================== FEATURE ARCHITECTURE PLOT =======================
    # * render domain plot -----------------------------------------------------
    observeEvent(input$doDomainPlot, {
        callModule(
            createArchitecturePlot, "archiPlot",
            pointInfo = pointInfoDetail,
            domainInfo = getDomainInformation,
            labelArchiSize = reactive(input$labelArchiSize),
            titleArchiSize = reactive(input$titleArchiSize),
            archiHeight = reactive(input$archiHeight),
            archiWidth = reactive(input$archiWidth)
        )
    })

    # ======================== FILTERED DATA DOWNLOADING =======================

    # * for main profile =======================================================
    mainFastaDownload <- reactive({
        downloadDf <- as.data.frame(downloadData())
        seqIDs <- downloadDf$orthoID
        fastain <- system.file(
            "extdata", "coronavirus/coronavirus.fasta",
            package="PhyloProfile"
        )
        mainFastaOut <- getFastaFromFile(seqIDs, fastain)
        return(mainFastaOut)
    })

    downloadData <- callModule(
        downloadFilteredMain,
        "filteredMainDownload",
        data = getFullData,
        taxaCount = getCountTaxa,
        fasta = mainFastaDownload,
        var1ID = reactive(var1ID),
        var2ID = reactive(var2ID),
        var1 = reactive(input$var1),
        var2 = reactive(input$var2),
        percent = reactive(input$percent)
    )

    # * for customized profile =================================================
    customizedFastaDownload <- reactive({
        downloadDf <- as.data.frame(downloadCustomData())
        seqIDs <- downloadDf$orthoID
        fastain <- system.file(
            "extdata", "coronavirus/coronavirus.fasta",
            package="PhyloProfile"
        )
        fastaOutDf <- getFastaFromFile(seqIDs, fastain)
        return(fastaOutDf)
    })

    downloadCustomData <- callModule(
        downloadFilteredCustomized,
        "filteredCustomizedDownload",
        data = downloadData,
        fasta = customizedFastaDownload,
        inSeq = reactive(input$inSeq),
        inTaxa = reactive(input$inTaxa)
    )

    # ============================ ANALYSIS FUNCTIONS ==========================

    # * DISTRIBUTION ANALYSIS ==================================================
    # ** description for distribution analysis function ------------------------
    observe({
        desc = paste(
            "Plot the distributions of the values incurred by the integrated
            information layers."
        )

        if (input$tabs == "Distribution analysis") {
            createAlert(
                session, "descDistributionUI", "descDistribution",
                content = desc, append = FALSE
            )
        }
    })

    # ** list of available variables for distribution plot ---------------------
    output$selected.distribution <- renderUI({
        if (nchar(var1ID) == 0 & nchar(var2ID) == 0) {
            varList <- "% present taxa"
        } else if (nchar(var1ID) == 0 & nchar(var2ID) > 0) {
            varList <- as.list(c(var2ID, "% present taxa"))
        } else if (nchar(var1ID) > 0 & nchar(var2ID) == 0) {
            varList <- as.list(c(var1ID, "% present taxa"))
        } else {
            varList <- as.list(c(var1ID, var2ID, "% present taxa"))
        }

        selectInput(
            "selectedDist", "Choose variable to plot:", varList, varList[1]
        )
    })

    # ** var1 / var2 distribution data -----------------------------------------
    distributionDf <- reactive({
        withProgress(message = 'Getting data for analyzing...', value = 0.5, {
            splitDt <- createVariableDistributionData(
                getMainInput(), input$var1, input$var2
            )
            # filter data base on customized plot (if chosen)
            if (input$dataset.distribution == "Customized data") {
                req(input$inSeq)
                splitDt <- createVariableDistributionDataSubset(
                    getFullData(),
                    splitDt,
                    input$inSeq,
                    input$inTaxa
                )
            }
            # return dt
            return(splitDt)
        })
    })

    # ** render distribution plots ---------------------------------------------
    observe({
        # req(v$doPlot)
        req(input$selectedDist)

        if (input$selectedDist == "% present taxa") {
            callModule(
                analyzeDistribution, "distPlot",
                data = reactive(
                    createPercentageDistributionData(
                        getMainInput(), rankSelect
                    )
                ),
                varID = reactive(input$selectedDist),
                varType = reactive("presSpec"),
                percent = reactive(input$percent),
                distTextSize = reactive(input$distTextSize),
                distWidth = reactive(input$distWidth)
            )
        } else {
            if (input$selectedDist == var1ID) {
                callModule(
                    analyzeDistribution, "distPlot",
                    data = distributionDf,
                    varID = reactive(input$selectedDist),
                    varType = reactive("var1"),
                    percent = reactive(input$percent),
                    distTextSize = reactive(input$distTextSize),
                    distWidth = reactive(input$distWidth)
                )
            } else if (input$selectedDist == var2ID) {
                callModule(
                    analyzeDistribution, "distPlot",
                    data = distributionDf,
                    varID = reactive(input$selectedDist),
                    varType = reactive("var2"),
                    percent = reactive(input$percent),
                    distTextSize = reactive(input$distTextSize),
                    distWidth = reactive(input$distWidth)
                )
            }
        }
    })

    # * CORE GENES IDENTIFICATION ==============================================
    # ** description for core gene identification function ---------------------
    observe({
        desc = paste(
            "IDENTIFY GENES THAT ARE SHARED AMONG SELECTED TAXA.",
            "You can set the minimal taxa that should be taken into
            account by using the \"Core taxa coverage\" cutoff.",
            "If you are working with a taxonomy level (e.g. Family)
            that is higher than the one in the input profile (e.g.
            Species), you can also identify a minimal fragtion of species
            that need to have an ortholog in each supertaxon with
            \"% of present taxa\" cutoff. WARNING: You should set the cutoffs 
            before selecting taxa of interest!"
        )

        if (input$tabs == "Core gene identification") {
            createAlert(
                session, "descCoreGeneUI", "descCoreGene",
                title = "", content = desc, append = FALSE
            )
        }
    })

    # ** render list of available taxa -----------------------------------------
    output$taxaListCore.ui <- renderUI({
        filein <- mainInput
        choice <- inputTaxonName()
        choice$fullName <- as.factor(choice$fullName)
        
        out <- as.list(levels(choice$fullName))
        out <- append("none", out)
        return(selectInput(
            "taxaCore",
            "Select taxa of interest:",
            out,
            selected = out[1],
            multiple = TRUE
        ))
    })

    # ** render popup for selecting group of taxa to find core genes -----------
    coreTaxaName <- callModule(
        selectTaxonRank,
        "selectTaxonRankCore",
        rankSelect = reactive(rankSelect),
        inputTaxonID = inputTaxonID
    )

    # ** render table contains list of core genes ------------------------------
    coreGeneDf <- callModule(
        identifyCoreGene,
        "coreGene",
        filteredData = getFullData,
        taxaCount = getCountTaxa,
        rankSelect = reactive(rankSelect),
        taxaCore = reactive(input$taxaCore),
        percentCore = reactive(input$percentCore),
        var1Cutoff = reactive(input$var1Core),
        var2Cutoff = reactive(input$var2Core),
        coreCoverage = reactive(input$coreCoverage)
    )

    # ** download gene list from coreGene.table -------------------------------
    output$coreGeneTableDownload <- downloadHandler(
        filename = function() {
            c("coreGeneList.out")
        },
        content = function(file) {
            dataOut <- coreGeneDf()
            write.table(dataOut, file, sep = "\t", row.names = FALSE,
                        quote = FALSE)
        }
    )
})
