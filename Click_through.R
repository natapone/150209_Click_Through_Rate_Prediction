# USAGE
# source("Click_through.R")
# train_set = read_data('train',1000)
# train_set = read_data('train',data_source="web", limit=0)
# test_set = read_data('test')
# train_set = read_clean_data(data_source="web")
# train_set = read_clean_data(column_names, data_source="web")

# Count
# validate_count_unique(train_set)
# test_without_train(train_set, test_set)
# all_level = list_all_level()

# Clean up
# clean_data(train_set)

# RARE fix: reduce level of factor and support new value
# > agg = replace_agg_rare(agg,p_rare=0.1)

# Test Data
# d = head(train_set, 100)
# t = interpret_data(train_set)
# plot_aggregate_to_click(train_set)
# plot_aggregate_to_click_banner_C14(train_set)

# Train
# k = caret_train(train_set)
# m = train_model_intersect_prob(train_set)

# Train Model relation score
# prep_model_relation_score(data_source="web", train_test_ratio=0.1)
# m = train_model_relation_score(data_source="web", train_percent=1)

# Predict Model relation score ***
# data = prep_model_relation_score(data_source="web", mode="test")

# Proof
# proof_split_site_app_domain()

# = Train Columns =
# id
# click **
# hour
# C1
# banner_pos
# site_id
# site_domain
# site_category
# app_id
# app_domain
# app_category
# device_id
# device_ip
# device_model
# device_type
# device_conn_type
# C14
# C15
# C16
# C17
# C18
# C19
# C20
# C21


require(data.table)
library(caret)
library(ggplot2)

train_model_relation_score <- function(data_source="web", file_name = "relation_score", train_percent=1) {
    
    # read data
    training = readRDS(paste("model", file_name, data_source, "training.RData", sep="/"))
    testing = readRDS(paste("model", file_name, data_source, "testing.RData", sep="/"))
    training_click = readRDS(paste("model", file_name, data_source, "training_click.RData", sep="/"))
    testing_click = readRDS(paste("model", file_name, data_source, "testing_click.RData", sep="/"))
    
    if (train_percent < 1) {
        inTrain = createDataPartition(y=training_click, p=train_percent, list=FALSE)
        training = training[inTrain[,1],]
        training_click = training_click[inTrain[,1]]
    }
    print("Finish reading files")
    
    # train
    system.time(
        modelFit <- train(x=training,y=training_click, method="glm")
    )
    file_path = paste("model", file_name, data_source, "modelFit.RData", sep="/")
    saveRDS(modelFit, file_path)
    cat( "Save model to", file_path, "\n"  )
    
    # test prediction
    predictions = predict(modelFit, newdata=testing)
    file_path = paste("model", file_name, data_source, "predictions.RData", sep="/")
    saveRDS(predictions, file_path)
    cat( "Save test prediction to", file_path, "\n"  )
    
    # plot cut off level
    library("pROC")
    op = roc(testing_click, predictions, algorithm = 2,plot=T)
    file_path = paste("model", file_name, data_source, "roc.RData", sep="/")
    saveRDS(op, file_path)
    cat( "Save ROC to", file_path, "\n"  )
    
    #
    return(1)
}

prep_model_relation_score <- function(data_source="web", file_name = "relation_score", train_test_ratio=0.6, mode="train") {
#     data_source = check_source(test_set)
    
    cat("data_source =", data_source, "\n")
#     print(data_source)
    
    if (data_source == "web") {
        combo_list = list(
            c("banner_pos","C1"),
            c("banner_pos","C17"),
            c("device_model","C14"),
            c("device_model","C15"),
            c("device_model","C16"),
            c("device_model","C17"),
            c("device_model","C21"),
            c("site_category","C14"),
            c("site_category","C15"),
            c("site_category","C16"),
            c("site_category","C21")
            
        )
        
        
    } else {
        combo_list = list(
            c("app_category","C15"),
            c("app_category","C17"),
            c("app_category","C19"),
            c("app_category","C20"),
            c("app_category","C21"),
            c("banner_pos","C1"),
            c("device_type","C20"),
            c("device_model","C17")
        )
    }
    
    # read relatioship score
    rel_score = NULL
    
    for (combo in combo_list) {
        
        main_col = combo[1]
        ob_col = combo[2]
        
        score_file = paste(data_source, main_col, ob_col, sep="_")
        score_file = paste("plot/cat", score_file, sep="/")
        score_file = paste(score_file, "RData", sep=".")
        
        cat("Read score file:", score_file, "\n")
        agg = readRDS(score_file)
        
        if(is.null(rel_score)) {
            rel_score = agg
        } else {
            rel_score = rbind(rel_score, agg)
        }
        #  agg[agg$names == "banner_pos1:C1RARE",]
        
        
    }
#     return(rel_score)
    
    # read data
    col_list = unlist(combo_list)
    col_list = unique(col_list)
#     col_list = c(col_list, "click")
    data = read_clean_data(col_list=col_list, file_name=mode, data_source=data_source)
#     data = head(data, 1000)
#     return(data)
    
    # loop cal score from each combo columns
    cal_score <- function(row) {
#         print(row)
        combo_score_max = list() # max score of each focus group
        for (combo in combo_list) {
            
            main_col = combo[1]
            ob_col = combo[2]
            
#             cat("Cal:", main_col, "-", ob_col, "\n")
            val_main_col = row[[main_col]]
#             cat(" -", main_col, "=", val_main_col, "\n")
            
            val_ob_col = row[[ob_col]]
#             cat(" -", ob_col, "=", val_ob_col, "\n")
            
            combo_index = paste(main_col, val_main_col, ":", ob_col, val_ob_col, sep="")
            
            combo_rel_score = rel_score[rel_score$names == combo_index,]
            if(is.null(combo_rel_score)) {
                combo_click_score = -1 # assume no click
            } else {
                combo_click_score = combo_rel_score$click
            }
#             cat(" -->", combo_index, "=", combo_click_score, "\n")
            
            # check max
            if(is.null(combo_score_max[[main_col]])) {
                combo_score_max[[main_col]] = combo_click_score
            } else if(combo_click_score > combo_score_max[[main_col]]) {
                combo_score_max[[main_col]] = combo_click_score
            }
            
#             return(1)
        }
#         print(combo_score_max)
#         cat("============================\n")
        return(combo_score_max)
    }
    
    data = apply(data, 1, cal_score)
#     return(data)
    
    # column name
    combo_score_col_name = names(data[[1]])
    
    # convert list to data.frame
    data = data.frame(matrix(unlist(data), nrow=length(data), byrow=T)) # no col name
    setnames(data, combo_score_col_name) # rename columns, less ram
    
    # return data for prediction
    if (mode == "test") {
        file_path = paste("model", file_name, "test",data_source, "data.RData", sep="/")
        saveRDS(data, file_path)
        cat("Save test data to", file_path, "\n")
        return(data)
    }
    
    # click
    data_click = read_clean_data(col_list=c("click"), file_name="train", data_source=data_source)
    data_click = data_click$click
#     data_click = head(data_click, 1000)
    
    # split train / test
    inTrain = createDataPartition(y=data_click, p=train_test_ratio, list=FALSE)
    
    training = data[inTrain[,1],] # first column is row index
    testing  = data[-inTrain[,1],]
    training_click = data_click[inTrain[,1]]
    testing_click  = data_click[-inTrain[,1]]
    
    
    saveRDS(training, paste("model", file_name, data_source, "training.RData", sep="/"))
    saveRDS(testing, paste("model", file_name, data_source, "testing.RData", sep="/"))
    saveRDS(training_click, paste("model", file_name, data_source, "training_click.RData", sep="/"))
    saveRDS(testing_click, paste("model", file_name, data_source, "testing_click.RData", sep="/"))
    return(1)

    saveRDS(training, "tmp/training.RData")
    saveRDS(testing, "tmp/testing.RData")
    saveRDS(training_click, "tmp/training_click.RData")
    saveRDS(testing_click, "tmp/testing_click.RData")
    return(1)
    
    training = readRDS("tmp/training.RData")
    testing = readRDS("tmp/testing.RData")
    training_click = readRDS("tmp/training_click.RData")
    testing_click = readRDS("tmp/testing_click.RData")

    system.time(modelFit <- train(x=training,y=training_click, method="glm"))
    saveRDS(modelFit, "tmp/modelFit.RData")
    #######
    predictions = predict(modelFit, newdata=testing)
    saveRDS(predictions, "tmp/predictions.RData")

#     cbind(predictions, testing_click)

    ####### plot cut off level
    library("pROC")
    op = roc(testing_click,predictions, algorithm = 2,plot=T)
    saveRDS(op, "tmp/op.RData")
    # 0.5422
    which(predictions >= 0.6449) # web
#     which(predictions >= 0.705) # app
    
#     confusionMatrix(predictions,testing$type)
    
}

predict_model_intersect_prob <- function(test_set) {
    # data is web or app
    app_index = which(train_set$site_domain == "c4e18dd6")
    if (length(app_index) > 0) {
        data_source = "app"
    } else {
        data_source = "web"
    }
    
    # read model file
    model_path = paste("rdata/model_intersect_prob_",data_source,".RData",sep="")
    m <- readRDS(model_path)
    
    # if there is column "click" -> test mode
    test_mode = FALSE
    if ( "click" %in% colnames(test_set)) {
        test_mode = TRUE
        print("== Test mode ==")
    }
    
    # loop read row
    for(n in 1:nrow(test_set)) {
        total_prob = 1
        t = test_set[n,]
        
#         print(t)
        
        
        # loop column
        for (col_name in colnames(t)) {
            
            col_val = t[[col_name]]
#             cat(col_name)
#             print(col_val)
            
            
            if (!is.null(m[[col_name]])) {
#                 return(m[[col_name]])
                
                # avg, count
                # return mean
                col_mean = m[[col_name]][[col_val]][1]
                
                # if null, use RARE or total click mean
                if(is.null(col_mean)) {
                    # RARE
                    if(!is.null(m[[col_name]][["RARE"]][1])) {
#                         cat("RARE ")
                        col_mean = m[[col_name]][["RARE"]][1] / 100
                    } else {
#                         cat("MISS ")
#                         col_mean = m[["CTR"]]
                        col_mean = 0
#                         cat(col_name) = 0
                    }
                    
                    
                    
                } else {
                    col_mean = col_mean / 100
                }
                
                # cal prob
                total_prob = total_prob * col_mean
                
#                 cat("---------------")
#                 cat(col_mean)
#                 cat(" => ")
#                 print(total_prob)
            }
            
#             return(m[[col_name]])
            
            #["1005"][[1]][1]
        }
        
        cat("Click ")
        cat(t$click)
        cat(" ++++ Total prob == ")
        print(total_prob)
#         return(total_prob)
    }
    
    # read from model
    
    
}

train_model_intersect_prob <- function(train_set, file_name = "intersect_prob", p_rare=1) {
    
    # validate if data is from web or app
    app_index = which(train_set$site_domain == "c4e18dd6")
    if (length(app_index) > 0) {
        data_source = "app"
    } else {
        data_source = "web"
    }
    
    file_name = paste("model",file_name, data_source, sep="_")
    file_name = paste(file_name, "RData", sep=".")
    file_name = paste("rdata", file_name, sep="/")
    
    cat("data_source = ")
    print(data_source)
    
    # refactoring from plot_aggregate_to_click
    if (data_source == "app") {
        col_list = c(
            "C1",
            "banner_pos",
    #         "site_id",
    #         "site_domain",
    #         "site_category",
    #         "app_id",
            "app_domain",
            "app_category",
    #         "device_id",
    #         "device_ip",
            "device_model",
            "device_type",
            "device_conn_type",
            "C14",
            "C15",
            "C16",
            "C17",
            "C18",
            "C19",
            "C20",
            "C21"
        )
    } else {
        col_list = c(
            "C1",
            "banner_pos",
    #         "site_id",
            "site_domain",
            "site_category",
    #         "app_id",
    #         "app_domain",
    #         "app_category",
    #         "device_id",
    #         "device_ip",
            "device_model",
            "device_type",
            "device_conn_type",
            "C14",
            "C15",
            "C16",
            "C17",
            "C18",
            "C19",
            "C20",
            "C21"
        )
    }
    
    all_row = nrow(train_set)
    
    cat("Rare if frequency <")
    cat(p_rare)
    print(" %")
    
    model = list()
    for (col_name in col_list) {
        print ( paste("Cal click -", col_name) )
        
        agg = aggregate(train_set$click, list(train_set[[col_name]]),   
                        FUN=function(x) c(
                            "rate"   =sum(x) / all_row * 100, # click rate compare to all
                            "count"  =length(x) / all_row * 100 # total rows
                        ) # return as matrix
        )
        #         return(agg)
        names(agg) = c(col_name, "summary")
        agg = agg[order(-agg$summary[,"rate"]),]
        
        agg = replace_agg_rare(agg, p_rare)
        
        #         return(agg)
        
        # save to model as list()
        
        f = list()
        for(n in 1:nrow(agg)) {
            a = agg[n,]
            # f = convert_freq(a)
            # print(agg[1])
            
            f[[ agg[n, 1] ]] = agg[n, 2]
            
        }
        # to read avg
        # f["1005"][[1]][1]
        # or
        # a = f["1005"]
        # a[[1]][1]
        # or 
        # m$C21$RARE[1]
        
        model[[col_name]] = f
    }
    
    # If no key is matched, use total CTR
    model[["CTR"]] = mean(train_set$click) * 100
    
    # save to file
    saveRDS(model, file = file_name)
    cat("Save model to ")
    print(file_name)
    
    model
}

create_dummy_var <- function(data) {
    # read levels from file
    all_level = readRDS("rdata/all_level.RData")
    
    # loop transform column data in to dummy var
    list_factors = c()
    for (col_name in colnames(data)) {
        # print(col_name)
        
        col_data = data[[col_name]]
        
        if ( !is.null(all_level[[col_name]])) {
            print( c(col_name , "HIT!") )
            
            # all levels
            levels(data[[col_name]]) = all_level[[col_name]]
            list_factors <- c(list_factors, col_name)
        }
        
    }
    list_factors
    # variable as formular
    f = as.formula(paste("~",paste(list_factors,collapse="+")))
    
    # keep "click" if Training set
    click = NULL
    if ( "click" %in% colnames(data)) {
        click = as.factor(data$click) # force caret to do classification
    }
    
    print ("-- start model.matrix")
    data = model.matrix(f, data) # replace instead of create new
    
    print ("-- convert to data.table")
    data = as.data.table(data)
    
    # return click if Training set
    if ( !is.null(click) ) {
        data = cbind(data, click)
    }

    data
}

# prove data is split into 2 group by site and app
# Mark NULL: site_domain = c4e18dd6, app_domain = 7801e8d9
proof_split_site_app_domain <- function(train_set) {
    site_index = which(train_set$site_domain == "c4e18dd6") # 15121739 entries
    cat("Site domain c4e18dd6 count = ")
    print (length(site_index))
    
    not_site  = train_set[-site_index,]
    cat("NOT site count = ")
    print (nrow(not_site))
    
    proof_index = which(not_site$app_domain == "7801e8d9")
    cat("App domain 7801e8d9 count =")
    print (length(proof_index))
    
    if(length(proof_index) == nrow(not_site)) {
        print("== Number not site match number of app ==")
        print("## proof data is splited between APP and SITE ##")
    } else {
        print("== no good! ==")
    }
}

plot_aggregate_to_click_banner_C14 <- function(train_set) {
    
    col_list = c(
        "banner_pos",
        "C14"        
    )
    
    agg = aggregate(train_set$click, list(train_set$banner_pos, train_set$C14),   
                    FUN=function(x) c(
                        "avg" =mean(x) * 100, # return %
                        "count"  =length(x) / 40428967 * 100 # total rows
                    ) # return as matrix
    )
    
    names(agg) = c("banner_pos", "C14", "summary")
    agg = agg[order(-agg$summary[,"avg"]),]
    
    # Plot
    file_name = "plot/aggregate_to_click_banner_C14.png"
    
    banner_pos_C14 = paste(agg$banner_pos, agg$C14, sep="-")
    agg = cbind(agg, banner_pos_C14)
    
#     print(agg)
#     agg

    ggplot(data=agg, aes_string(x="banner_pos_C14", y="summary[,'avg']", colour="summary[,'count']", size="summary[,'count']")) + 
        geom_point( alpha=0.7) +
        scale_colour_gradientn( colours=rainbow(7)) +
        #             scale_colour_gradient2(low="blue", high="red", mid="green", midpoint=0) +
        geom_hline(yintercept=80, colour="darkgreen", linetype = "longdash") +
        geom_hline(yintercept=5, colour="red", linetype = "longdash") +
        theme(
            panel.background=element_rect(fill="grey97")
        )
    
    ggsave(file=file_name, width=10, height=6)
}

plot_aggregate_to_click <- function(train_set) {
    # mean click by fields 
    
    col_list = c(
        
                "C1",
                "banner_pos",
#                 "site_id",
                "site_domain",
                "site_category",
#                 "app_id",
                "app_domain",
                "app_category",
#                 "device_id",
#                 "device_ip",
                "device_model",
                "device_type",
                "device_conn_type",
                "C14",
                "C15",
                "C16",
                "C17",
                "C18",
                "C19",
                "C20",
                "C21"
        
    )
    
    
    # multiple
    # aggregate(train_set$click, list(train_set$banner_pos, train_set$app_category), mean  )
    
    all_row = nrow(train_set)
    
    # Single
    for (col_name in col_list) {
        print ( c("Aggregate click -", col_name), quote=FALSE )
        
        agg = aggregate(train_set$click, list(train_set[[col_name]]),   
                    FUN=function(x) c(
                        "avg" =mean(x) * 100, # return %
                        "count"  =length(x) / all_row * 100 # total rows
                    ) # return as matrix
            )
#         return(agg)
        names(agg) = c(col_name, "summary")
        agg = agg[order(-agg$summary[,"avg"]),]
        
        agg = replace_agg_rare(agg)
        
#         return(agg)
        print(agg)
        
        # Plot
        file_name = paste("aggregate_to_click",col_name,sep="-")
        file_name = paste(file_name,"png",sep=".")
        file_name = paste("plot",file_name,sep="/")
        
        ggplot(data=agg, aes_string(x=col_name, y="summary[,'avg']", colour="summary[,'count']", size="summary[,'count']")) + 
            geom_point( alpha=0.7) +
            scale_colour_gradientn( colours=rainbow(7)) +
#             scale_colour_gradient2(low="blue", high="red", mid="green", midpoint=0) +
            geom_hline(yintercept=80, colour="darkgreen", linetype = "longdash") +
            geom_hline(yintercept=5, colour="red", linetype = "longdash") +
            theme(
                panel.background=element_rect(fill="grey97")
            )

        ggsave(file=file_name, width=10, height=6)
        
    }
}

# Model list http://topepo.github.io/caret/modelList.html
caret_train <- function(data, model_seed=32343, data_model="simple", caret_model="glm") {
    set.seed(model_seed)
    
    # map field to model
    data = interpret_data(data, data_model, "train")
    
    inTrain = createDataPartition(y=data$click, p=0.1, list=FALSE)
    
    training = data[inTrain[,1],] # first column is row index
    testing  = data[-inTrain[,1],]
    
    ###
    summary(training)
    return (testing)
    
    cat ("Train data dimension ") 
    print (dim(training))
    
    cat ("Test data dimension ") 
    print (dim(testing))
    
    drops = c("click") # separate test result column
#     x = data[,!(names(data) %in% drops), with=FALSE]
    
    modelFit <- train(click ~.,data=t, method="glm") # ada
    
knnFit1 <- train(click ~.,data=d,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
    
    modelFit <- train(
                click ~.,
                data=training, 
                method = "nnet",
                preProcess = "range", 
                tuneLength = 2,
                trace = FALSE,
                maxit = 100
            )
    

    modelFit <- train(
                click ~.,
#                 data= as.data.frame.matrix(training), 
                data=training, 
                preProcess = "range", 
                tuneLength = 2,
#                 trace = FALSE,
                maxit = 50,
                method = "nnet"
            )
   
# call nnet directly
# nnetFit <- nnet(
#     click ~.,
#     data=training, 
# #     rang = 0.1,
# #     decay = 0.01, 
#     size = 10,
# #     softmax=TRUE,
# #     linout=TRUE,
#     entropy=TRUE,
#     maxit = 10
# )



# nnetFit <- nnet(
#     training[,1:( ncol(training)-1 ),with=F], 
#     training[,"click",with=F],
#     #     rang = 0.1,
#     #     decay = 0.01, 
#     size = 1,
#     #     softmax=TRUE,
#     #     linout=TRUE,
# #     entropy=TRUE,
#     maxit = 10
# )


#====================

    modelFit
    
    
    
    
    
    return(modelFit)
    
    
#     1
}

int_model_simple <- function(data, type) {
    # simple model
    # banner_pos, site_category, app_category, device_type, device_conn_type
    
    col_list = c(
        
#         "C1",
        "banner_pos",
#         "site_id",
#         "site_domain",
        "site_category",
#         "app_id",
#         "app_domain",
        "app_category"
#         "device_id",
#         "device_ip",
#         "device_model",
#         "device_type",
#         "device_conn_type",
#         "C14",
#         "C15",
#         "C16",
#         "C17",
#         "C18",
#         "C19",
#         "C20",
#         "C21"
    )
    
    if (type == "train") {
        col_list = c(col_list, "click")
    }
    
    data = data[, col_list, with=FALSE]
    
    # create Dummy Variables
    data = create_dummy_var(data)

    data
}

interpret_data <- function( data, model="simple", type="train") {
    print ( c("Model", type, model), quote=FALSE )
    if (model == "simple") {
        data = int_model_simple(data, type)
    } else {
        return (NULL)
    }
    
    data
}

read_data <- function(file_name, data_source="all", limit=0, col_class_list=NULL) {
    if(is.null(col_class_list)) {
        # column data type ny file name
        if (file_name == 'train') {
            col_class_list = c(
                "NULL",
                "numeric",
                "numeric",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character"
            )
        } else if (file_name == 'test') {
            # no click data
            col_class_list = c(
                "NULL",
                "numeric",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character",
                "character"
            )
        }
    }

    # read data by type
    file_path = paste("Raw", file_name, sep = "/")
    
    #data = fread(file_path) # much faster
#     data = fread(file_path, header = TRUE, colClasses = col_class_list) # acceptable fast, 10 min
#     return(data)
    # end test
    
    if (limit) {
        data = fread(file_path, nrows=limit, colClasses = col_class_list,
                     sep=",", stringsAsFactors=FALSE)
    } else {
        data = fread(file_path, colClasses = col_class_list,
                     sep=",", stringsAsFactors=FALSE)
    } # acceptable fast, 10 min
    
    if (data_source == "all") {
        return(data)
    }
    
    #read app index from file
    app_index_filename = paste("clean/", file_name, "_app_index.RData", sep="")
    app_index = readRDS(app_index_filename)
    
    # prevent reading everything if index is out of bound
    if (limit) {
        app_index = app_index[app_index <= limit]
    }
    
    if (data_source == "web" ) {
        data = data[-app_index,]
    } else if (data_source == "app") {
        data = data[app_index,]
    }
    remove(app_index)
    
    data
}

validate_count_unique <- function(data) {
    # loop through columns
    
    for (col_name in colnames(data)) {
        # print(col_name)
        
        col_data = data[[col_name]]
        col_data = unique(col_data) # only unique
        
        #print (length(data[[col_name]])) # count data
        print( c(col_name , length(col_data)) )
        
        
        # test[,i]
    }
    
}

test_without_train <- function(train, test) {
    
    field_list = c(
        "hour",
        "C1",
        "banner_pos",
        "site_id",
        "site_domain",
        "site_category",
        "app_id",
        "app_domain",
        "app_category",
        "device_id",
        "device_ip",
        "device_model",
        "device_type",
        "device_conn_type",
        "C14",
        "C15",
        "C16",
        "C17",
        "C18",
        "C19",
        "C20",
        "C21"
    )
    
    for (col_name in field_list) {
        
        col_data_train = train[[col_name]]
        col_data_train = unique(col_data_train)
        
        col_data_test = test[[col_name]]
        col_data_test = unique(col_data_test)
        
        # test that not in train
        miss = setdiff(col_data_test, col_data_train)
        
        print ( 
            c(
                col_name,
                length(miss)
            )
        )
    }
    
}

list_all_level <- function(test_set,limit=0) {
    col_list = c(
        
        "C1",
        "banner_pos",
        #         "site_id",
        #         "site_domain",
        "site_category",
        #         "app_id",
        #         "app_domain",
        "app_category",
        #         "device_id",
        #         "device_ip",
        #         "device_model",
        "device_type",
        "device_conn_type",
        #         "C14",
        "C15",
        "C16",
        #         "C17",
        "C18"
        #         "C19",
        #         "C20",
        #         "C21"
    )
    
    # read train
    train_set = read_data('train',limit)
    
    print("Read train set")
    train_col = list_all_level_read_col(col_list, train_set)
    print(train_col)
    remove(train_set)
    
    #     return(train_col)
    
    # read test
    test_set = read_data('test',limit)
    print("Read test set")
    test_col = list_all_level_read_col(col_list, test_set)
    print(test_col)
    remove(test_set)
    
    # merge level list
    all_level = list()
    for (col_name in col_list) {
        print (paste("Merge", col_name))
        
        l = c(train_col[[col_name]], test_col[[col_name]])
        l = unique(l)
        
        # insert
        all_level[[col_name]] = l
    }
    
    # save to file
    all_level
}

list_all_level_read_col <- function(col_list, data) {
    l = list()
    for (col_name in col_list) {
        # print(col_name)
        
        col_data = data[[col_name]]
        col_data = unique(col_data) # only unique
        
        #print (length(data[[col_name]])) # count data
        print( c(col_name , length(col_data)) )
        
        # save to list (hash)
        l[[col_name]] = col_data
        
    }
    
    l
}

clean_data <- function (data) {
    # read data
    
    col_list = c(
        "C1",
        "banner_pos",
        "site_id",
        "site_domain",
        "site_category",
        "app_id",
        "app_domain",
        "app_category",
        "device_id",
        "device_ip",
        "device_model",
        "device_type",
        "device_conn_type",
        "C14",
        "C15",
        "C16",
        "C17",
        "C18",
        "C19",
        "C20",
        "C21"
    )
    
    # remove low frequency
    # freq < 100 = rare
    # freq < 1,000 = low
    for (col_name in col_list) {
        print( paste("Clean low fequency" , col_name) )
        
        agg = aggregate(data[[col_name]], list(data[[col_name]]), length)
        names(agg) = c("col", "count")

        # select only column : agg[agg$count <= 100,  "col"]    
        rare_col = agg[agg$count < 10,  "col"]
        low_col  = agg[(agg$count < 1000 & agg$count >= 10) ,  "col"]
        
        cat("-- rare ")
        print(length(rare_col))
        cat("-- low ")
        print(length(low_col))
        
        
#         dtQuery[,newClose:=c(101,401)]
#         dtData[dtQuery,Close:=newClose]
#         return(low_col)
        
        # replace RARE
#         if(length(rare_col)) {
# #             data[data[[col_name]] == rare_col, col_name] = "RARE"
#             for(c_val in rare_col) {
#                 data[data[[col_name]] == c_val, col_name] = "RARE"
#             }
#         }
#         
#         # replace LOW
#         if(length(low_col)) {
#             for(c_val in low_col) {
#                 data[data[[col_name]] == c_val, col_name] = "LOW"
#             }
#         }
    }
    
    # rewrite to file
#     saveRDS(data, file = 'rdata/clean.RData',compress = F)
    return(NULL)
}

replace_agg_rare <- function(agg, p_rare=1) {
    
    rare_index = which(agg$summary[,"count"] <= p_rare)
    if (length(rare_index) == 0) {
        print("---no RARE!---")
        return(agg)
    }
    
    cat("---rare_index----")
    print(length(rare_index))
    
    agg_rare  = agg[rare_index,]
    agg_normal = agg[-rare_index,]
    
    # copy 1 row of agg because it's hard to recreate the structure
    rare_row = subset(agg[1,],TRUE)
    
    # change name to RARE
    rare_row[1,1] = "RARE"
    # cal average and sum %
    rare_row$summary[,"rate"]  =sum(agg_rare$summary[,"rate"])
    rare_row$summary[,"count"] =sum(agg_rare$summary[,"count"])
    
    rbind(agg_normal, rare_row )
}

check_source <- function(data) {
    # data is web or app
    app_index = which(data$site_domain == "c4e18dd6")
    if (length(app_index) > 0) {
        data_source = "app"
    } else {
        data_source = "web"
    }
    
    data_source
}

save_check_source_index <- function(data) {
    if ( "click" %in% colnames(data)) {
        data_part = "train"
    } else {
        data_part = "test"
    }
    
    app_index = which(data$site_domain == "c4e18dd6")
    
    # save app index
    file_name = paste(data_part, "app", "index", sep="_")
    file_name = paste(file_name, "RData", sep=".")
    file_name = paste("clean", file_name, sep="/")

    cat("Save index ")
    cat(data_part)
    print(file_name)
    
    saveRDS(app_index, file_name,compress=F)
    app_index
}

read_clean_data <- function(col_list=NULL, file_name="train", data_source="web") {
    # col_train_web_banner_pos.RData
    
    # test list
    if(is.null(col_list)) {
        col_list = c(
            "banner_pos",
            "traffic",
            "C1",
            "device_type",
            "site_category",
            "click"
        )
    }
    
    data = NULL
    for(col_name in col_list) {
        data_path = paste("col",file_name,data_source,col_name,sep="_")
        data_path = paste(data_path, "RData", sep=".")
        data_path = paste("clean", data_path, sep="/")
        
        cat("Read ")
        print(data_path)
        
        # read from file
        d = readRDS(data_path)
        print(length(d))
#         return(d)
        # merge
        if(is.null(data)) {
            data = d
        } else {
            data = cbind(data,d)
        }
    }
    data = as.data.table(data)
    setnames(data, col_list)
    return(data)
}
test <- function() {
    data = read_clean_data(data_source="web")
    
    inTrain = createDataPartition(y=data$click, p=0.1, list=FALSE)
    saveRDS(inTrain, "tmp/web_inTrain.RData")
    
    training = data[inTrain[,1],] # first column is row index
#     testing  = data[-inTrain[,1],]
    
    data_model <- dummyVars(click ~ banner_pos ,
                        data = training, sep = ".", levelsOnly = TRUE)
    data_model <- dummyVars(click ~ banner_pos + C1+device_type+site_category ,
                        data = training, sep = ".", levelsOnly = TRUE)
    saveRDS(data_model, "tmp/web_data_model.RData")

    data_dummy = predict(data_model, training)

    
    # should read traffic , click separatly because it dont need process
    c_names = colnames(data_dummy)
    data_dummy = cbind(data_dummy, as.numeric(training$traffic))
    colnames(data_dummy) <- c(c_names, "traffic")
    
    data_click = as.numeric(training$click)
    remove(data)
    
#     modelFit <- train(x=data_dummy,y=data_click, method="glm")
#     system.time(modelFit <- train(x=training,y=data_click, method="glm"))
    system.time(modelFit <- train(x=data_dummy,y=data_click, method="glm"))
    # web: elapsed 12201.340
    
    #---
    data_model = readRDS("tmp/app_data_model.RData")
    t = head(testing,100)
    data_dummy = predict(data_model, t)
    c_names = colnames(data_dummy)
    data_dummy = cbind(data_dummy, as.numeric(t$traffic))
    colnames(data_dummy) <- c(c_names, "traffic")
    
    modelFit = readRDS("rdata/app_model_clean_glm.RData")
    
    
    # fix missing RARE
    c_names = colnames(data_dummy)
    data_dummy = cbind( data_dummy,rep(0,nrow(data_dummy)))
    colnames(data_dummy) <- c(c_names, "banner_posRARE")
    
    predictions = predict(modelFit, newdata=data_dummy)

    cbind(predictions, t$click)

    #---
    #banner_pos traffic   C1 device_type site_category click
    t = rbind(t, 
        data.frame(
            "banner_pos" = 0,
            "traffic" = 0 ,
            "C1" = 0,
            "device_type" = 0,
            "site_category" = 0 ,
            "click" =0
        )
    )
}
