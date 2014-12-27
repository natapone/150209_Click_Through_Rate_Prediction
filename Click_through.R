# USAGE
# source("Click_through.R")
# train_set = read_data('train',1000)
# test_set = read_data('test')

# Count
# validate_count_unique(train_set)
# test_without_train(train_set, test_set)
# all_level = list_all_level()

# Test Data
# d = head(train_set, 100)
# t = interpret_data(train_set)
# plot_aggregate_to_click(train_set)
# plot_aggregate_to_click_banner_C14(train_set)

# Train
# k = caret_train(train_set)

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
#         "banner_pos",
#         "site_category",
#         "app_category",
#         "device_type",
#         "device_conn_type"
        
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
    
    # Single
    for (col_name in col_list) {
        print ( c("Aggregate click -", col_name), quote=FALSE )
        
        agg = aggregate(train_set$click, list(train_set[[col_name]]),   
                    FUN=function(x) c(
                        "avg" =mean(x) * 100, # return %
                        "count"  =length(x) / 40428967 * 100 # total rows
                    ) # return as matrix
            )
#         return(agg)
        names(agg) = c(col_name, "summary")
        agg = agg[order(-agg$summary[,"avg"]),]

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

read_data <- function(file_name, limit=0) {
    
    # column data type ny file name
    if (file_name == 'train') {
        col_class_list = c(
            "NULL",
            "character",
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
    
    # change click to %
    # convert hour to timestamp
#     data$click = as.numeric(data$click) * 100
#     data$hour = as.numeric(data$hour)
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

