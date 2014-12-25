# USAGE
# source("Click_through.R")
# train_set = read_data('train',1000)
# test_set = read_data('test')
# validate_count_unique(train_set)
# test_without_train(train_set, test_set)

# Test Data
# d = head(train_set, 100)
# t = interpret_data(d)

# Train
# k = caret_train(train_set)

require(data.table)
library(caret)

# Model list http://topepo.github.io/caret/modelList.html
caret_train <- function(data, data_model="simple", caret_model="glm", model_seed=32343) {
    set.seed(model_seed)
    
    # map field to model
    data = interpret_data(data, data_model, "train")
    
    inTrain = createDataPartition(y=data$click, p=0.75, list=FALSE)
    
    training = data[inTrain[,1],] # first column is row index
    testing  = data[-inTrain[,1],]
    
    return (training)
    
    cat ("Train data dimension ") 
    print (dim(training))
    
    cat ("Test data dimension ") 
    print (dim(testing))
    
    drops = c("click") # separate test result column
#     x = data[,!(names(data) %in% drops), with=FALSE]
    
#     modelFit <- train(
#                 click ~.,
#                 data=training, 
#                 method = "nnet",
#                 preProcess = "range", 
# #                 tuneLength = 2,
#                 trace = FALSE,
#                 maxit = 100
#             )
    
    modelFit <- train(
                click ~.,
#                 data= as.data.frame.matrix(training), 
                data=training, 
#                 preProcess = "range", 
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
        "banner_pos",
        "site_category",
        "app_category",
        "device_type",
        "device_conn_type"
        
#         "C1",
#         "banner_pos",
#         "site_id",
#         "site_domain",
#         "site_category",
#         "app_id",
#         "app_domain",
#         "app_category",
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
    
    data[, col_list, with=FALSE]


# data[data == "0"] = 1
# training[training == "0"] = 1

}

interpret_data <- function( data, model="simple", type="train") {
    print ( c("Model", model), quote=FALSE )
    if (model == "simple") {
        data = int_model_simple(data, type)
    } else {
        return (NULL)
    }
    
    data
}

read_data <- function(file_name, limit=0) {
    # read data by type
    file_path = paste("Raw", file_name, sep = "/")
    
    if (limit) {
        data = fread(file_path, nrows=limit, colClasses="character")
    } else {
        data = fread(file_path, colClasses="character")
    }
    
    # change click to %
    # convert hour to timestamp
    data$click = as.numeric(data$click) * 100
    data$hour = as.numeric(data$hour)
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


