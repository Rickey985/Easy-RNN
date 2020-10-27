EZ_RNN_REG_Train=function(train_data,y_col,step_to_go_back=5,units_in_first=600,
                          units_in_second=600,drop_data=0.25,iterations=150,mse_vs_mae=c('mse','mae'),
                          optimizer=c('adam','adadelta','sgd','rmsprop','adagrad','adamax','nadam')){
  print("Here's a quick guide:")
  print("data=you input the data here, make sure that besides the ID, and Date
        The other columns have already been aggregated on a per Date basis.")
  print("y_col is the y column you aretrying to solve for")
  print("Step_to_go_back is how many dates back you want the RNN to study")
  print("Remember: No ID column, and This is only meant for a top down view")
  print("I'd recomment going with the rmsprop/adam optimizer, as that's the one usually recommended for time series")
  set.seed(123456)
  require(keras)
  require(tensorflow)
  port=as.data.table(train_data)
  setorder(port,Date)
  step=step_to_go_back
  port=unique(port,by=c('Date'))
  
  #Split output from inputs, kick out date, and ID. then run TimeseriesGenerator from keras, batch_size=1,
  #   and length=step_size
  setorder(port,Date)
  Ys=port[Date==123456789,c(y_col),with=F][,`:=`(ID=NULL,Date=NULL)]
  Xs=port[Date==123456789,!c(y_col),with=F][,`:=`(ID=NULL,Date=NULL)]
  port=port[,`:=`(ID=NULL,Date=NULL)]
  
  x=Xs
  y=Ys
  sample=length(unique(train_data$Date))-step-1
  for(l in seq(1,sample)){
    first_in=port[l+seq(0,step),!c(y_col),with=F]
    first_out=port[l+c(step),c(y_col),with=F]
    Xs=rbind(Xs,first_in)
    Ys=rbind(Ys,first_out)
  }
  
  x=Xs
  n_col=ncol(x)
  x=as.matrix(x)
  x[is.infinite(x) | is.na(x) | is.nan(x)]=0
  #Ys[,RNN_CLASS:=0][Cor_Mom<0,RNN_CLASS:=1] #Class
  y=as.matrix(Ys)
  #y=to_categorical(Ys$RNN_CLASS,num_classes = 2) #Class
  
  #Time to re-arrange X and Y appropriately (to array)
  X=array(x,dim=c(sample,step,n_col))
  Y=array(y,dim=c(sample,1)) #Class
  
  set.seed(123456)
  tensorflow::use_session_with_seed(seed=123456)
  EZ_RNN<<-keras_model_sequential() %>%
    bidirectional(layer_lstm(units=units_in_first,return_sequences = T),input_shape=c(step,n_col)) %>%  #Change units only
    layer_dropout(drop_data) %>% #Change the number to whatever
    bidirectional(layer_lstm(units=units_in_second,return_sequences = F)) %>%  #Change units only
    layer_dropout(drop_data) %>% #Change the number to whatever
    layer_dense(units=1) #Don't touch this
  EZ_RNN %>% compile(loss = mse_vs_mae[1],optimizer = optimizer[1]) #Change optimizer & loss, to whatever is allowed on keras,
  filepath="weights.best.hdf5"
  dank_back=callback_model_checkpoint(filepath,monitor='val_loss',verbose=1,save_best_only = T,save_weights_only = T,mode='min',period=3)
  EZ_RNN  %>% fit(X,Y,epochs=iterations,validation_split=0.20,verbose=1,callbacks=list(dank_back))
  EZ_RNN %>% load_model_weights_hdf5(filepath = filepath)
  preds=EZ_RNN %>% predict(X)
  
}

EZ_RNN_REG_Test=function(all_data,y_col,step_to_go_back,test_data_start_date,test_data_end_date){
  print('Make sure this step_to_go_back is the same as the one in the Train \n')
  print('Make sure all_data has the exact same columns as the train_data did, the order of the column does matter')
  set.seed(123456)
  require(keras)
  require(tensorflow)
  test_data_start_date=as.Date(test_data_start_date)
  test_data_end_date=as.Date(test_data_end_date)
  all_d=copy(all_data)
  highest_date=as.Date(test_data_end_date)
  all_d=all_d[Date<=as.Date(highest_date)]
  port=as.data.table(all_d)
  setorder(port,Date)
  step_size=step_to_go_back
  
  test_set=all_d[Date>=as.Date(test_data_start_date) & Date<=as.Date(test_data_end_date)]
  
  #Data prep for RNN
  port=unique(port,by=c('Date'))
  setorder(port,Date)
  port[,Num:=1]
  port[,Posn:=cumsum(Num)]
  starting_OS_date=as.Date(min(test_set$Date))
  port[Date==starting_OS_date,Position:=Posn]
  #Sending the last out one as the y sample, AKA, what we want to predict on
  #Oldie
  position=mean(port$Position,na.rm = T)-step_size-1
  sample=length(unique(test_set$Date))
  setorder(port,Date)
  Xs=port[Date==123456789,!c(y_col),with=F][,`:=`(ID=NULL,Date=NULL,Position=NULL,Num=NULL,Posn=NULL)]
  port=port[,`:=`(ID=NULL,Date=NULL,Position=NULL,Num=NULL,Posn=NULL)][,!c(y_col),with=F]
  portie_two<<-port
  print(paste('First Date in OS:',starting_OS_date))
  posit<<-position
  for(l in seq(1,sample)){
    first_in=port[l+seq(0,step_size)+position,]
    Xs=rbind(Xs,first_in)
  }
  x=Xs
  n_col=ncol(x)
  x=as.matrix(x)
  x[is.infinite(x) | is.na(x) | is.nan(x)]=0
  #Ys[,RNN_CLASS:=0][Cor_Mom<0,RNN_CLASS:=1] #Class
  #y=to_categorical(Ys$RNN_CLASS,num_classes = 2) #Class
  
  xyz<<-x
  #Time to re-arrange X and Y appropriately (to array)
  X=array(x,dim=c(sample,step_size,n_col))
  print('Starting RNN Predictions')
  set.seed(123456)
  RNN_Preds=EZ_RNN %>% predict(X) #Class
  #print(paste('Preds:',RNN_Preds))
  port_dates=unique(all_data,by=c('Date'))[Date>=as.Date(test_data_start_date) & Date<=as.Date(test_data_end_date)]
  port_dates[,RNN_Predictions:=RNN_Preds]
  port_dates=port_dates[,c('Date',y_col,'RNN_Predictions'),with=F]
}

EZ_RNN_REG=function(all_data,y_col,back_steps,test_data_start_date,test_data_end_date,
                    units_in_first=600,units_in_second=600,drop_data=0.25,iterations=150,mse_vs_mae=c('mse','mae'),
                    optimizer=c('adam','adadelta','sgd','rmsprop','adagrad','adamax','nadam')){
  training=all_data[Date<as.Date(test_data_start_date)]
  training_chunk=EZ_RNN_REG_Train(training,y_col,step_to_go_back = back_steps,units_in_first,units_in_second,
                                  drop_data,iterations,mse_vs_mae)
  testing_chunk=EZ_RNN_REG_Test(all_data,y_col,step_to_go_back = back_steps,test_data_start_date,test_data_end_date)
  print('FYI, you now have a new global variable called EZ_RNN')
  return(testing_chunk)
}

