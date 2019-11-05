Grab_Factors=function(){
  #grab as much stuff here and the rets
  
  print('Starting Factor Builder')
  ###ests###
  #IBES ests
  print('IBES Ests')
  yrs_and_qs=c('Q1','FY1','FY2')
  for (i in yrs_and_qs){
    var_name=paste('raw_est_',i,sep='')
    assign(var_name,grab_estimate_data(issueids,SDATE,EDATE,simplify=FALSE,items = c('FFO','CPS','EPS','EBT',
                                                                                     'SAL','ROE','ROA','BPS'),keep_col='Mean',FY=i,fx_adj=FALSE))
  }
  
  
  ###other data###
  #Buy sell hold ranks
  ibes_ranks=grab_ibes_rnk(pds,SDATE,EDATE)[,c('ID','Date','BuyPct','HoldPct','SellPct')]
  ibes_ranks_est=improved_fn_JOIN_dt(pds=pds,freq = 'M',eds=list(ibes_ranks))
  names(ibes_ranks_est)=c('ID','Date','BuyPct','HoldPct','SellPct')
  setorder(ibes_ranks_est,Date,ID)
  if(freq=='W'){
    ibes_ranks_est[,`:=`(BuyPct_30_days=shift(BuyPct,4,typ='lag'),
                         HoldPct_30_days=shift(HoldPct,4,typ='lag'),
                         SellPct_30_days=shift(SellPct,4,typ='lag')),by=c('ID')]
    ibes_ranks_est[,`:=`(BuyPct_60_days=shift(BuyPct,8,typ='lag'),
                         HoldPct_60_days=shift(HoldPct,8,typ='lag'),
                         SellPct_60_days=shift(SellPct,8,typ='lag')),by=c('ID')]
    ibes_ranks_est[,`:=`(BuyPct_90_days=shift(BuyPct,12,typ='lag'),
                         HoldPct_90_days=shift(HoldPct,12,typ='lag'),
                         SellPct_90_days=shift(SellPct,12,typ='lag')),by=c('ID')]
  }
  else{
    ibes_ranks_est[,`:=`(BuyPct_30_days=shift(BuyPct,1,typ='lag'),
                         HoldPct_30_days=shift(HoldPct,1,typ='lag'),
                         SellPct_30_days=shift(SellPct,1,typ='lag')),by=c('ID')]
    ibes_ranks_est[,`:=`(BuyPct_60_days=shift(BuyPct,2,typ='lag'),
                         HoldPct_60_days=shift(HoldPct,2,typ='lag'),
                         SellPct_60_days=shift(SellPct,2,typ='lag')),by=c('ID')]
    ibes_ranks_est[,`:=`(BuyPct_90_days=shift(BuyPct,3,typ='lag'),
                         HoldPct_90_days=shift(HoldPct,3,typ='lag'),
                         SellPct_90_days=shift(SellPct,3,typ='lag')),by=c('ID')]
  }
  
  
  #Actuals
  ibess=c('EPS','SAL')
  all_acts_Q=grab_quarter_actuals(issueids,SDATE,EDATE,item=ibess)
  for (i in ibess){
    #Setting up Q1
    var_name=paste('Q_actual_',i,sep='')
    abc=all_acts_Q[[i]][,c('ID','Date','Local','SurpriseMean_Q1')]
    names(abc)=c('ID','Date','AcutalValue_Q1','SurpriseMean_Q1')
    assign(var_name,abc)
  }
  ibess=c('EPS','EBT','SAL','CPS','BPS','ROE','ROA','EBI','FFO','FCF')
  all_acts_Y=grab_FY_actuals(issueids,SDATE,EDATE,item=ibess)
  for (i in ibess){
    #Setting up FY1
    var_name=paste('FY_actual_',i,sep='')
    abc=all_acts_Y[[i]][,c('ID','Date','Local','SurpriseMean_FY1')]
    names(abc)=c('ID','Date','AcutalValue_FY1','SurpriseMean_FY1')
    assign(var_name,abc)
  }
  
  
  FY_actual_BPS[FY_actual_BPS<0]=NA
  print('Grabbed all the ests/acts')
  
  #TTM
  ibess=c('EPS','SAL')
  all_acts=grab_TTM(issueids,SDATE,EDATE,item=ibess)
  #NTM
  NTM_FFO=clean_NTM(raw_est_FY1$EST_FFO,raw_est_FY2$EST_FFO)
  NTM_CPS=clean_NTM(raw_est_FY1$EST_CPS,raw_est_FY2$EST_CPS)
  NTM_EPS=clean_NTM(raw_est_FY1$EST_EPS,raw_est_FY2$EST_EPS)
  NTM_EBT=clean_NTM(raw_est_FY1$EST_EBT,raw_est_FY2$EST_EBT)
  NTM_SAL=clean_NTM(raw_est_FY1$EST_SAL,raw_est_FY2$EST_SAL)
  NTM_ROE=clean_NTM(raw_est_FY1$EST_ROE,raw_est_FY2$EST_ROE)
  NTM_ROA=clean_NTM(raw_est_FY1$EST_ROA,raw_est_FY2$EST_ROA)
  
  ###Calculated Stuff###
  ##EV Prep##
  EV_raw_cash=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='CASH',annualize = F))[,c('ID','Date','CASH')]
  EV_raw_ST_Debt=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='ST_DEBT',annualize = F))[,!c('PerDate','fx','FQTR','ShrOut')]
  EV_raw_LT_Debt=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='LT_DEBT',annualize = F))[,!c('PerDate','fx','FQTR','ShrOut')]
  EV_raw_Total_Debt=fn_JOIN_dt(pds,SDATE,EDATE,freq=freq,8,fds=list(EV_raw_ST_Debt,EV_raw_LT_Debt),eds=list())[,Total_Debt:=ST_DEBT+LT_DEBT][,c('ID','Date','Total_Debt')]
  EV_raw_Minority_Interest=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='MINORITY_INTEREST',annualize = F))[,c('ID','Date','MINORITY_INTEREST')]
  EV_raw_Pref_Stock=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='PREFERRED_STOCK',annualize = F))[,c('ID','Date','PREFERRED_STOCK')]
  EV_consolidated=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(EV_raw_cash,EV_raw_Total_Debt,EV_raw_Minority_Interest,EV_raw_Pref_Stock), eds=list(), si=list())[,EV:=((LclPrice*Shares)+Total_Debt-CASH+MINORITY_INTEREST+PREFERRED_STOCK)][,c('ID','Date','EV')]
  cl_EV_consolidated=EV_consolidated[,EV:=EV/1000000]
  ##P/E, and other ratios##
  fwd1yr_P_per_E=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(), eds=list(raw_est_FY1$EST_EPS), si=list())[,c('ID','Date','LclPrice','estEST1')]
  fwd1yr_P_per_E[,Fwd1yr_P_perE:=LclPrice/(estEST1)][,':='(LclPrice=NULL,estEST1=NULL)]
  fwd2yr_P_per_E=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(), eds=list(raw_est_FY2$EST_EPS), si=list())[,c('ID','Date','LclPrice','estEST1')]
  fwd2yr_P_per_E[,Fwd2yr_P_perE:=LclPrice/(estEST1)][,':='(LclPrice=NULL,estEST1=NULL)]
  #Actual_fwd1yr_P_per_E=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(FY_actual_EPS), eds=list(), si=list())[,Price_per_acutal:=LclPrice/(AcutalValue_FY1)][,c('ID','Date','Price_per_acutal')]
  fwd1yr_P_per_CFPS=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(), eds=list(raw_est_FY1$EST_CPS), si=list())[,c('ID','Date','LclPrice','estEST1')]
  fwd1yr_P_per_CFPS[,Fwd1yr_P_perFFO:=LclPrice/(estEST1)][,':='(LclPrice=NULL,estEST1=NULL)]
  fwd2yr_P_per_CFPS=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(), eds=list(raw_est_FY2$EST_CPS), si=list())[,c('ID','Date','LclPrice','estEST1')]
  fwd2yr_P_per_CFPS[,Fwd2yr_P_perFFO:=LclPrice/(estEST1)][,':='(LclPrice=NULL,estEST1=NULL)]
  #Actual_fwd1yr_P_per_CF=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(FY_actual_CPS), eds=list(), si=list())[,Price_per_acutal:=LclPrice/(AcutalValue_FY1)][,c('ID','Date','Price_per_acutal')]
  fwd1yr_P_per_BPS=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(), eds=list(raw_est_FY1$EST_BPS), si=list())[,c('ID','Date','LclPrice','estEST1')]
  fwd1yr_P_per_BPS[,Fwd1yr_P_perBPS:=LclPrice/(estEST1)][,':='(LclPrice=NULL,estEST1=NULL)]
  fwd2yr_P_per_BPS=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(), eds=list(raw_est_FY2$EST_BPS), si=list())[,c('ID','Date','LclPrice','estEST1')]
  fwd2yr_P_per_BPS[,Fwd2yr_P_perBPS:=LclPrice/(estEST1)][,':='(LclPrice=NULL,estEST1=NULL)]
  #Actual_fwd1yr_P_per_B=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(FY_actual_BPS), eds=list(), si=list())[,Price_per_acutal:=LclPrice/(AcutalValue_FY1)][,c('ID','Date','Price_per_acutal')]
  EV_consolidated_EBITDA_est=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(EV_raw_cash,EV_raw_Total_Debt,EV_raw_Minority_Interest,EV_raw_Pref_Stock), eds=list(raw_est_FY1$EST_EBT), si=list())[,EV:=((Shares*LclPrice)+Total_Debt-CASH+MINORITY_INTEREST+PREFERRED_STOCK)]
  Fwd1yr_EV_per_EBITDA=EV_consolidated_EBITDA_est[,Fwd_EV_per_EBITDA:=(EV/estEST1)][,c('ID','Date','Fwd_EV_per_EBITDA')]
  EV_consolidated_EBITDA_est=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(EV_raw_cash,EV_raw_Total_Debt,EV_raw_Minority_Interest,EV_raw_Pref_Stock), eds=list(raw_est_FY2$EST_EBT), si=list())[,EV:=((Shares*LclPrice)+Total_Debt-CASH+MINORITY_INTEREST+PREFERRED_STOCK)]
  Fwd2yr_EV_per_EBITDA=EV_consolidated_EBITDA_est[,Fwd_EV_per_EBITDA:=(EV/estEST1)][,c('ID','Date','Fwd_EV_per_EBITDA')]
  #Actual_fwd1yr_EV_per_EBITDA=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(EV_raw_cash,EV_raw_Total_Debt,EV_raw_Minority_Interest,EV_raw_Pref_Stock,FY_actual_EBT), eds=list(), si=list())[,EV:=((Shares*LclPrice)+Total_Debt-CASH+MINORITY_INTEREST+PREFERRED_STOCK)][,EV_per_acutal:=EV/(AcutalValue_FY1)][,c('ID','Date','EV_per_acutal')]
  print('Finished Price/Earnings Stuff')
  ##Gross Profit##
  COGS=(improved_grab_fund_dt(issueids,SDATE,EDATE,item='COGS',incl_fq = T,annualize = F))[,c('ID','Date','COGS')]
  Rev=(improved_grab_fund_dt(issueids,SDATE,EDATE,item='SAL',incl_fq = T,annualize = F))[,c('ID','Date','FQTR','SAL')]
  actual_GP_Qs=merge(COGS,Rev,by=c('ID','Date'))[,Actual_GP:=(SAL-COGS)/SAL][,c('ID','Date','COGS','FQTR','SAL','Actual_GP')]
  
  #SAL COGS ASSET
  ASSET=improved_grab_fund_dt(issueids,SDATE,EDATE,item='ASSETS',incl_fq = T,annualize = F)[,c('ID','Date','ASSETS')]
  SAL_COGS_ASSET=improved_fn_JOIN_dt(pds,fds=list(Rev,COGS,ASSET),eds=list(),freq=freq)[,Ratio:=(SAL-COGS)/ASSETS][,c('ID','Date','Ratio')]
  ##Growth##
  EPS_consensus=improved_fn_JOIN_dt(pds,fds=list(FY_actual_EPS),eds=list(raw_est_FY1$EST_EPS),freq=freq)[,Consensus:=estEST1/AcutalValue_FY1][,c('ID','Date','Consensus')]
  EPS_Growth=improved_fn_JOIN_dt(pds,fds=list(),eds=list(raw_est_FY1$EST_EPS,raw_est_FY2$EST_EPS),freq=freq)[,Growth:=estEST2/estEST1][,c('ID','Date','Growth')]
  EPS_Q_consensus=improved_fn_JOIN_dt(pds,fds=list(Q_actual_EPS),eds=list(raw_est_Q1$EST_EPS),freq=freq)[,Consensus:=estEST1/AcutalValue_Q1][,c('ID','Date','Consensus')]
  EPS_FY_Q_growth=improved_fn_JOIN_dt(pds,fds=list(),eds=list(raw_est_FY1$EST_EPS,raw_est_Q1$EST_EPS),freq=freq)[,Growth:=estEST1/estEST2][,c('ID','Date','Growth')]
  print('Finished Weird Earnings Calcs')
  ##Momentum##
  Mo_12_1=grab_mom_fast_dt(pds[,c('ID','Date')],SDATE,EDATE,mo_length = 365,excl_length = 30)
  Mo_12_6=grab_mom_fast_dt(pds[,c('ID','Date')],SDATE,EDATE,mo_length = 365,excl_length = 182)
  Mo_24_12=grab_mom_fast_dt(pds[,c('ID','Date')],SDATE,EDATE,mo_length = 730,excl_length = 365)
  Mo_24_1=grab_mom_fast_dt(pds[,c('ID','Date')],SDATE,EDATE,mo_length = 730,excl_length = 30)
  ##Leverage##
  LT_DEBT=improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = F,item='LT_DEBT',annualize = F)[,c('ID','Date','LT_DEBT')]
  ST_DEBT=improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = F,item='ST_DEBT',annualize = F)[,c('ID','Date','ST_DEBT')]
  EQUITY=improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = F,item='EQUITY',annualize = F)[,c('ID','Date','EQUITY')]
  CASH=improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = F,item='CASH',annualize = F)[,c('ID','Date','CASH')]
  DEBT_TO_EQUITY=improved_fn_JOIN_dt(pds,fds=list(LT_DEBT,ST_DEBT,EQUITY),eds=list(),freq=freq)[,DEBT_TO_EQUITY_RATIO:=(LT_DEBT+ST_DEBT)/EQUITY]
  NET_DEBT=improved_fn_JOIN_dt(pds,fds=list(LT_DEBT,ST_DEBT,CASH),eds=list(),freq=freq)[,NET_DEBT:=LT_DEBT+ST_DEBT-CASH]
  ##Analyst Revisions factors##
  print('Starting Analyst Revisions Sentiment')
  #Qs#
  Sentiment_EPS_Q_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_EPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_Q_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_EBT))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_Q_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_SAL))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_Q_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_CPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_Q_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_FFO))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_Q_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_BPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_Q_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_ROE))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_Q_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_ROA))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EPS_Q_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_EPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_Q_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_EBT))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_Q_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_SAL))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_Q_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_CPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_Q_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_FFO))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_Q_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_BPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_Q_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_ROE))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_Q_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_ROA))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EPS_Q_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_EPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_Q_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_EBT))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_Q_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_SAL))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_Q_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_CPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_Q_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_FFO))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_Q_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_BPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_Q_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_ROE))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_Q_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_Q1$EST_ROA))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  #FY1s#
  Sentiment_EPS_FY1_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_EPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_FY1_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_EBT))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_FY1_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_SAL))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_FY1_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_CPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_FY1_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_FFO))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_FY1_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_BPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_FY1_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_ROE))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_FY1_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_ROA))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EPS_FY1_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_EPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_FY1_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_EBT))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_FY1_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_SAL))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_FY1_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_CPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_FY1_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_FFO))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_FY1_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_BPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_FY1_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_ROE))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_FY1_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_ROA))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EPS_FY1_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_EPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_FY1_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_EBT))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_FY1_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_SAL))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_FY1_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_CPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_FY1_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_FFO))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_FY1_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_BPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_FY1_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_ROE))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_FY1_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY1$EST_ROA))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  #FY2s#
  Sentiment_EPS_FY2_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_EPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_FY2_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_EBT))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_FY2_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_SAL))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_FY2_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_CPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_FY2_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_FFO))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_FY2_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_BPS))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_FY2_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_ROE))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_FY2_1M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_ROA))[,lagged:=shift(estEST1,4,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EPS_FY2_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_EPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_FY2_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_EBT))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_FY2_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_SAL))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_FY2_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_CPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_FY2_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_FFO))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_FY2_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_BPS))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_FY2_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_ROE))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_FY2_2M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_ROA))[,lagged:=shift(estEST1,8,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EPS_FY2_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_EPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_EBT_FY2_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_EBT))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_SAL_FY2_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_SAL))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_CPS_FY2_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_CPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_FFO_FY2_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_FFO))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_BPS_FY2_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_BPS))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROE_FY2_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_ROE))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  Sentiment_ROA_FY2_3M=fn_merge_list(pds,fds=list(),eds=list(raw_est_FY2$EST_ROA))[,lagged:=shift(estEST1,12,'lag'),by=c('ID')][,increase:=(estEST1-lagged)/max(c(0.1,abs(lagged)),na.rm = T),by=c('ID','Date')][,c('ID','Date','increase')]
  
  
  ###macro economic data###
  #macro_data=grab_macro_data(grab_type = c('Available_at_that_time'),sdate = SDATE,edate=EDATE)
  
  
  ###Starmine Model Data###
  starmine_data=grab_Starmine_dt(daily_pds,SDATE,EDATE,items=c(44,119))
  
  ###Additional Fundamental Ratios to add here###
  print('Grabbing Fundamental Ratios')
  #remember use the fds in everything
  #vdt
  vdt=improved_fn_JOIN_dt(pds=pds,fds=list(CASH,EQUITY,EV_raw_Total_Debt),eds=list(),freq=freq)
  vdt[,NOA:= -ifelse(is.na(CASH),0,CASH)+ifelse(is.na(EQUITY),0,EQUITY)+ifelse(is.na(Total_Debt),0,Total_Debt)]
  vdt[,ACCRUALS := (NOA - shift(NOA,12)) / ((NOA + shift(NOA,12))/2), by='ID']
  vdt=vdt[,c('ID','Date','NOA','ACCRUALS')]
  
  #vdt1
  CAPEX=improved_grab_fund_dt(issueids,SDATE,EDATE,item='CAPEX',incl_fq = F)[,c('ID','Date','CAPEX')]
  CPS=improved_grab_fund_dt(issueids,SDATE,EDATE,item='CPS',incl_fq = F)[,c('ID','Date','CPS')]
  MINORITY_INTEREST=improved_grab_fund_dt(issueids,SDATE,EDATE,item='MINORITY_INTEREST')[,c('ID','Date','MINORITY_INTEREST')]
  PREFERRED_STOCK=improved_grab_fund_dt(issueids,SDATE,EDATE,item='PREFERRED_STOCK')[,c('ID','Date','PREFERRED_STOCK')]
  EPS=improved_grab_fund_dt(issueids,SDATE,EDATE,item='EPS')[,c('ID','Date','EPS')]
  INTANGIBLES=improved_grab_fund_dt(issueids,SDATE,EDATE,item='INTANGIBLES')[,c('ID','Date','INTANGIBLES')]
  Price=pds[,c('ID','Date','Price')]
  names(Price)=c('ID','Date','Dat_Price')
  vdt1=improved_fn_JOIN_dt(pds=pds,fds=list(CPS,CAPEX,EV_raw_Total_Debt,Price,MINORITY_INTEREST,PREFERRED_STOCK,CASH,
                                            EPS,EQUITY,INTANGIBLES),eds=list(),freq=freq)
  vdt1=vdt1[,`:=`(FCF = CPS - ifelse(is.na(CAPEX),0,CAPEX),EVshr=Dat_Price+ifelse(is.na(Total_Debt),0,Total_Debt)-
               ifelse(is.na(CASH),0,CASH)+ifelse(is.na(MINORITY_INTEREST),0,MINORITY_INTEREST)+
               ifelse(is.na(PREFERRED_STOCK),0,PREFERRED_STOCK),ROE=EPS/EQUITY,
             BPS=EQUITY-ifelse(is.na(INTANGIBLES),0,INTANGIBLES),DEBT_EQUITY=Total_Debt/EQUITY,EY=EPS/Dat_Price)][,c('ID','Date','FCF','EVshr','BPS','DEBT_EQUITY','EY')]
  
  #vdt2
  LQA_Div=pds[,c('ID','Date','LQA_Div')]
  names(LQA_Div)=c('ID','Date','Dat_LQA_Div')
  EBITDA=improved_grab_fund_dt(issueids,SDATE,EDATE,item='EBT')[,c('ID','Date','EBT')]
  SAL=improved_grab_fund_dt(issueids,SDATE,EDATE,item='SAL')[,c('ID','Date','SAL')]
  RnD=improved_grab_fund_dt(issueids,SDATE,EDATE,item='RnD')[,c('ID','Date','RnD')]
  Depreciation=improved_grab_fund_dt(issueids,SDATE,EDATE,item='DEPRECIATION')[,c('ID','Date','DEPRECIATION')]
  vdt2=improved_fn_JOIN_dt(pds=pds,fds=list(CPS,CAPEX,EV_raw_Total_Debt,Price,MINORITY_INTEREST,PREFERRED_STOCK,CASH,
                                            EPS,EQUITY,INTANGIBLES,LQA_Div,EBITDA,SAL,ASSET,RnD,Depreciation),eds=list(NTM_EPS,NTM_CPS,NTM_EBT,NTM_SAL),freq=freq)
  vdt2[,`:=`(BPS=EQUITY-ifelse(is.na(INTANGIBLES),0,INTANGIBLES),FCF = CPS - ifelse(is.na(CAPEX),0,CAPEX),
             EVshr=Dat_Price+ifelse(is.na(Total_Debt),0,Total_Debt)-
               ifelse(is.na(CASH),0,CASH)+ifelse(is.na(MINORITY_INTEREST),0,MINORITY_INTEREST)+
               ifelse(is.na(PREFERRED_STOCK),0,PREFERRED_STOCK),ROE=EPS/EQUITY)]
  vdt2[,`:=`(BY=BPS/Dat_Price,CFY=CPS/Dat_Price,eEY=estEST1/Dat_Price,eCFY=estEST2/Dat_Price,DY=Dat_LQA_Div/Dat_Price,FCFY=FCF/EVshr,
             EBITDAy=EBT/EVshr,eEBITDAy=estEST3/EVshr,SALY=SAL/EVshr,eSALY=estEST4/EVshr,FCF_Margin=FCF/SAL,NI_Margin=EPS/SAL,eNI_Margin=estEST1/estEST4,
             CASH_MC=CASH/Dat_Price,Asset_Turn=SAL/ASSETS,RnD_Sales=RnD/SAL,CAPEX_FCF=CAPEX/FCF,CAPEX_Dep=CAPEX/DEPRECIATION,
             SAL_eGrowth=estEST4/SAL,EPS_eGrowth=estEST1-EPS,TBY=BPS/Dat_Price,eROE=estEST1/EPS*ROE,eND_EBT=(Total_Debt-CASH)/estEST3)]
  vdt2[,TBY:=ifelse(TBY<=0,NA,TBY)]
  vdt2=vdt2[,c('ID','Date','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY','eSALY','FCF_Margin','NI_Margin',
               'CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','SAL_eGrowth','EPS_eGrowth','TBY','eROE','eND_EBT')]
  
  
  
  
  
  ##Putting it all together###
  print('Now putting it all together')
  all_data=improved_fn_JOIN_dt(pds,freq=freq,eds=list(Fwd1yr_EV_per_EBITDA,
                                                      #Fwd1yr_GICS_Ind_EV_per_EBITDA,Fwd1yr_GICS_Ind_P_per_BPS,Fwd1yr_GICS_Ind_P_per_CFPS,
                                                      #Fwd1yr_GICS_Ind_P_per_EPS,Fwd1yr_GICS_Sec_EV_per_EBITDA,Fwd1yr_GICS_Sec_P_per_BPS,
                                                      #Fwd1yr_GICS_Sec_P_per_CFPS,Fwd1yr_GICS_Sec_P_per_EPS,Fwd1yr_MRKT_EV_per_EBITDA,
                                                      #Fwd1yr_MRKT_P_per_BPS,Fwd1yr_MRKT_P_per_CFPS,Fwd1yr_MRKT_P_per_EPS,
                                                      fwd1yr_P_per_BPS,fwd1yr_P_per_CFPS,fwd1yr_P_per_E,Fwd2yr_EV_per_EBITDA,
                                                      #Fwd2yr_GICS_Ind_EV_per_EBITDA,
                                                      #Fwd2yr_GICS_Ind_P_per_BPS,Fwd2yr_GICS_Ind_P_per_CFPS,Fwd2yr_GICS_Ind_P_per_EPS,
                                                      #Fwd2yr_GICS_Sec_EV_per_EBITDA,Fwd2yr_GICS_Sec_P_per_BPS,Fwd2yr_GICS_Sec_P_per_CFPS,
                                                      #Fwd2yr_GICS_Sec_P_per_EPS,Fwd2yr_MRKT_EV_per_EBITDA,Fwd2yr_MRKT_P_per_BPS,Fwd2yr_MRKT_P_per_CFPS,
                                                      #Fwd2yr_MRKT_P_per_EPS,
                                                      fwd2yr_P_per_BPS,fwd2yr_P_per_CFPS,fwd2yr_P_per_E,
                                                      #NTM_CPS,NTM_EBT,
                                                      #NTM_EPS,
                                                      #NTM_EV_per_EBITDA,
                                                      #NTM_FFO,NTM_GICS_Ind_EV_per_EBITDA,
                                                      #NTM_GICS_Ind_EV_per_EBITDA,
                                                      #NTM_GICS_Ind_P_per_BPS,NTM_GICS_Ind_P_per_CFPS,NTM_GICS_Ind_P_per_EPS,NTM_GICS_Sec_EV_per_EBITDA,
                                                      #NTM_GICS_Sec_P_per_BPS,NTM_GICS_Sec_P_per_CFPS,NTM_GICS_Sec_P_per_EPS,NTM_MRKT_EV_per_EBITDA,
                                                      #NTM_MRKT_P_per_BPS,NTM_MRKT_P_per_CFPS,NTM_MRKT_P_per_EPS,NTM_P_per_BPS,NTM_P_per_CFPS,NTM_P_per_E,
                                                      #NTM_ROA,NTM_ROE,NTM_SAL,
                                                      Sentiment_BPS_FY1_1M,Sentiment_BPS_FY1_2M,Sentiment_BPS_FY1_3M,  #12
                                                      Sentiment_BPS_FY2_1M,Sentiment_BPS_FY2_2M,Sentiment_BPS_FY2_3M,Sentiment_BPS_Q_1M,Sentiment_BPS_Q_2M,
                                                      Sentiment_BPS_Q_3M,Sentiment_CPS_FY1_1M,Sentiment_CPS_FY1_2M,Sentiment_CPS_FY1_3M,Sentiment_CPS_FY2_1M, #22
                                                      Sentiment_CPS_FY2_2M,Sentiment_CPS_FY2_3M,Sentiment_CPS_Q_1M,Sentiment_CPS_Q_2M,Sentiment_CPS_Q_3M,
                                                      Sentiment_EBT_FY1_1M,Sentiment_EBT_FY1_2M,Sentiment_EBT_FY1_3M,Sentiment_EBT_FY2_1M,Sentiment_EBT_FY1_3M, #32
                                                      Sentiment_EBT_FY2_1M,Sentiment_EBT_FY2_2M,Sentiment_EBT_FY2_3M,Sentiment_EBT_Q_1M,Sentiment_EBT_Q_2M,
                                                      Sentiment_EBT_Q_3M,Sentiment_EPS_FY1_1M,Sentiment_EPS_FY1_2M,Sentiment_EPS_FY1_3M,Sentiment_EPS_FY2_1M, #42
                                                      Sentiment_EPS_FY2_2M,Sentiment_EPS_FY2_3M,Sentiment_EPS_Q_1M,Sentiment_EPS_Q_2M,Sentiment_EPS_Q_3M,
                                                      Sentiment_FFO_FY1_1M,Sentiment_FFO_FY1_2M,Sentiment_FFO_FY1_3M,Sentiment_FFO_FY2_1M,Sentiment_FFO_FY2_2M, #52
                                                      Sentiment_FFO_FY2_3M,Sentiment_FFO_Q_1M,Sentiment_FFO_Q_2M,Sentiment_FFO_Q_3M,Sentiment_ROA_FY1_1M,
                                                      Sentiment_FFO_FY1_2M,Sentiment_FFO_FY1_3M,Sentiment_FFO_FY2_1M,Sentiment_ROA_FY1_1M,Sentiment_ROA_FY1_2M, #62
                                                      Sentiment_ROA_FY1_3M,Sentiment_ROA_FY2_1M,Sentiment_ROA_FY2_2M,Sentiment_ROA_FY2_3M,Sentiment_ROA_Q_1M,
                                                      Sentiment_ROA_Q_2M,Sentiment_ROA_Q_3M,Sentiment_ROE_FY1_1M,Sentiment_ROE_FY1_2M,Sentiment_ROE_FY1_3M, #72
                                                      Sentiment_ROE_FY2_1M,Sentiment_ROE_FY2_2M,Sentiment_ROE_FY2_3M,Sentiment_ROE_Q_1M,Sentiment_ROE_Q_2M,
                                                      Sentiment_ROE_Q_3M,Sentiment_SAL_FY1_1M,Sentiment_SAL_FY1_2M,Sentiment_SAL_FY1_3M,Sentiment_SAL_FY2_1M,
                                                      Sentiment_SAL_FY2_2M,Sentiment_SAL_FY2_3M,Sentiment_SAL_Q_1M,Sentiment_SAL_Q_2M,Sentiment_SAL_Q_3M),
                               
                               fds=list(#Actual_fwd1yr_EV_per_EBITDA,Actual_fwd1yr_P_per_B,Actual_fwd1yr_P_per_CF,Actual_fwd1yr_P_per_E,
                                        #ASSET,CASH,COGS,
                                        DEBT_TO_EQUITY,EPS_consensus,EPS_FY_Q_growth,EPS_Growth,EPS_Q_consensus,
                                        #EQUITY,EV_raw_cash,EV_raw_Minority_Interest,EV_raw_Pref_Stock,FY_actual_BPS,
                                        #FY_actual_CPS,FY_actual_EBI,FY_actual_EBT,FY_actual_EPS,FY_actual_ROA,FY_actual_ROE,
                                        #FY_actual_SAL,
                                        #FY_GICS_Ind_EV_per_EBITDA,FY_GICS_Ind_P_per_BPS,FY_GICS_Ind_P_per_CFPS,
                                        #FY_GICS_Ind_P_per_EPS,FY_GICS_Sec_EV_per_EBITDA,FY_GICS_Sec_P_per_BPS,FY_GICS_Sec_P_per_CFPS,
                                        #FY_GICS_Sec_P_per_EPS,FY_MRKT_EV_per_EBITDA,FY_MRKT_P_per_BPS,FY_MRKT_P_per_CFPS,FY_MRKT_P_per_EPS,
                                        ibes_ranks_est,Mo_12_1,Mo_12_6,Mo_24_1,Mo_24_12,
                                        #Q_actual_BPS,Q_actual_CPS,Q_actual_EBI,Q_actual_EBT,
                                        #Q_actual_ROA,Q_actual_ROE,Q_actual_SAL,
                                        SAL_COGS_ASSET,
                                        #TTM_EV_per_EBITDA,
                                        #TTM_GICS_Ind_EV_per_EBITDA,
                                        #TTM_GICS_Ind_P_per_BPS,TTM_GICS_Ind_P_per_CFPS,TTM_GICS_Ind_P_per_EPS,TTM_GICS_Sec_EV_per_EBITDA,
                                        #TTM_GICS_Sec_P_per_BPS,TTM_GICS_Sec_P_per_CFPS,TTM_GICS_Sec_P_per_EPS,TTM_MRKT_EV_per_EBITDA,
                                        #TTM_MRKT_P_per_BPS,TTM_MRKT_P_per_CFPS,TTM_MRKT_P_per_EPS,
                                        #TTM_P_per_BPS,TTM_P_per_CFPS,
                                        #TTM_P_per_E,
                                        starmine_data,vdt,vdt1,vdt2))
  
  print('Finished Factor Builder')
  return(all_data)
}

Daily_Grab_Factors=function(){
  #grab as much stuff here and the rets
  
  print('Starting Factor Builder')
  ###ests###
  #IBES ests
  print('IBES Ests')
  yrs_and_qs=c('Q1','FY1','FY2')
  for (i in yrs_and_qs){
    var_name=paste('raw_est_',i,sep='')
    assign(var_name,grab_estimate_data(issueids,SDATE,EDATE,simplify=FALSE,items = c('FFO','CPS','EPS','EBT',
                                                                                     'SAL','ROE','ROA','BPS'),keep_col='Mean',FY=i,fx_adj=FALSE))
  }
  
  #Actuals
  ibess=c('EPS','SAL')
  all_acts_Q=grab_quarter_actuals(issueids,SDATE,EDATE,item=ibess)
  for (i in ibess){
    #Setting up Q1
    var_name=paste('Q_actual_',i,sep='')
    abc=all_acts_Q[[i]][,c('ID','Date','Local','SurpriseMean_Q1')]
    names(abc)=c('ID','Date','AcutalValue_Q1','SurpriseMean_Q1')
    assign(var_name,abc)
  }
  ibess=c('EPS','EBT','SAL','CPS','BPS','ROE','ROA','EBI','FFO','FCF')
  all_acts_Y=grab_FY_actuals(issueids,SDATE,EDATE,item=ibess)
  for (i in ibess){
    #Setting up FY1
    var_name=paste('FY_actual_',i,sep='')
    abc=all_acts_Y[[i]][,c('ID','Date','Local','SurpriseMean_FY1')]
    names(abc)=c('ID','Date','AcutalValue_FY1','SurpriseMean_FY1')
    assign(var_name,abc)
  }
  
  
  FY_actual_BPS[FY_actual_BPS<0]=NA
  print('Grabbed all the ests/acts')
  
  #TTM
  ibess=c('EPS','SAL')
  all_acts=grab_TTM(issueids,SDATE,EDATE,item=ibess)
  #NTM
  NTM_FFO=clean_NTM(raw_est_FY1$EST_FFO,raw_est_FY2$EST_FFO)
  NTM_CPS=clean_NTM(raw_est_FY1$EST_CPS,raw_est_FY2$EST_CPS)
  NTM_EPS=clean_NTM(raw_est_FY1$EST_EPS,raw_est_FY2$EST_EPS)
  NTM_EBT=clean_NTM(raw_est_FY1$EST_EBT,raw_est_FY2$EST_EBT)
  NTM_SAL=clean_NTM(raw_est_FY1$EST_SAL,raw_est_FY2$EST_SAL)
  NTM_ROE=clean_NTM(raw_est_FY1$EST_ROE,raw_est_FY2$EST_ROE)
  NTM_ROA=clean_NTM(raw_est_FY1$EST_ROA,raw_est_FY2$EST_ROA)
  
  ###Calculated Stuff###
  ##EV Prep##
  EV_raw_cash=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='CASH',annualize = F))[,c('ID','Date','CASH')]
  EV_raw_ST_Debt=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='ST_DEBT',annualize = F))[,!c('PerDate','fx','FQTR','ShrOut')]
  EV_raw_LT_Debt=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='LT_DEBT',annualize = F))[,!c('PerDate','fx','FQTR','ShrOut')]
  EV_raw_Total_Debt=fn_JOIN_dt(pds,SDATE,EDATE,freq=freq,8,fds=list(EV_raw_ST_Debt,EV_raw_LT_Debt),eds=list())[,Total_Debt:=ST_DEBT+LT_DEBT][,c('ID','Date','Total_Debt')]
  EV_raw_Minority_Interest=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='MINORITY_INTEREST',annualize = F))[,c('ID','Date','MINORITY_INTEREST')]
  EV_raw_Pref_Stock=(improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = T,item='PREFERRED_STOCK',annualize = F))[,c('ID','Date','PREFERRED_STOCK')]
  EV_consolidated=fn_JOIN_dt(pds, SDATE, EDATE, freq, 8, fds=list(EV_raw_cash,EV_raw_Total_Debt,EV_raw_Minority_Interest,EV_raw_Pref_Stock), eds=list(), si=list())[,EV:=((LclPrice*Shares)+Total_Debt-CASH+MINORITY_INTEREST+PREFERRED_STOCK)][,c('ID','Date','EV')]
  cl_EV_consolidated=EV_consolidated[,EV:=EV/1000000]
  ##Gross Profit##
  COGS=(improved_grab_fund_dt(issueids,SDATE,EDATE,item='COGS',incl_fq = T,annualize = F))[,c('ID','Date','COGS')]
  Rev=(improved_grab_fund_dt(issueids,SDATE,EDATE,item='SAL',incl_fq = T,annualize = F))[,c('ID','Date','FQTR','SAL')]
  actual_GP_Qs=merge(COGS,Rev,by=c('ID','Date'))[,Actual_GP:=(SAL-COGS)/SAL][,c('ID','Date','COGS','FQTR','SAL','Actual_GP')]
  
  #SAL COGS ASSET
  ASSET=improved_grab_fund_dt(issueids,SDATE,EDATE,item='ASSETS',incl_fq = T,annualize = F)[,c('ID','Date','ASSETS')]
  SAL_COGS_ASSET=improved_fn_JOIN_dt(pds,fds=list(Rev,COGS,ASSET),eds=list(),freq=freq)[,Ratio:=(SAL-COGS)/ASSETS][,c('ID','Date','Ratio')]
  ##Growth##
  EPS_consensus=improved_fn_JOIN_dt(pds,fds=list(FY_actual_EPS),eds=list(raw_est_FY1$EST_EPS),freq=freq)[,Consensus:=estEST1/AcutalValue_FY1][,c('ID','Date','Consensus')]
  EPS_Growth=improved_fn_JOIN_dt(pds,fds=list(),eds=list(raw_est_FY1$EST_EPS,raw_est_FY2$EST_EPS),freq=freq)[,Growth:=estEST2/estEST1][,c('ID','Date','Growth')]
  EPS_Q_consensus=improved_fn_JOIN_dt(pds,fds=list(Q_actual_EPS),eds=list(raw_est_Q1$EST_EPS),freq=freq)[,Consensus:=estEST1/AcutalValue_Q1][,c('ID','Date','Consensus')]
  EPS_FY_Q_growth=improved_fn_JOIN_dt(pds,fds=list(),eds=list(raw_est_FY1$EST_EPS,raw_est_Q1$EST_EPS),freq=freq)[,Growth:=estEST1/estEST2][,c('ID','Date','Growth')]
  print('Finished Weird Earnings Calcs')
  ##Momentum##
  Mo_12_1=grab_mom_fast_dt(pds[,c('ID','Date')],SDATE,EDATE,mo_length = 365,excl_length = 30)
  Mo_12_6=grab_mom_fast_dt(pds[,c('ID','Date')],SDATE,EDATE,mo_length = 365,excl_length = 182)
  Mo_24_12=grab_mom_fast_dt(pds[,c('ID','Date')],SDATE,EDATE,mo_length = 730,excl_length = 365)
  Mo_24_1=grab_mom_fast_dt(pds[,c('ID','Date')],SDATE,EDATE,mo_length = 730,excl_length = 30)
  ##Leverage##
  LT_DEBT=improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = F,item='LT_DEBT',annualize = F)[,c('ID','Date','LT_DEBT')]
  ST_DEBT=improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = F,item='ST_DEBT',annualize = F)[,c('ID','Date','ST_DEBT')]
  EQUITY=improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = F,item='EQUITY',annualize = F)[,c('ID','Date','EQUITY')]
  CASH=improved_grab_fund_dt(issueids,SDATE,EDATE,incl_fq = F,item='CASH',annualize = F)[,c('ID','Date','CASH')]
  DEBT_TO_EQUITY=improved_fn_JOIN_dt(pds,fds=list(LT_DEBT,ST_DEBT,EQUITY),eds=list(),freq=freq)[,DEBT_TO_EQUITY_RATIO:=(LT_DEBT+ST_DEBT)/EQUITY]
  NET_DEBT=improved_fn_JOIN_dt(pds,fds=list(LT_DEBT,ST_DEBT,CASH),eds=list(),freq=freq)[,NET_DEBT:=LT_DEBT+ST_DEBT-CASH]
  ##Analyst Revisions factors##
  print('Starting Analyst Revisions Sentiment')
  ###macro economic data###
  #macro_data=grab_macro_data(grab_type = c('Available_at_that_time'),sdate = SDATE,edate=EDATE)
  
  
  ###Starmine Model Data###
  starmine_data=grab_Starmine_dt(daily_pds,SDATE,EDATE,items=c(44,119))
  
  ###Additional Fundamental Ratios to add here###
  print('Grabbing Fundamental Ratios')
  #remember use the fds in everything
  #vdt
  vdt=improved_fn_JOIN_dt(pds=pds,fds=list(CASH,EQUITY,EV_raw_Total_Debt),eds=list(),freq=freq)
  vdt[,NOA:= -ifelse(is.na(CASH),0,CASH)+ifelse(is.na(EQUITY),0,EQUITY)+ifelse(is.na(Total_Debt),0,Total_Debt)]
  vdt[,ACCRUALS := (NOA - shift(NOA,12)) / ((NOA + shift(NOA,12))/2), by='ID']
  vdt=vdt[,c('ID','Date','NOA','ACCRUALS')]
  
  #vdt1
  CAPEX=improved_grab_fund_dt(issueids,SDATE,EDATE,item='CAPEX',incl_fq = F)[,c('ID','Date','CAPEX')]
  CPS=improved_grab_fund_dt(issueids,SDATE,EDATE,item='CPS',incl_fq = F)[,c('ID','Date','CPS')]
  MINORITY_INTEREST=improved_grab_fund_dt(issueids,SDATE,EDATE,item='MINORITY_INTEREST')[,c('ID','Date','MINORITY_INTEREST')]
  PREFERRED_STOCK=improved_grab_fund_dt(issueids,SDATE,EDATE,item='PREFERRED_STOCK')[,c('ID','Date','PREFERRED_STOCK')]
  EPS=improved_grab_fund_dt(issueids,SDATE,EDATE,item='EPS')[,c('ID','Date','EPS')]
  INTANGIBLES=improved_grab_fund_dt(issueids,SDATE,EDATE,item='INTANGIBLES')[,c('ID','Date','INTANGIBLES')]
  Price=pds[,c('ID','Date','Price')]
  names(Price)=c('ID','Date','Dat_Price')
  vdt1=improved_fn_JOIN_dt(pds=pds,fds=list(CPS,CAPEX,EV_raw_Total_Debt,Price,MINORITY_INTEREST,PREFERRED_STOCK,CASH,
                                            EPS,EQUITY,INTANGIBLES),eds=list(),freq=freq)
  vdt1=vdt1[,`:=`(FCF = CPS - ifelse(is.na(CAPEX),0,CAPEX),EVshr=Dat_Price+ifelse(is.na(Total_Debt),0,Total_Debt)-
                    ifelse(is.na(CASH),0,CASH)+ifelse(is.na(MINORITY_INTEREST),0,MINORITY_INTEREST)+
                    ifelse(is.na(PREFERRED_STOCK),0,PREFERRED_STOCK),ROE=EPS/EQUITY,
                  BPS=EQUITY-ifelse(is.na(INTANGIBLES),0,INTANGIBLES),DEBT_EQUITY=Total_Debt/EQUITY,EY=EPS/Dat_Price)][,c('ID','Date','FCF','EVshr','BPS','DEBT_EQUITY','EY')]
  
  #vdt2
  LQA_Div=pds[,c('ID','Date','LQA_Div')]
  names(LQA_Div)=c('ID','Date','Dat_LQA_Div')
  EBITDA=improved_grab_fund_dt(issueids,SDATE,EDATE,item='EBT')[,c('ID','Date','EBT')]
  SAL=improved_grab_fund_dt(issueids,SDATE,EDATE,item='SAL')[,c('ID','Date','SAL')]
  RnD=improved_grab_fund_dt(issueids,SDATE,EDATE,item='RnD')[,c('ID','Date','RnD')]
  Depreciation=improved_grab_fund_dt(issueids,SDATE,EDATE,item='DEPRECIATION')[,c('ID','Date','DEPRECIATION')]
  vdt2=improved_fn_JOIN_dt(pds=pds,fds=list(CPS,CAPEX,EV_raw_Total_Debt,Price,MINORITY_INTEREST,PREFERRED_STOCK,CASH,
                                            EPS,EQUITY,INTANGIBLES,LQA_Div,EBITDA,SAL,ASSET,RnD,Depreciation),eds=list(NTM_EPS,NTM_CPS,NTM_EBT,NTM_SAL),freq=freq)
  vdt2[,`:=`(BPS=EQUITY-ifelse(is.na(INTANGIBLES),0,INTANGIBLES),FCF = CPS - ifelse(is.na(CAPEX),0,CAPEX),
             EVshr=Dat_Price+ifelse(is.na(Total_Debt),0,Total_Debt)-
               ifelse(is.na(CASH),0,CASH)+ifelse(is.na(MINORITY_INTEREST),0,MINORITY_INTEREST)+
               ifelse(is.na(PREFERRED_STOCK),0,PREFERRED_STOCK),ROE=EPS/EQUITY)]
  vdt2[,`:=`(BY=BPS/Dat_Price,CFY=CPS/Dat_Price,eEY=estEST1/Dat_Price,eCFY=estEST2/Dat_Price,DY=Dat_LQA_Div/Dat_Price,FCFY=FCF/EVshr,
             EBITDAy=EBT/EVshr,eEBITDAy=estEST3/EVshr,SALY=SAL/EVshr,eSALY=estEST4/EVshr,FCF_Margin=FCF/SAL,NI_Margin=EPS/SAL,eNI_Margin=estEST1/estEST4,
             CASH_MC=CASH/Dat_Price,Asset_Turn=SAL/ASSETS,RnD_Sales=RnD/SAL,CAPEX_FCF=CAPEX/FCF,CAPEX_Dep=CAPEX/DEPRECIATION,
             SAL_eGrowth=estEST4/SAL,EPS_eGrowth=estEST1-EPS,TBY=BPS/Dat_Price,eROE=estEST1/EPS*ROE,eND_EBT=(Total_Debt-CASH)/estEST3)]
  vdt2[,TBY:=ifelse(TBY<=0,NA,TBY)]
  vdt2=vdt2[,c('ID','Date','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY','eSALY','FCF_Margin','NI_Margin',
               'CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','SAL_eGrowth','EPS_eGrowth','TBY','eROE','eND_EBT')]
  
  
  
  
  
  ##Putting it all together###
  print('Now putting it all together')
  all_data=improved_fn_JOIN_dt(pds,freq=freq,eds=list(#Fwd1yr_EV_per_EBITDA,
                                                      #Fwd1yr_GICS_Ind_EV_per_EBITDA,Fwd1yr_GICS_Ind_P_per_BPS,Fwd1yr_GICS_Ind_P_per_CFPS,
                                                      #Fwd1yr_GICS_Ind_P_per_EPS,Fwd1yr_GICS_Sec_EV_per_EBITDA,Fwd1yr_GICS_Sec_P_per_BPS,
                                                      #Fwd1yr_GICS_Sec_P_per_CFPS,Fwd1yr_GICS_Sec_P_per_EPS,Fwd1yr_MRKT_EV_per_EBITDA,
                                                      #Fwd1yr_MRKT_P_per_BPS,Fwd1yr_MRKT_P_per_CFPS,Fwd1yr_MRKT_P_per_EPS,
                                                      #fwd1yr_P_per_BPS,fwd1yr_P_per_CFPS,fwd1yr_P_per_E,Fwd2yr_EV_per_EBITDA,
                                                      #Fwd2yr_GICS_Ind_EV_per_EBITDA,
                                                      #Fwd2yr_GICS_Ind_P_per_BPS,Fwd2yr_GICS_Ind_P_per_CFPS,Fwd2yr_GICS_Ind_P_per_EPS,
                                                      #Fwd2yr_GICS_Sec_EV_per_EBITDA,Fwd2yr_GICS_Sec_P_per_BPS,Fwd2yr_GICS_Sec_P_per_CFPS,
                                                      #Fwd2yr_GICS_Sec_P_per_EPS,Fwd2yr_MRKT_EV_per_EBITDA,Fwd2yr_MRKT_P_per_BPS,Fwd2yr_MRKT_P_per_CFPS,
                                                      #Fwd2yr_MRKT_P_per_EPS,
                                                      #fwd2yr_P_per_BPS,fwd2yr_P_per_CFPS,fwd2yr_P_per_E
                                                      #NTM_CPS,NTM_EBT,
                                                      #NTM_EPS,
                                                      #NTM_EV_per_EBITDA,
                                                      #NTM_FFO,NTM_GICS_Ind_EV_per_EBITDA,
                                                      #NTM_GICS_Ind_EV_per_EBITDA,
                                                      #NTM_GICS_Ind_P_per_BPS,NTM_GICS_Ind_P_per_CFPS,NTM_GICS_Ind_P_per_EPS,NTM_GICS_Sec_EV_per_EBITDA,
                                                      #NTM_GICS_Sec_P_per_BPS,NTM_GICS_Sec_P_per_CFPS,NTM_GICS_Sec_P_per_EPS,NTM_MRKT_EV_per_EBITDA,
                                                      #NTM_MRKT_P_per_BPS,NTM_MRKT_P_per_CFPS,NTM_MRKT_P_per_EPS,NTM_P_per_BPS,NTM_P_per_CFPS,NTM_P_per_E,
                                                      #NTM_ROA,NTM_ROE,NTM_SAL
                                                      ),
                               
                               fds=list(#Actual_fwd1yr_EV_per_EBITDA,Actual_fwd1yr_P_per_B,Actual_fwd1yr_P_per_CF,Actual_fwd1yr_P_per_E,
                                 #ASSET,CASH,COGS,
                                 DEBT_TO_EQUITY,EPS_consensus,EPS_FY_Q_growth,EPS_Growth,EPS_Q_consensus,
                                 #EQUITY,EV_raw_cash,EV_raw_Minority_Interest,EV_raw_Pref_Stock,FY_actual_BPS,
                                 #FY_actual_CPS,FY_actual_EBI,FY_actual_EBT,FY_actual_EPS,FY_actual_ROA,FY_actual_ROE,
                                 #FY_actual_SAL,
                                 #FY_GICS_Ind_EV_per_EBITDA,FY_GICS_Ind_P_per_BPS,FY_GICS_Ind_P_per_CFPS,
                                 #FY_GICS_Ind_P_per_EPS,FY_GICS_Sec_EV_per_EBITDA,FY_GICS_Sec_P_per_BPS,FY_GICS_Sec_P_per_CFPS,
                                 #FY_GICS_Sec_P_per_EPS,FY_MRKT_EV_per_EBITDA,FY_MRKT_P_per_BPS,FY_MRKT_P_per_CFPS,FY_MRKT_P_per_EPS,
                                 Mo_12_1,Mo_12_6,Mo_24_1,Mo_24_12,
                                 #Q_actual_BPS,Q_actual_CPS,Q_actual_EBI,Q_actual_EBT,
                                 #Q_actual_ROA,Q_actual_ROE,Q_actual_SAL,
                                 SAL_COGS_ASSET,
                                 #TTM_EV_per_EBITDA,
                                 #TTM_GICS_Ind_EV_per_EBITDA,
                                 #TTM_GICS_Ind_P_per_BPS,TTM_GICS_Ind_P_per_CFPS,TTM_GICS_Ind_P_per_EPS,TTM_GICS_Sec_EV_per_EBITDA,
                                 #TTM_GICS_Sec_P_per_BPS,TTM_GICS_Sec_P_per_CFPS,TTM_GICS_Sec_P_per_EPS,TTM_MRKT_EV_per_EBITDA,
                                 #TTM_MRKT_P_per_BPS,TTM_MRKT_P_per_CFPS,TTM_MRKT_P_per_EPS,
                                 #TTM_P_per_BPS,TTM_P_per_CFPS,
                                 #TTM_P_per_E,
                                 starmine_data,vdt,vdt1,vdt2))
  
  print('Finished Factor Builder')
  return(all_data)
}

ML_Builder_train=function(data=Factors_Train,dep=1,num_of_algos=c(''),pca_nums=c(),only=c('All','Value','Quality','Momentum','SI'),
                          bench=c('US','CA'),GICS=c('All'),only_bench=F,to_100_y=F,obj=c('rank:map','rank:pairwise','rank:ndcg'),
                          maker=20,samp=8,outlier_remove=F,seq_size=36,harmony=F,feats=100,lamb=50000,alp=1,iter=2500,
                          macro=F){
  set.seed(123456)
  print('Starting Train Set')
  print(paste('Starting:',GICS))
  
  dat=copy(data)
  all_data=copy(dat)
  print('Now ranking the mini factors from 0 to 100')
  all_data=all_data[,!c('i.LclPrice','i.LclOpnPrice','i.Shares','i.USD_FX','i.Price','i.Volume','i.AcutalValue_Q1',
                        'i.AcutalValue_Q1.1','i.AcutalValue_Q1.2','EQUITY','AcutalValue_Q1','i.LQA_Div','i.Vol_Median',
                        'Dividend','i.Dividend','i.MDV','i.MarketCap','i.TotRet','i.SO_Chg','ST_DEBT','LT_DEBT','BuyPct',
                        'HoldPct','SellPct',
                        'NOA','FCF','EVshr','BPS')]
  #Feature engineering
  print('Remember: Distribution quantile means quantile regression, else remove it for classification, and winsorization')
  #Normalizing DT here
  #all_them_cols=names(all_data)[3:(length(names(all_data)))]
  #all_data[,i.PayFreq:=as.numeric(i.PayFreq)]
  #all_data=calc_CSnorm_dt(all_data,var_lst = c(all_them_cols),grp_col = c('Date'),InfNA = T,posttext = '_N',zscore = F,rnk=T,verbose = T)
  
  train_act=copy(train_active)
  if(to_100_y==T){
    train_act=to_100_rank(train_act,cols_to_apply_to = c('ActiveFwdRet'),new_names = c('ActiveFwdRet'))
  }
  if(outlier_remove==T){
    #Replace vals greater than 83% quantile with 83% quantile, and same for 17% other way
    #Idea is for pairwise to git gud at the overall IC stuff, and not so at outliers
    train_act[ActiveFwdRet>=quantile(ActiveFwdRet,0.83),ActiveFwdRet:=quantile(ActiveFwdRet,0.83),by=c('Date')]
    train_act[ActiveFwdRet<=quantile(ActiveFwdRet,0.17),ActiveFwdRet:=quantile(ActiveFwdRet,0.17),by=c('Date')]
  }
  
  if('All' %in% GICS){
  } else{
    new_filter=paste('L1_',GICS[1],sep='')
    all_data=all_data[get(new_filter)==1]
  }
  print('passed new_filter')
  all_data=merge(all_data,train_act,by=c('ID','Date'))
  require(DescTools)
  #all_data[,ActiveFwdRet:=as.factor(round(ActiveFwdRet/10))]
  #all_data[,ActiveFwdRet:=Winsorize(ActiveFwdRet,minval = -0.05,maxval = 0.05)]

  setorder(all_data,Date,ID)
  
  port=copy(all_data)
  port=calc_CSnorm_dt(port,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
                                          N_Neg_Volatility='-Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',
                                          N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth')
                      ,grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
  #CA Benchmark
  if(bench[1]=='CA' & 'All' %in% only){
    port[,Basic_Mean:=(1/3*((N_Mom_365_30+N_ARM_100_REG)/2)+1/3*((N_DY+N_CFY+N_EY+N_eEY)/4)
                       +1/3*((N_EQ_REG_RANK+N_Neg_Volatility)/2)),by=c('ID','Date')]
  }
  if(bench[1]=='CA' & 'Value' %in% only){
    port[,Basic_Mean:=((0.05*N_DY+0.05*N_CFY+0.05*N_EY+0.05*N_eEY)/4),by=c('ID','Date')]
  }
  if(bench[1]=='CA' & 'Quality' %in% only){
    port[,Basic_Mean:=((0.1*N_EQ_REG_RANK+0.1*N_Neg_Volatility)/2),by=c('ID','Date')]
  }
  if(bench[1]=='CA' & 'Momentum' %in% only){
    port[,Basic_Mean:=((0.25*N_Mom_365_30+0.25*N_ARM_100_REG)/2),by=c('ID','Date')]
  }
  #US Benchmark
  if(bench[1]=='US' & 'All' %in% only){
    port[,Basic_Mean:=((1/3)*((2*N_ARM_100_REG+2*N_Mom_365_30+N_SAL_eGrowth+N_EPS_eGrowth)/6)
                       +(1/3)*((N_DY+N_CFY+N_EY+N_eEY)/4)
                       +(1/3)*((N_EQ_REG_RANK+N_Neg_Volatility)/2)),by=c('ID','Date')]
  }
  if(bench[1]=='US' & 'Value' %in% only){
    port[,Basic_Mean:=((1/12)*N_DY+(1/12)*N_CFY+(1/12)*N_EY+(1/12)*N_eEY),by=c('ID','Date')]
  }
  if(bench[1]=='US' & 'Quality' %in% only){
    port[,Basic_Mean:=((1/3)*(N_EQ_REG_RANK+N_Neg_Volatility)),by=c('ID','Date')]
  }
  if(bench[1]=='US' & 'Momentum' %in% only){
    port[,Basic_Mean:=((1/9)*N_ARM_100_REG+(1/9)*N_Mom_365_30+(1/18)*N_SAL_eGrowth+(1/18)*N_EPS_eGrowth)
         ,by=c('ID','Date')]
  }
  #ALL
  if('SI' %in% only){
    port[,Basic_Mean:=(-1*Utilisation),by=c('ID','Date')]
  }
  if('SI_prune' %in% only){
    port[,Basic_Mean:=(-1*ActiveUtilisation),by=c('ID','Date')]
  }
  
  setorder(port,Date,ID)
  port=to_100_rank(port,cols_to_apply_to = c('Basic_Mean'),new_names = c('Basic_Mean'))
  portfolio=Portfolio_maker(port,cols_to_apply_to = 'Basic_Mean',top_bot_pct = 20,pds=test_pds)
  print('Basic Benchmark Ready')
  
  port123=copy(all_data)
  port123[,Basic_Mo:=Mom_365_30]
  port123=to_100_rank(port123,cols_to_apply_to = c('Basic_Mo'),new_names = c('Basic_Mo'))
  portfolio123=Portfolio_maker(port123,cols_to_apply_to = 'Basic_Mo',top_bot_pct = 20,pds=train_pds)
  portfolio=merge(portfolio,portfolio123,by=c('ID','Date'))
  print('Basic Momentum Ready')
  if('Lin_Reg' %in% num_of_algos){
    set.seed(123456)
    print('Starting Linear Regression')
    port=copy(all_data)
    port=as.data.frame(port)
    port=port[!duplicated(as.list(port))]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]

    
    
    lin_reg<<-lm(ActiveFwdRet ~ EV_per_acutal + Price_per_acutal + i.Price_per_acutal + i.Price_per_acutal.1 + ASSETS + CASH + COGS + LT_DEBT + ST_DEBT + EQUITY + DEBT_TO_EQUITY_RATIO + Consensus + Growth + i.Growth + i.Consensus + MINORITY_INTEREST + PREFERRED_STOCK + AcutalValue_FY1 + i.AcutalValue_FY1 + i.AcutalValue_FY1.1 + i.AcutalValue_FY1.2 + i.AcutalValue_FY1.3 + i.AcutalValue_FY1.4 + i.AcutalValue_FY1.5 + i.SurpriseMean_FY1.5 + i.AcutalValue_FY1.6 + GICS_Industry_Ratio + i.GICS_Industry_Ratio + i.GICS_Industry_Ratio.2 + GICS_Sector_Ratio + i.GICS_Sector_Ratio + i.GICS_Sector_Ratio.2 + BuyPct + HoldPct + SellPct + BuyPct_30_days + HoldPct_30_days + SellPct_30_days + BuyPct_60_days + HoldPct_60_days + SellPct_60_days + BuyPct_90_days + HoldPct_90_days + SellPct_90_days + Mom_365_30 + Mom_365_182 + Mom_730_30 + Mom_730_365 + AcutalValue_Q1 + i.AcutalValue_Q1 + i.AcutalValue_Q1.1 + i.AcutalValue_Q1.2 + i.AcutalValue_Q1.3 + i.AcutalValue_Q1.4 + i.AcutalValue_Q1.5 + Ratio + i.EV_per_acutal + i.GICS_Industry_Ratio.3 + i.GICS_Industry_Ratio.4 + i.GICS_Industry_Ratio.6 + i.GICS_Industry_Ratio.7 + i.GICS_Industry_Ratio.8 + i.GICS_Industry_Ratio.10 + i.Price_per_acutal.2 + i.Price_per_acutal.3 + i.Price_per_acutal.4 + estEST1 + estEST2 + estEST3 + estEST4 + estEST5 + estEST6 + estEST7 + estEST8 + estEST9 + estEST14 + estEST15 + estEST16 + estEST17 + estEST18 + estEST19 + estEST20 + estEST21 + estEST22 + estEST23 + estEST24 + estEST25 + estEST30 + estEST31 + estEST32 + estEST33 + estEST34 + estEST35 + estEST36 + estEST37 + estEST38 + estEST41 + estEST42 + estEST43 + estEST44 + estEST45 + estEST46 + estEST48 + estEST49 + estEST53 + estEST54 + estEST55 + estEST56 + estEST57 + estEST58 + lagged + increase + i.lagged + i.increase + i.lagged.1 + i.increase.1 + estEST61 + i.lagged.2 + i.increase.2 + i.lagged.3 + i.increase.3 + i.lagged.4 + i.increase.4 + estEST64 + i.lagged.5 + i.increase.5 + i.lagged.6,data=port)
    port$Linear_Regression=predict(lin_reg,port)
    port=cbind(ids,port)
    port[is.na(Linear_Regression),Linear_Regression:=weighted.mean(
      c(EV_per_acutal, Price_per_acutal, i.Price_per_acutal, 
        i.Price_per_acutal.1, ASSETS, CASH, COGS, LT_DEBT, ST_DEBT, 
        EQUITY, DEBT_TO_EQUITY_RATIO, Consensus, Growth, i.Growth, 
        i.Consensus, MINORITY_INTEREST, PREFERRED_STOCK, AcutalValue_FY1, 
        i.AcutalValue_FY1, i.AcutalValue_FY1.1, i.AcutalValue_FY1.2, 
        i.AcutalValue_FY1.3, i.AcutalValue_FY1.4, i.AcutalValue_FY1.5, 
        i.SurpriseMean_FY1.5, i.AcutalValue_FY1.6, GICS_Industry_Ratio, 
        i.GICS_Industry_Ratio, i.GICS_Industry_Ratio.2, 
        GICS_Sector_Ratio, i.GICS_Sector_Ratio, i.GICS_Sector_Ratio.2, 
        BuyPct, HoldPct, SellPct, BuyPct_30_days, HoldPct_30_days, 
        SellPct_30_days, BuyPct_60_days, HoldPct_60_days, SellPct_60_days, 
        BuyPct_90_days, HoldPct_90_days, SellPct_90_days, Mom_365_30, 
        Mom_365_182, Mom_730_30, Mom_730_365, AcutalValue_Q1, i.AcutalValue_Q1, 
        i.AcutalValue_Q1.1, i.AcutalValue_Q1.2, i.AcutalValue_Q1.3, 
        i.AcutalValue_Q1.4, i.AcutalValue_Q1.5, Ratio, i.EV_per_acutal, 
        i.GICS_Industry_Ratio.3, i.GICS_Industry_Ratio.4, i.GICS_Industry_Ratio.6, 
        i.GICS_Industry_Ratio.7, i.GICS_Industry_Ratio.8, i.GICS_Industry_Ratio.10, 
        i.Price_per_acutal.2, i.Price_per_acutal.3, i.Price_per_acutal.4, 
        estEST1, estEST2, estEST3, estEST4, estEST5, estEST6, estEST7, 
        estEST8, estEST9, estEST14, estEST15, estEST16, estEST17, 
        estEST18, estEST19, estEST20, estEST21, estEST22, estEST23, 
        estEST24, estEST25, estEST30, estEST31, estEST32, estEST33, 
        estEST34, estEST35, estEST36, estEST37, estEST38, estEST41, 
        estEST42, estEST43, estEST44, estEST45, estEST46, estEST48, 
        estEST49, estEST53, estEST54, estEST55, estEST56, 
        estEST57, estEST58, lagged, increase, i.lagged, i.increase, 
        i.lagged.1, i.increase.1, estEST61, i.lagged.2, i.increase.2, 
        i.lagged.3, i.increase.3, i.lagged.4, i.increase.4, estEST64, 
        i.lagged.5, i.increase.5, i.lagged.6)
      ,w=c(lin_reg$coefficients[2:29],lin_reg$coefficients[31:106],lin_reg$coefficients[108:130]),na.rm = T),by=c('ID','Date')]
    abc=port[,c('ID','Date','Linear_Regression')]
    port1=to_100_rank(port,cols_to_apply_to = c('Linear_Regression'),new_names = c('Linear_Regression'))
    portfolio1=Portfolio_maker(port1,cols_to_apply_to = 'Linear_Regression',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio1,by=c('ID','Date'))
    
    print('lin reg was successful')
    
  }
  
  if('Elastic_Net' %in% num_of_algos){
    print('Starting Elastic Net')
    set.seed(123456)
    require(MASS)
    port=copy(Filter(function(x) !all(is.na(x)|x == 0), all_data))
    port=as.data.frame(port)
    port=port[!duplicated(as.list(port))]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]

    Elastic_Net<<-lm.ridge(ActiveFwdRet ~ EV_per_acutal + Price_per_acutal + i.Price_per_acutal + i.Price_per_acutal.1 + ASSETS + CASH + COGS + LT_DEBT + ST_DEBT + EQUITY + DEBT_TO_EQUITY_RATIO + Consensus + Growth + i.Growth + i.Consensus + MINORITY_INTEREST + PREFERRED_STOCK + AcutalValue_FY1 + i.AcutalValue_FY1 + i.AcutalValue_FY1.1 + i.AcutalValue_FY1.2 + i.AcutalValue_FY1.3 + i.AcutalValue_FY1.4 + i.AcutalValue_FY1.5 + i.SurpriseMean_FY1.5 + i.AcutalValue_FY1.6 + GICS_Industry_Ratio + i.GICS_Industry_Ratio + i.GICS_Industry_Ratio.2 + GICS_Sector_Ratio + i.GICS_Sector_Ratio + i.GICS_Sector_Ratio.2 + BuyPct + HoldPct + SellPct + BuyPct_30_days + HoldPct_30_days + SellPct_30_days + BuyPct_60_days + HoldPct_60_days + SellPct_60_days + BuyPct_90_days + HoldPct_90_days + SellPct_90_days + Mom_365_30 + Mom_365_182 + Mom_730_30 + Mom_730_365 + AcutalValue_Q1 + i.AcutalValue_Q1 + i.AcutalValue_Q1.1 + i.AcutalValue_Q1.2 + i.AcutalValue_Q1.3 + i.AcutalValue_Q1.4 + i.AcutalValue_Q1.5 + Ratio + i.EV_per_acutal + i.GICS_Industry_Ratio.3 + i.GICS_Industry_Ratio.4 + i.GICS_Industry_Ratio.6 + i.GICS_Industry_Ratio.7 + i.GICS_Industry_Ratio.8 + i.GICS_Industry_Ratio.10 + i.Price_per_acutal.2 + i.Price_per_acutal.3 + i.Price_per_acutal.4 + estEST1 + estEST2 + estEST3 + estEST4 + estEST5 + estEST6 + estEST7 + estEST8 + estEST9 + estEST14 + estEST15 + estEST16 + estEST17 + estEST18 + estEST19 + estEST20 + estEST21 + estEST22 + estEST23 + estEST24 + estEST25 + estEST30 + estEST31 + estEST32 + estEST33 + estEST34 + estEST35 + estEST36 + estEST37 + estEST38 + estEST41 + estEST42 + estEST43 + estEST44 + estEST45 + estEST46 + estEST48 + estEST49 + estEST53 + estEST54 + estEST55 + estEST56 + estEST57 + estEST58 + lagged + increase + i.lagged + i.increase + i.lagged.1 + i.increase.1 + estEST61 + i.lagged.2 + i.increase.2 + i.lagged.3 + i.increase.3 + i.lagged.4 + i.increase.4 + estEST64 + i.lagged.5 + i.increase.5 + i.lagged.6,data=port)
    #port$Elastic_Net=predict(Elastic_Net,port)
    port=cbind(ids,port)
    port[,Elastic_Net:=weighted.mean(
      c(EV_per_acutal, Price_per_acutal, i.Price_per_acutal, 
        i.Price_per_acutal.1, ASSETS, CASH, COGS, LT_DEBT, ST_DEBT, 
        EQUITY, DEBT_TO_EQUITY_RATIO, Consensus, Growth, i.Growth, 
        i.Consensus, MINORITY_INTEREST, PREFERRED_STOCK, AcutalValue_FY1, 
        i.AcutalValue_FY1, i.AcutalValue_FY1.1, i.AcutalValue_FY1.2, 
        i.AcutalValue_FY1.3, i.AcutalValue_FY1.4, i.AcutalValue_FY1.5, 
        i.SurpriseMean_FY1.5, i.AcutalValue_FY1.6, GICS_Industry_Ratio, 
        i.GICS_Industry_Ratio, i.GICS_Industry_Ratio.2, 
        GICS_Sector_Ratio, i.GICS_Sector_Ratio, i.GICS_Sector_Ratio.2, 
        BuyPct, HoldPct, SellPct, BuyPct_30_days, HoldPct_30_days, 
        SellPct_30_days, BuyPct_60_days, HoldPct_60_days, SellPct_60_days, 
        BuyPct_90_days, HoldPct_90_days, SellPct_90_days, Mom_365_30, 
        Mom_365_182, Mom_730_30, Mom_730_365, AcutalValue_Q1, i.AcutalValue_Q1, 
        i.AcutalValue_Q1.1, i.AcutalValue_Q1.2, i.AcutalValue_Q1.3, 
        i.AcutalValue_Q1.4, i.AcutalValue_Q1.5, Ratio, i.EV_per_acutal, 
        i.GICS_Industry_Ratio.3, i.GICS_Industry_Ratio.4, i.GICS_Industry_Ratio.6, 
        i.GICS_Industry_Ratio.7, i.GICS_Industry_Ratio.8, i.GICS_Industry_Ratio.10, 
        i.Price_per_acutal.2, i.Price_per_acutal.3, i.Price_per_acutal.4, 
        estEST1, estEST2, estEST3, estEST4, estEST5, estEST6, estEST7, 
        estEST8, estEST9, estEST14, estEST15, estEST16, estEST17, 
        estEST18, estEST19, estEST20, estEST21, estEST22, estEST23, 
        estEST24, estEST25, estEST30, estEST31, estEST32, estEST33, 
        estEST34, estEST35, estEST36, estEST37, estEST38, estEST41, 
        estEST42, estEST43, estEST44, estEST45, estEST46, estEST48, 
        estEST49, estEST53, estEST54, estEST55, estEST56, 
        estEST57, estEST58, lagged, increase, i.lagged, i.increase, 
        i.lagged.1, i.increase.1, estEST61, i.lagged.2, i.increase.2, 
        i.lagged.3, i.increase.3, i.lagged.4, i.increase.4, estEST64, 
        i.lagged.5, i.increase.5, i.lagged.6)
      ,w=c(Elastic_Net$coef[2:29],Elastic_Net$coef[31:106],Elastic_Net$coef[108:130]),na.rm = T),by=c('ID','Date')]
    abc=port[,c('ID','Date','Elastic_Net')]
    port2=to_100_rank(port,cols_to_apply_to = c('Elastic_Net'),new_names = c('Elastic_Net'))
    portfolio2=Portfolio_maker(port2,cols_to_apply_to = 'Elastic_Net',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio2,by=c('ID','Date'))
    
    print('Elastic Net was successful')
  }
  
  if('Random_Forest' %in% num_of_algos){
    #require(h2o)
    print('Starting Random Forest')
    set.seed(123456)
    port=copy(as.data.table(all_data))
    if('Value' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    #port=copy(all_data)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    
    Random_Forest<<-h2o::h2o.randomForest(y='ActiveFwdRet',training_frame = port,ntrees=700,max_depth=2,
                                          seed=123456,verbose = T
                                          ,stopping_metric = 'MSE'
                                          ,distribution = 'quantile'
                                          ,min_rows = 10,
                                          nbins = 40,nbins_top_level = 1024,sample_rate = 0.6320000291
                                          ,histogram_type = 'QuantilesGlobal',stopping_tolerance = 0.001,
                                          col_sample_rate_per_tree = 0.9)
    print('Starting preds')
    preds=h2o::h2o.predict(Random_Forest,newdata = port)
    #Classification: thing is misclassification, otherwise: go with MSE, and get rid of line underneath
    preds=as.data.table(preds)
    port=as.data.table(port)
    #port$Random_Forest=as.numeric(preds$predict)
    port$Random_Forest=preds
    port=cbind(ids,port)
    abc=port[,c('ID','Date','Random_Forest')]
    port3=to_100_rank(port,cols_to_apply_to = c('Random_Forest'),new_names = c('Random_Forest'))
    portfolio3=Portfolio_maker(port3,cols_to_apply_to = 'Random_Forest',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio3,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('Random Forest was successful')
  }
  
  if('GBM' %in% num_of_algos){
    #require(h2o)
    set.seed(123456)
    port=copy(as.data.table(all_data))
    if('Value' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    print('Starting Gradient Boosted Model')
    #port=copy(Filter(function(x) !all(is.na(x)|x == 0), all_data))
    #port=copy(all_data)
    port=as.data.frame(port)
    #port=port[!duplicated(as.list(port))]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    
    GBM<<-h2o::h2o.gbm(y='ActiveFwdRet',training_frame = port,ntrees = 800,max_depth = 2,min_rows = 1,
                       nbins=40,seed=123456,learn_rate = 0.2
                       ,sample_rate = 0.6320000291,
                       col_sample_rate_per_tree = 0.9,pred_noise_bandwidth = 0
                       ,quantile_alpha = 0.1,nbins_top_level = 1024,verbose = T)
    
    preds=h2o::h2o.predict(GBM,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$GBM=as.numeric(preds$predict)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','GBM')]
    port4=to_100_rank(port,cols_to_apply_to = c('GBM'),new_names = c('GBM'))
    portfolio4=Portfolio_maker(port4,cols_to_apply_to = 'GBM',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio4,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('Gradient Boosted Model was successful')
  }
  
  if('FFNN' %in% num_of_algos){
    #require(h2o)
    set.seed(123456)
    port=copy(as.data.table(all_data))
    if('Value' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    print('Starting Feed Forward NN Model')
    #port=copy(Filter(function(x) !all(is.na(x)|x == 0), all_data))
    #port=copy(all_data)
    port=as.data.frame(port)
    #port=port[!duplicated(as.list(port))]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    
    FFNN<<-h2o::h2o.deeplearning(y='ActiveFwdRet',training_frame = port,verbose = T,hidden=c(100,100)
                                 #,quantile_alpha = 0.5#,distribution = 'quantile'
                                 ,activation = 'RectifierWithDropout',epochs=2000,seed=123456,adaptive_rate = T,mini_batch_size = 3000
                                 )
    preds=h2o::h2o.predict(FFNN,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$FFNN=as.numeric(preds$predict)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','FFNN')]
    port5=to_100_rank(port,cols_to_apply_to = c('FFNN'),new_names = c('FFNN'))
    portfolio5=Portfolio_maker(port5,cols_to_apply_to = 'FFNN',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio5,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('Feed Forward NN was successful')
  }
  
  if('SVM' %in% num_of_algos){
    require(e1071)
    set.seed(123456)
    print('Starting SVM')
    #port=copy(Filter(function(x) !all(is.na(x)|x == 0), all_data))
    port=copy(all_data)
    port=as.data.frame(port)
    port=port[!duplicated(as.list(port))]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[is.na(port)]=0
    port[,`:=`(ID=NULL,Date=NULL)]
    
    SVM<<-svm(ActiveFwdRet~.,data=port)
    port$SVM=predict(SVM,port)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','SVM')]
    port6=to_100_rank(port,cols_to_apply_to = c('SVM'),new_names = c('SVM'))
    portfolio6=Portfolio_maker(port6,cols_to_apply_to = 'SVM',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio6,by=c('ID','Date'))
    
  }
  
  if('RNN' %in% num_of_algos){
    #Not working as of yet
    set.seed(123456)
    require(keras)
    require(tensorflow)
    port=as.data.table(all_data)
    port=calc_CSnorm_dt(port,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
                                                    N_Volatility_185_0='Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',N_Mom_31_0='Mom_31_0',
                                                    N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth',N_ActiveFwdRet='ActiveFwdRet'
    )
    ,grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
    
    port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY','CFY',
                 'EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                 'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY','N_CFY',
                 'N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth','N_Mom_31_0')]
    setorder(port,Date,ID)
    invisible(lapply(names(port),function(.name) set(port, which(is.infinite(port[[.name]])), j = .name,value =NA)))
    invisible(lapply(names(port),function(.name) set(port, which(is.nan(port[[.name]])), j = .name,value =NA)))
    #Data prep for RNN
    #In sample timestep: 40
    step=seq_size
    step_size<<-step
    port[,Cor_Mom:=cor(Mom_365_30,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_ARM:=cor(ARM_100_REG,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EQ:=cor(EQ_REG_RANK,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Vol:=cor(Volatility_185_0,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_DY:=cor(DY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_CFY:=cor(CFY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EY:=cor(EY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_eEY:=cor(eEY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_SAL:=cor(SAL_eGrowth,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EPS:=cor(EPS_eGrowth,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Mom31:=cor(Mom_31_0,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    
    #Outputs to find
    if(bench[1]=='CA'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    if(bench[1]=='US'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }

    port[,Cor_Value:=cor(Value,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Momentum:=cor(Mom,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Qual:=cor(Qual,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    
    #Normalized inputs
    port[,Norm_Mom:=mean(Mom_365_30,na.rm = T),by=c('Date')]
    port[,Norm_ARM:=mean(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,Norm_EQ:=mean(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,Norm_Vol:=mean(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,Norm_DY:=mean(DY,na.rm = T),by=c('Date')]
    port[,Norm_CFY:=mean(CFY,na.rm = T),by=c('Date')]
    port[,Norm_EY:=mean(EY,na.rm = T),by=c('Date')]
    port[,Norm_eEY:=mean(eEY,na.rm = T),by=c('Date')]
    port[,Norm_SAL:=mean(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,Norm_EPS:=mean(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,Norm_Mom31:=mean(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,SD_Mom:=var(Mom_365_30,na.rm = T),by=c('Date')]
    port[,SD_ARM:=var(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,SD_EQ:=var(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,SD_Vol:=var(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,SD_DY:=var(DY,na.rm = T),by=c('Date')]
    port[,SD_CFY:=var(CFY,na.rm = T),by=c('Date')]
    port[,SD_EY:=var(EY,na.rm = T),by=c('Date')]
    port[,SD_eEY:=var(eEY,na.rm = T),by=c('Date')]
    port[,SD_SAL:=var(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,SD_EPS:=var(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,SD_Mom31:=var(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,IQR_Mom:=IQR(Mom_365_30,na.rm = T),by=c('Date')]
    port[,IQR_ARM:=IQR(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,IQR_EQ:=IQR(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,IQR_Vol:=IQR(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,IQR_DY:=IQR(DY,na.rm = T),by=c('Date')]
    port[,IQR_CFY:=IQR(CFY,na.rm = T),by=c('Date')]
    port[,IQR_EY:=IQR(EY,na.rm = T),by=c('Date')]
    port[,IQR_eEY:=IQR(eEY,na.rm = T),by=c('Date')]
    port[,IQR_SAL:=IQR(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_EPS:=IQR(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_Mom31:=IQR(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,K_Mom:=kurtosis(Mom_365_30,na.rm = T),by=c('Date')]
    port[,K_ARM:=kurtosis(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,K_EQ:=kurtosis(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,K_Vol:=kurtosis(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,K_DY:=kurtosis(DY,na.rm = T),by=c('Date')]
    port[,K_CFY:=kurtosis(CFY,na.rm = T),by=c('Date')]
    port[,K_EY:=kurtosis(EY,na.rm = T),by=c('Date')]
    port[,K_eEY:=kurtosis(eEY,na.rm = T),by=c('Date')]
    port[,K_SAL:=kurtosis(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,K_EPS:=kurtosis(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,K_Mom31:=kurtosis(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,S_Mom:=skewness(Mom_365_30,na.rm = T),by=c('Date')]
    port[,S_ARM:=skewness(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,S_EQ:=skewness(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,S_Vol:=skewness(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,S_DY:=skewness(DY,na.rm = T),by=c('Date')]
    port[,S_CFY:=skewness(CFY,na.rm = T),by=c('Date')]
    port[,S_EY:=skewness(EY,na.rm = T),by=c('Date')]
    port[,S_eEY:=skewness(eEY,na.rm = T),by=c('Date')]
    port[,S_SAL:=skewness(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,S_EPS:=skewness(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,S_Mom31:=skewness(Mom_31_0,na.rm = T),by=c('Date')]
    
    og_port=port
    port=unique(port,by=c('Date'))
    setorder(port,Date,ID)
    #Additional inputs to use
    port[,U_Cor_Mom:=shift(Cor_Mom,n=2,typ='lag')]
    port[,U_Cor_ARM:=shift(Cor_ARM,n=2,typ='lag')]
    port[,U_Cor_EQ:=shift(Cor_EQ,n=2,typ='lag')]
    port[,U_Cor_Vol:=shift(Cor_Vol,n=2,typ='lag')]
    port[,U_Cor_DY:=shift(Cor_DY,n=2,typ='lag')]
    port[,U_Cor_CFY:=shift(Cor_CFY,n=2,typ='lag')]
    port[,U_Cor_EY:=shift(Cor_EY,n=2,typ='lag')]
    port[,U_Cor_eEY:=shift(Cor_eEY,n=2,typ='lag')]
    port[,U_Cor_SAL:=shift(Cor_SAL,n=2,typ='lag')]
    port[,U_Cor_EPS:=shift(Cor_EPS,n=2,typ='lag')]
    port[,U_Cor_Mom31:=shift(Cor_Mom31,n=2,typ='lag')]
    port[,U_Value:=shift(Cor_Value,n=2,typ='lag')]
    port[,U_Momentum:=shift(Cor_Momentum,n=2,typ='lag')]
    port[,U_Qual:=shift(Cor_Qual,n=2,typ='lag')]
    #Sending the last out one as the y sample, AKA, what we want to predict on
    in_sample=port[,c('Norm_Mom','Norm_ARM','Norm_EQ','Norm_Vol','Norm_DY','Norm_CFY','Norm_EY','Norm_eEY','Norm_SAL',
                      'Norm_EPS','Norm_Mom31',
                      'SD_Mom','SD_ARM','SD_EQ','SD_Vol','SD_DY','SD_CFY','SD_EY','SD_eEY','SD_SAL',
                      'SD_EPS','SD_Mom31',
                      'IQR_Mom','IQR_ARM','IQR_EQ','IQR_Vol','IQR_DY','IQR_CFY','IQR_EY','IQR_eEY','IQR_SAL',
                      'IQR_EPS','IQR_Mom31',
                      'K_Mom','K_ARM','K_EQ','K_Vol','K_DY','K_CFY','K_EY','K_eEY','K_SAL','K_EPS','K_Mom31',
                      'S_Mom','S_ARM','S_EQ','S_Vol','S_DY','S_CFY','S_EY','S_eEY','S_SAL','S_EPS','S_Mom31',
                      'U_Cor_Mom','U_Cor_ARM','U_Cor_EQ','U_Cor_Vol','U_Cor_DY','U_Cor_CFY','U_Cor_EY',
                      'U_Cor_eEY','U_Cor_SAL','U_Cor_EPS','U_Cor_Mom31','U_Value','U_Momentum','U_Qual')]
    out_sample=port[,c('Cor_Value','Cor_Mom','Cor_Qual')]
    Xs=in_sample[Norm_Mom==564654654]
    Ys=out_sample[Cor_Mom==5]
    sample=length(unique(port$Date))-step
    for(l in seq(1,sample)){
      print(paste('Doing the first',l,'groups (rolling)'))
      first_in=in_sample[l+seq(0,step),]
      first_out=out_sample[l+c(step),]
      Xs=rbind(Xs,first_in)
      Ys=rbind(Ys,first_out)
    }
    
    x=Xs
    n_col=ncol(x)
    x=as.matrix(x)
    x[is.infinite(x) | is.na(x) | is.nan(x)]=0
    y=as.matrix(Ys$Cor_Mom)
    y2=as.matrix(Ys$Cor_Value)
    y3=as.matrix(Ys$Cor_Qual)
    
    #Time to re-arrange X and Y appropriately (to array)
    X=array(x,dim=c(sample,step,n_col))
    Y=array(y,dim=c(sample,1))
    Y2=array(y2,dim=c(sample,1))
    Y3=array(y3,dim=c(sample,1))
    
    RNN_Mo<<- keras_model_sequential() %>%
      bidirectional(layer_gru(units=10,return_sequences = T),input_shape=c(step,n_col)) %>%
      layer_dropout(0.2) %>%
      bidirectional(layer_gru(units=10)) %>%
      layer_dropout(0.2) %>%
      layer_dense(units=1)
    RNN_Mo %>% compile(loss = 'mse',optimizer = 'adam')
    filepath="weights.best.hdf5"
    dank_back=callback_model_checkpoint(filepath,monitor='val_loss',verbose=1,save_best_only = T,save_weights_only = T,mode='min')
    RNN_Mo  %>% fit(X,Y,epochs=1200,validation_split=0.25,verbose=1,callbacks=list(dank_back))
    RNN_Mo %>% load_model_weights_hdf5(filepath = filepath)
    
    RNN_Val<<- keras_model_sequential() %>% 
      bidirectional(layer_gru(units=10,return_sequences = T),input_shape=c(step,n_col)) %>%
      layer_dropout(0.2) %>%
      bidirectional(layer_gru(units=10)) %>%
      layer_dropout(0.2) %>%
      layer_dense(units=1)
    RNN_Val %>% compile(loss = 'mse',optimizer = 'adam')
    filepath="weights.best.hdf5"
    dank_back=callback_model_checkpoint(filepath,monitor='val_loss',verbose=1,save_best_only = T,save_weights_only = T,mode='min')
    RNN_Val  %>% fit(X,Y2,epochs=1200,validation_split=0.25,verbose=1,callbacks=list(dank_back))
    RNN_Val %>% load_model_weights_hdf5(filepath = filepath)
    
    RNN_Qual<<- keras_model_sequential() %>% 
      bidirectional(layer_gru(units=10,return_sequences = T),input_shape=c(step,n_col)) %>%
      layer_dropout(0.2) %>%
      bidirectional(layer_gru(units=10)) %>%
      layer_dropout(0.2) %>%
      layer_dense(units=1)
    RNN_Qual %>% compile(loss = 'mse',optimizer = 'adam')
    filepath="weights.best.hdf5"
    dank_back=callback_model_checkpoint(filepath,monitor='val_loss',verbose=1,save_best_only = T,save_weights_only = T,mode='min')
    RNN_Qual  %>% fit(X,Y3,epochs=1200,validation_split=0.25,verbose=1,callbacks=list(dank_back))
    RNN_Qual %>% load_model_weights_hdf5(filepath = filepath)
    
  }
  
  if('RNN_Mo' %in% num_of_algos){
    set.seed(123456)
    require(keras)
    require(tensorflow)
    port=as.data.table(all_data)
    port=calc_CSnorm_dt(port,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
                                            N_Volatility_185_0='Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',N_Mom_31_0='Mom_31_0',
                                            N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth',N_ActiveFwdRet='ActiveFwdRet'
    )
    ,grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
    
    if(macro==F){
      port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY','CFY',
                   'EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                   'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY','N_CFY',
                   'N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth','N_Mom_31_0')]
    }
    else{
      port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY','CFY',
                   'EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                   'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY','N_CFY',
                   'N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth','N_Mom_31_0',macro_names),with=F]
    }
    
    setorder(port,Date,ID)
    invisible(lapply(names(port),function(.name) set(port, which(is.infinite(port[[.name]])), j = .name,value =NA)))
    invisible(lapply(names(port),function(.name) set(port, which(is.nan(port[[.name]])), j = .name,value =NA)))
    #Data prep for RNN
    #In sample timestep: 40
    step=seq_size
    step_size<<-step
    #Outputs to find
    if(bench[1]=='CA'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    if(bench[1]=='US'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    #Grabbing previous Rets
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'Mom',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_Mom')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_Mom:=sum(overall_weight_Mom*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'ARM_100_REG',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_ARM_100_REG')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_ARM:=sum(overall_weight_ARM_100_REG*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'EQ_REG_RANK',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_EQ_REG_RANK')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_EQ:=sum(overall_weight_EQ_REG_RANK*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'Mom_31_0',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_Mom_31_0')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_Mom31:=sum(overall_weight_Mom_31_0*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'DY',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_DY')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_DY:=sum(overall_weight_DY*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'CFY',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_CFY')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_CFY:=sum(overall_weight_CFY*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'EY',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_EY')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_EY:=sum(overall_weight_EY*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'eEY',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_eEY')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_eEY:=sum(overall_weight_eEY*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'EPS_eGrowth',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_EPS_eGrowth')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_EPS:=sum(overall_weight_EPS_eGrowth*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'Volatility_185_0',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_Volatility_185_0')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_Vol:=sum(overall_weight_Volatility_185_0*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'SAL_eGrowth',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_SAL_eGrowth')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_SAL:=sum(overall_weight_SAL_eGrowth*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    port[,Cor_Value:=cor(Value,ActiveFwdRet,use='pairwise.complete',method='pearson'),by=c('Date')]
    port[,Cor_Qual:=cor(Qual,ActiveFwdRet,use='pairwise.complete',method='pearson'),by=c('Date')]
    
    #Normalized inputs
    port[,Norm_Mom:=mean(Mom_365_30,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_Mom)][,Norm_Mom:=Norm_Mom/temp_max]
    port[,Norm_ARM:=mean(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,Norm_EQ:=mean(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,Norm_Vol:=mean(Volatility_185_0,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_Vol)][,Norm_Mom:=Norm_Vol/temp_max]
    port[,Norm_DY:=mean(DY,na.rm = T),by=c('Date')]#[,temp_DY:=cummaxNA(Norm_DY)][,Norm_DY:=Norm_DY/temp_max]
    port[,Norm_CFY:=mean(CFY,na.rm = T),by=c('Date')]#[,temp_CFY:=cummaxNA(Norm_CFY)][,Norm_CFY:=Norm_CFY/temp_max]
    port[,Norm_EY:=mean(EY,na.rm = T),by=c('Date')]#[,temp_EY:=cummaxNA(Norm_EY)][,Norm_EY:=Norm_EY/temp_max]
    port[,Norm_eEY:=mean(eEY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_eEY)][,Norm_eEY:=Norm_eEY/temp_max]
    port[,Norm_SAL:=mean(SAL_eGrowth,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_SAL)][,Norm_SAL:=Norm_SAL/temp_max]
    port[,Norm_EPS:=mean(EPS_eGrowth,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_EPS)][,Norm_EPS:=Norm_EPS/temp_max]
    port[,Norm_Mom31:=mean(Mom_31_0,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_Mom31)][,Norm_Mom31:=Norm_Mom31/temp_max]
    
    port[,SD_Mom:=var(Mom_365_30,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_Mom)][,SD_Mom:=SD_Mom/temp_max]
    port[,SD_ARM:=var(ARM_100_REG,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_ARM)][,SD_ARM:=SD_ARM/temp_max]
    port[,SD_EQ:=var(EQ_REG_RANK,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_EQ)][,SD_EQ:=SD_EQ/temp_max]
    port[,SD_Vol:=var(Volatility_185_0,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_Vol)][,SD_Vol:=SD_Vol/temp_max]
    port[,SD_DY:=var(DY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_DY)][,SD_DY:=SD_DY/temp_max]
    port[,SD_CFY:=var(CFY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_CFY)][,SD_CFY:=SD_CFY/temp_max]
    port[,SD_EY:=var(EY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_EY)][,SD_EY:=SD_EY/temp_max]
    port[,SD_eEY:=var(eEY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_eEY)][,SD_eEY:=SD_eEY/temp_max]
    port[,SD_SAL:=var(SAL_eGrowth,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_SAL)][,SD_SAL:=SD_SAL/temp_max]
    port[,SD_EPS:=var(EPS_eGrowth,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_EPS)][,SD_EPS:=SD_EPS/temp_max]
    port[,SD_Mom31:=var(Mom_31_0,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_Mom31)][,SD_Mom31:=SD_Mom31/temp_max]
    
    port[,IQR_Mom:=IQR(Mom_365_30,na.rm = T),by=c('Date')]
    port[,IQR_ARM:=IQR(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,IQR_EQ:=IQR(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,IQR_Vol:=IQR(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,IQR_DY:=IQR(DY,na.rm = T),by=c('Date')]
    port[,IQR_CFY:=IQR(CFY,na.rm = T),by=c('Date')]
    port[,IQR_EY:=IQR(EY,na.rm = T),by=c('Date')]
    port[,IQR_eEY:=IQR(eEY,na.rm = T),by=c('Date')]
    port[,IQR_SAL:=IQR(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_EPS:=IQR(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_Mom31:=IQR(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,K_Mom:=kurtosis(Mom_365_30,na.rm = T),by=c('Date')]
    port[,K_ARM:=kurtosis(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,K_EQ:=kurtosis(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,K_Vol:=kurtosis(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,K_DY:=kurtosis(DY,na.rm = T),by=c('Date')]
    port[,K_CFY:=kurtosis(CFY,na.rm = T),by=c('Date')]
    port[,K_EY:=kurtosis(EY,na.rm = T),by=c('Date')]
    port[,K_eEY:=kurtosis(eEY,na.rm = T),by=c('Date')]
    port[,K_SAL:=kurtosis(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,K_EPS:=kurtosis(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,K_Mom31:=kurtosis(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,S_Mom:=skewness(Mom_365_30,na.rm = T),by=c('Date')]
    port[,S_ARM:=skewness(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,S_EQ:=skewness(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,S_Vol:=skewness(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,S_DY:=skewness(DY,na.rm = T),by=c('Date')]
    port[,S_CFY:=skewness(CFY,na.rm = T),by=c('Date')]
    port[,S_EY:=skewness(EY,na.rm = T),by=c('Date')]
    port[,S_eEY:=skewness(eEY,na.rm = T),by=c('Date')]
    port[,S_SAL:=skewness(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,S_EPS:=skewness(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,S_Mom31:=skewness(Mom_31_0,na.rm = T),by=c('Date')]
    
    og_port=port
    port=unique(port,by=c('Date'))
    setorder(port,Date,ID)
    #Additional inputs to use
    port[,U_Cor_Mom:=shift(Cor_Mom,n=2,typ='lag')]
    port[,U_Cor_ARM:=shift(Cor_ARM,n=2,typ='lag')]
    port[,U_Cor_EQ:=shift(Cor_EQ,n=2,typ='lag')]
    port[,U_Cor_Vol:=shift(Cor_Vol,n=2,typ='lag')]
    port[,U_Cor_DY:=shift(Cor_DY,n=2,typ='lag')]
    port[,U_Cor_CFY:=shift(Cor_CFY,n=2,typ='lag')]
    port[,U_Cor_EY:=shift(Cor_EY,n=2,typ='lag')]
    port[,U_Cor_eEY:=shift(Cor_eEY,n=2,typ='lag')]
    port[,U_Cor_SAL:=shift(Cor_SAL,n=2,typ='lag')]
    port[,U_Cor_EPS:=shift(Cor_EPS,n=2,typ='lag')]
    port[,U_Cor_Mom31:=shift(Cor_Mom31,n=2,typ='lag')]
    port[,U_Value:=shift(Cor_Value,n=2,typ='lag')]
    port[,U_Qual:=shift(Cor_Qual,n=2,typ='lag')]
    
    port[U_Cor_Mom>=0,Last_Mom_Pos:=1]
    port[U_Cor_Mom<=0,Last_Mom_Neg:=1]
    port[,Cor_Mom:=sign(Cor_Mom)]
    #Sending the last out one as the y sample, AKA, what we want to predict on
    if(macro==F){
      in_sample=port[,c('Norm_Mom','Norm_ARM','Norm_EQ','Norm_Vol','Norm_DY','Norm_CFY','Norm_EY','Norm_eEY','Norm_SAL',
                        'Norm_EPS','Norm_Mom31',
                        'SD_Mom','SD_ARM','SD_EQ','SD_Vol','SD_DY','SD_CFY','SD_EY','SD_eEY','SD_SAL',
                        'SD_EPS','SD_Mom31',
                        'IQR_Mom','IQR_ARM','IQR_EQ','IQR_Vol','IQR_DY','IQR_CFY','IQR_EY','IQR_eEY','IQR_SAL',
                        'IQR_EPS','IQR_Mom31',
                        'K_Mom','K_ARM','K_EQ','K_Vol','K_DY','K_CFY','K_EY','K_eEY','K_SAL','K_EPS','K_Mom31',
                        'S_Mom','S_ARM','S_EQ','S_Vol','S_DY','S_CFY','S_EY','S_eEY','S_SAL','S_EPS','S_Mom31',
                        'U_Cor_Mom','U_Cor_ARM','U_Cor_EQ','U_Cor_Vol','U_Cor_DY','U_Cor_CFY','U_Cor_EY',
                        'U_Cor_eEY','U_Cor_SAL','U_Cor_EPS','U_Cor_Mom31','U_Value','U_Momentum','U_Qual')]
    }
    else{
      in_sample=port[,c('SD_Mom','SD_ARM','SD_EQ','SD_Vol','SD_DY','SD_CFY','SD_EY','SD_eEY','SD_SAL',
                        'SD_EPS','SD_Mom31',
                        'U_Cor_Mom','U_Cor_ARM','U_Cor_EQ','U_Cor_DY','U_Cor_CFY','U_Cor_EY',
                        'U_Cor_eEY','U_Cor_SAL','U_Cor_EPS','U_Cor_Mom31','U_Value','U_Qual','Last_Mom_Pos',
                        'Last_Mom_Neg',macro_names),with=F]
    }
    
    out_sample=port[,c('Cor_Mom')]
    Xs=in_sample[U_Cor_Mom==564654654]
    Ys=out_sample[Cor_Mom==5]
    sample=length(unique(port$Date))-step
    for(l in seq(1,sample)){
      print(paste('Doing the first',l,'groups (rolling)'))
      first_in=in_sample[l+seq(0,step),]
      first_out=out_sample[l+c(step),]
      Xs=rbind(Xs,first_in)
      Ys=rbind(Ys,first_out)
    }
    
    x=Xs
    n_col=ncol(x)
    x=as.matrix(x)
    x[is.infinite(x) | is.na(x) | is.nan(x)]=0
    #Ys[,RNN_CLASS:=0][Cor_Mom<0,RNN_CLASS:=1] #Class
    y=as.matrix(Ys$Cor_Mom)
    #y=to_categorical(Ys$RNN_CLASS,num_classes = 2) #Class
    
    
    
    #Time to re-arrange X and Y appropriately (to array)
    X=array(x,dim=c(sample,step,n_col))
    Y=array(y,dim=c(sample,1)) #Class
    
    set.seed(123456)
    tensorflow::use_session_with_seed(seed=123456)
    RNN_Mo<<- keras_model_sequential() %>%
      bidirectional(layer_lstm(units=600,return_sequences = T),input_shape=c(step,n_col)) %>%
      layer_dropout(0.25) %>%
      bidirectional(layer_lstm(units=600,return_sequences = F)) %>%
      layer_dropout(0.25) %>%
      layer_dense(units=1) #Class, activation='softmax'
    RNN_Mo %>% compile(loss = 'mae',optimizer = 'adam')
    filepath="weights.best.hdf5"
    dank_back=callback_model_checkpoint(filepath,monitor='val_loss',verbose=1,save_best_only = T,save_weights_only = T,mode='min',period=3)
    RNN_Mo  %>% fit(X,Y,epochs=150,validation_split=0.20,verbose=1,callbacks=list(dank_back),batch_size=200)
    RNN_Mo %>% load_model_weights_hdf5(filepath = filepath)
    
  }
  
  if('RNN_Class' %in% num_of_algos){
    #Not working as of yet
    set.seed(456)
    require(keras)
    require(tensorflow)
    port=as.data.table(all_data)
    port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY','CFY',
                 'EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                 'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY','N_CFY',
                 'N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth','N_Mom_31_0')]
    setorder(port,Date,ID)
    invisible(lapply(names(port),function(.name) set(port, which(is.infinite(port[[.name]])), j = .name,value =NA)))
    invisible(lapply(names(port),function(.name) set(port, which(is.nan(port[[.name]])), j = .name,value =NA)))
    #Data prep for RNN
    #In sample timestep: 40
    step=seq_size
    step_size<<-step
    port[,Cor_Mom:=cor(Mom_365_30,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_ARM:=cor(ARM_100_REG,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EQ:=cor(EQ_REG_RANK,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Vol:=cor(Volatility_185_0,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_DY:=cor(DY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_CFY:=cor(CFY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EY:=cor(EY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_eEY:=cor(eEY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_SAL:=cor(SAL_eGrowth,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EPS:=cor(EPS_eGrowth,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Mom31:=cor(Mom_31_0,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    
    #Outputs to find
    if(bench[1]=='CA'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    if(bench[1]=='US'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    port[,Cor_Value:=cor(Value,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Momentum:=cor(Mom,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Qual:=cor(Qual,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    
    port[,Long:=0]     #0=Value, 1=Mom, 2=Qual
    port[(Cor_Momentum>Cor_Value) & (Cor_Momentum>Cor_Qual),Long:=1]
    port[(Cor_Qual>Cor_Value) & (Cor_Qual>Cor_Momentum),Long:=2]
    
    port[,Short:=0]     #0=Value, 1=Mom, 2=Qual
    port[(Cor_Momentum<Cor_Value) & (Cor_Momentum<Cor_Qual),Short:=1]
    port[(Cor_Qual<Cor_Value) & (Cor_Qual<Cor_Momentum),Short:=2]
    
    #Normalized inputs
    port[,Norm_Mom:=mean(Mom_365_30,na.rm = T),by=c('Date')]
    port[,Norm_ARM:=mean(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,Norm_EQ:=mean(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,Norm_Vol:=mean(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,Norm_DY:=mean(DY,na.rm = T),by=c('Date')]
    port[,Norm_CFY:=mean(CFY,na.rm = T),by=c('Date')]
    port[,Norm_EY:=mean(EY,na.rm = T),by=c('Date')]
    port[,Norm_eEY:=mean(eEY,na.rm = T),by=c('Date')]
    port[,Norm_SAL:=mean(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,Norm_EPS:=mean(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,Norm_Mom31:=mean(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,SD_Mom:=var(Mom_365_30,na.rm = T),by=c('Date')]
    port[,SD_ARM:=var(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,SD_EQ:=var(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,SD_Vol:=var(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,SD_DY:=var(DY,na.rm = T),by=c('Date')]
    port[,SD_CFY:=var(CFY,na.rm = T),by=c('Date')]
    port[,SD_EY:=var(EY,na.rm = T),by=c('Date')]
    port[,SD_eEY:=var(eEY,na.rm = T),by=c('Date')]
    port[,SD_SAL:=var(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,SD_EPS:=var(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,SD_Mom31:=var(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,IQR_Mom:=IQR(Mom_365_30,na.rm = T),by=c('Date')]
    port[,IQR_ARM:=IQR(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,IQR_EQ:=IQR(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,IQR_Vol:=IQR(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,IQR_DY:=IQR(DY,na.rm = T),by=c('Date')]
    port[,IQR_CFY:=IQR(CFY,na.rm = T),by=c('Date')]
    port[,IQR_EY:=IQR(EY,na.rm = T),by=c('Date')]
    port[,IQR_eEY:=IQR(eEY,na.rm = T),by=c('Date')]
    port[,IQR_SAL:=IQR(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_EPS:=IQR(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_Mom31:=IQR(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,K_Mom:=kurtosis(Mom_365_30,na.rm = T),by=c('Date')]
    port[,K_ARM:=kurtosis(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,K_EQ:=kurtosis(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,K_Vol:=kurtosis(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,K_DY:=kurtosis(DY,na.rm = T),by=c('Date')]
    port[,K_CFY:=kurtosis(CFY,na.rm = T),by=c('Date')]
    port[,K_EY:=kurtosis(EY,na.rm = T),by=c('Date')]
    port[,K_eEY:=kurtosis(eEY,na.rm = T),by=c('Date')]
    port[,K_SAL:=kurtosis(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,K_EPS:=kurtosis(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,K_Mom31:=kurtosis(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,S_Mom:=skewness(Mom_365_30,na.rm = T),by=c('Date')]
    port[,S_ARM:=skewness(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,S_EQ:=skewness(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,S_Vol:=skewness(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,S_DY:=skewness(DY,na.rm = T),by=c('Date')]
    port[,S_CFY:=skewness(CFY,na.rm = T),by=c('Date')]
    port[,S_EY:=skewness(EY,na.rm = T),by=c('Date')]
    port[,S_eEY:=skewness(eEY,na.rm = T),by=c('Date')]
    port[,S_SAL:=skewness(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,S_EPS:=skewness(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,S_Mom31:=skewness(Mom_31_0,na.rm = T),by=c('Date')]
    
    og_port=port
    port=unique(port,by=c('Date'))
    setorder(port,Date,ID)
    #Additional inputs to use
    port[,U_Cor_Mom:=shift(Cor_Mom,n=2,typ='lag')]
    port[,U_Cor_ARM:=shift(Cor_ARM,n=2,typ='lag')]
    port[,U_Cor_EQ:=shift(Cor_EQ,n=2,typ='lag')]
    port[,U_Cor_Vol:=shift(Cor_Vol,n=2,typ='lag')]
    port[,U_Cor_DY:=shift(Cor_DY,n=2,typ='lag')]
    port[,U_Cor_CFY:=shift(Cor_CFY,n=2,typ='lag')]
    port[,U_Cor_EY:=shift(Cor_EY,n=2,typ='lag')]
    port[,U_Cor_eEY:=shift(Cor_eEY,n=2,typ='lag')]
    port[,U_Cor_SAL:=shift(Cor_SAL,n=2,typ='lag')]
    port[,U_Cor_EPS:=shift(Cor_EPS,n=2,typ='lag')]
    port[,U_Cor_Mom31:=shift(Cor_Mom31,n=2,typ='lag')]
    port[,U_Value:=shift(Cor_Value,n=2,typ='lag')]
    port[,U_Momentum:=shift(Cor_Momentum,n=2,typ='lag')]
    port[,U_Qual:=shift(Cor_Qual,n=2,typ='lag')]
    #Sending the last out one as the y sample, AKA, what we want to predict on
    in_sample=port[,c('Norm_Mom','Norm_ARM','Norm_EQ','Norm_Vol','Norm_DY','Norm_CFY','Norm_EY','Norm_eEY','Norm_SAL',
                      'Norm_EPS','Norm_Mom31',
                      'SD_Mom','SD_ARM','SD_EQ','SD_Vol','SD_DY','SD_CFY','SD_EY','SD_eEY','SD_SAL',
                      'SD_EPS','SD_Mom31',
                      'IQR_Mom','IQR_ARM','IQR_EQ','IQR_Vol','IQR_DY','IQR_CFY','IQR_EY','IQR_eEY','IQR_SAL',
                      'IQR_EPS','IQR_Mom31',
                      'K_Mom','K_ARM','K_EQ','K_Vol','K_DY','K_CFY','K_EY','K_eEY','K_SAL','K_EPS','K_Mom31',
                      'S_Mom','S_ARM','S_EQ','S_Vol','S_DY','S_CFY','S_EY','S_eEY','S_SAL','S_EPS','S_Mom31',
                      'U_Cor_Mom','U_Cor_ARM','U_Cor_EQ','U_Cor_Vol','U_Cor_DY','U_Cor_CFY','U_Cor_EY',
                      'U_Cor_eEY','U_Cor_SAL','U_Cor_EPS','U_Cor_Mom31','U_Value','U_Momentum','U_Qual')]
    out_sample=port[,c('Long','Short')]
    Xs=in_sample[Norm_Mom==564654654]
    Ys=out_sample[Long==5]
    sample=length(unique(port$Date))-step
    for(l in seq(1,sample)){
      print(paste('Doing the first',l,'groups (rolling)'))
      first_in=in_sample[l+seq(0,step),]
      first_out=out_sample[l+c(step),]
      Xs=rbind(Xs,first_in)
      Ys=rbind(Ys,first_out)
    }
    
    x=Xs
    n_col=ncol(x)
    x=as.matrix(x)
    x[is.infinite(x) | is.na(x) | is.nan(x)]=0
    y=to_categorical(Ys$Long,num_classes = 3)
    y2=to_categorical(Ys$Short,num_classes = 3)
    
    #Time to re-arrange X and Y appropriately (to array)
    X=array(x,dim=c(sample,step,n_col))
    Y=array(y,dim=c(sample,3))
    Y2=array(y2,dim=c(sample,3))
    
    RNN_Long<<- keras_model_sequential() %>%
      bidirectional(layer_gru(units=10,return_sequences = T),input_shape=c(step,n_col)) %>%
      layer_dropout(0.2) %>%
      bidirectional(layer_gru(units=10,return_sequences = T)) %>%
      layer_dropout(0.2) %>%
      layer_dense(units=3,activation = 'softmax')
    RNN_Long %>% compile(loss = 'categorical_crossentropy',optimizer = 'adam')
    filepath="weights.best.hdf5"
    dank_back=callback_model_checkpoint(filepath,monitor='val_loss',verbose=1,save_best_only = T,save_weights_only = T,mode='min')
    RNN_Long  %>% fit(X,Y,epochs=200,validation_split=0.30,verbose=1,callbacks=list(dank_back))
    RNN_Long %>% load_model_weights_hdf5(filepath = filepath)
    
    RNN_Short<<- keras_model_sequential() %>% 
      bidirectional(layer_gru(units=10,return_sequences = T),input_shape=c(step,n_col)) %>%
      layer_dropout(0.2) %>%
      bidirectional(layer_gru(units=10,return_sequences = T)) %>%
      layer_dropout(0.2) %>%
      layer_dense(units=3,activation='softmax')
    RNN_Short %>% compile(loss = 'categorical_crossentropy',optimizer = 'adam')
    filepath="weights.best.hdf5"
    dank_back=callback_model_checkpoint(filepath,monitor='val_loss',verbose=1,save_best_only = T,save_weights_only = T,mode='min')
    RNN_Short %>% fit(X,Y2,epochs=200,validation_split=0.30,verbose=1,callbacks=list(dank_back))
    RNN_Short %>% load_model_weights_hdf5(filepath = filepath)
    
  }
  
  if('PCA_Regr' %in% num_of_algos){
    print('Starting PCA_Regression')
    set.seed(123456)
    #port=copy(Filter(function(x) !all(is.na(x)|x == 0), all_data))
    port=copy(all_data)
    port=as.data.frame(port)
    #port=port[!duplicated(as.list(port))]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    pca_ed<<-h2o::h2o.prcomp(training_frame = port,k=pca_nums[1],impute_missing = T)
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.table(new_port)
    port$ActiveFwdRet=y_vals
    
    PCA_lin_reg<<-lm(ActiveFwdRet ~ .,data=port)
    port$PCA_Linear_Regression=as.numeric(predict(PCA_lin_reg,port))
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_Linear_Regression')]
    port8=to_100_rank(port,cols_to_apply_to = c('PCA_Linear_Regression'),new_names = c('PCA_Linear_Regression'))
    portfolio8=Portfolio_maker(port8,cols_to_apply_to = 'PCA_Linear_Regression',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio8,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('PCA Regression was successful')
  }
  
  if('PCA_Random_Forest' %in% num_of_algos){
    print('Starting PCA_Random_Forest')
    set.seed(123456)
    port=copy(as.data.table(all_data))
    if('Value' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    #port=copy(all_data)
    port=as.data.frame(port)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    pca_ed<<-h2o::h2o.prcomp(training_frame = port,k=pca_nums[1],impute_missing = T)
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.table(new_port)
    port$ActiveFwdRet=y_vals
    port=h2o::as.h2o(port)
    
    PCA_Random_Forest<<-h2o::h2o.randomForest(y='ActiveFwdRet',training_frame = port,ntrees=700,max_depth=2,
                                              seed=123456,verbose = T
                                              ,stopping_metric = 'MSE'
                                              ,distribution = 'quantile'
                                              ,min_rows = 10,
                                              nbins = 40,nbins_top_level = 1024,sample_rate = 0.6320000291
                                              ,histogram_type = 'QuantilesGlobal',stopping_tolerance = 0.001,
                                              col_sample_rate_per_tree = 0.9)
    preds=h2o::h2o.predict(PCA_Random_Forest,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$PCA_Random_Forest=as.numeric(preds$predict)
    port=as.data.table(port)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_Random_Forest')]
    port9=to_100_rank(port,cols_to_apply_to = c('PCA_Random_Forest'),new_names = c('PCA_Random_Forest'))
    portfolio9=Portfolio_maker(port9,cols_to_apply_to = 'PCA_Random_Forest',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio9,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('PCA_Random_Forest was successful')
  }
  
  if('PCA_GBM' %in% num_of_algos){
    #require(h2o)
    set.seed(123456)
    port=copy(as.data.table(all_data))
    if('Value' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    print('Starting PCA_Gradient Boosted Model')
    #port=copy(all_data)
    port=as.data.frame(port)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    pca_ed<<-h2o::h2o.prcomp(training_frame = port,k=pca_nums[1],impute_missing = T)
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.table(new_port)
    port$ActiveFwdRet=y_vals
    port=h2o::as.h2o(port)
    
    PCA_GBM<<-h2o::h2o.gbm(y='ActiveFwdRet',training_frame = port,ntrees = 700,max_depth = 2,min_rows = 1,
                           nbins=40,seed=123456,learn_rate = 0.2
                           #,distribution ='quantile'
                           ,sample_rate = 0.6320000291,
                           col_sample_rate_per_tree = 0.9,pred_noise_bandwidth = 0,histogram_type = 'QuantilesGlobal',
                           stopping_metric = 'MSE',quantile_alpha = 0.1,nbins_top_level = 1024,verbose = T)
    preds=h2o::h2o.predict(PCA_GBM,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$PCA_GBM=as.numeric(preds$predict)
    port=as.data.table(port)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_GBM')]
    port10=to_100_rank(port,cols_to_apply_to = c('PCA_GBM'),new_names = c('PCA_GBM'))
    portfolio10=Portfolio_maker(port10,cols_to_apply_to = 'PCA_GBM',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio10,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('PCA_Gradient Boosted Model was successful')
  }
  
  if('PCA_FFNN' %in% num_of_algos){
    #require(h2o)
    set.seed(123456)
    print('Starting PCA_FFNN Model')
    #port=copy(Filter(function(x) !all(is.na(x)|x == 0), all_data))
    port=copy(all_data)
    port=as.data.frame(port)
    #port=port[!duplicated(as.list(port))]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    pca_ed<<-h2o::h2o.prcomp(training_frame = port,k=pca_nums[1],impute_missing = T)
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.table(new_port)
    port$ActiveFwdRet=y_vals
    port=h2o::as.h2o(port)
    
    PCA_FFNN<<-h2o::h2o.deeplearning(y='ActiveFwdRet',training_frame = port,distribution = 'quantile')
    preds=h2o::h2o.predict(PCA_FFNN,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$PCA_FFNN=as.numeric(preds$predict)
    port=as.data.table(port)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_FFNN')]
    port11=to_100_rank(port,cols_to_apply_to = c('PCA_FFNN'),new_names = c('PCA_FFNN'))
    portfolio11=Portfolio_maker(port11,cols_to_apply_to = 'PCA_FFNN',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio11,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('PCA_FFNN was successful')
  }
  
  if('PCA_SVM' %in% num_of_algos){
    require(e1071)
    set.seed(123456)
    print('Starting PCA_SVM Model')
    port=copy(all_data)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    pca_ed<<-h2o::h2o.prcomp(training_frame = port,k=pca_nums[1],impute_missing = T)
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.table(new_port)
    port$ActiveFwdRet=y_vals
    
    PCA_SVM<<-svm(ActiveFwdRet~.,data=port,kernel='radial',degree=5,cost=1)
    port$PCA_SVM=as.numeric(predict(PCA_SVM,port))
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_SVM')]
    port12=to_100_rank(port,cols_to_apply_to = c('PCA_SVM'),new_names = c('PCA_SVM'))
    portfolio12=Portfolio_maker(port12,cols_to_apply_to = 'PCA_SVM',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio12,by=c('ID','Date'))
    
    print('PCA_SVM was successful')
  }
  
  if('PCA_KNN' %in% num_of_algos){
    require(caret)
    set.seed(123456)
    print('Starting PCA_KNN')
    port=copy(all_data)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    port=as.data.frame(port)
    port=h2o::as.h2o(port)

    pca_ed<<-h2o::h2o.prcomp(training_frame = port,k=pca_nums[1])
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.frame(new_port)
    port$ActiveFwdRet=y_vals
    
    PCA_KNN<<-train(ActiveFwdRet~.,data=port,method='knn',k=5)
    preds=predict(PCA_KNN,port)
    port$PCA_KNN=as.numeric(preds)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_KNN')]
    port13=to_100_rank(port,cols_to_apply_to = c('PCA_KNN'),new_names = c('PCA_KNN'))
    portfolio13=Portfolio_maker(port13,cols_to_apply_to = 'PCA_KNN',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio13,by=c('ID','Date'))
    
    print('PCA_KNN was successful')
  }
  
  if('XGBM' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting XGBM Model')
    port=copy(as.data.table(all_data))
    if('Value' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                     'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                     'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                     'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                     'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                     'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                     'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                     'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                     'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60',macro_names),with=F]
      }

    }
    if('Quality' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                     'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                     'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                     'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                     'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60',macro_names),with=F]
      }

    }
    if('Momentum' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                     'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                     'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                     'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                     'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                     'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                     'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                     'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                     'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                     'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                     'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                     'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                     'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                     'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                     'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                     'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                     'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                     'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                     'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                     'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                     'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                     'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                     'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                     'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                     'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                     'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                     'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('SI' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c(names(SI_stuff),'ActiveFwdRet',
                     'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60'),with=F]
      }
      else{
        port=port[,c(names(SI_stuff),'ActiveFwdRet',
                     'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60',macro_names),with=F]
      }

    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS','ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
                   )]
    }
    if('All' %in% only & only_bench==T){
      setorder(port,Date,ID)
      port=port[,c('ID','Date','ActiveFwdRet',
                   'Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY',
                   'CFY','EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
      
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    setorder(port,Date,ID)
    testa_port=copy(port)
    testa_port[,entries:=1]
    testa_port[,entries_per_date:=sum(entries),by=c('Date')]
    testa_port=unique(testa_port,by=c('Date'))[,c('Date','entries_per_date')]
    
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    og_port=port
    port=as.matrix(port)
    port[is.infinite(port) | is.na(port) | is.nan(port)]=0
    y_vals=as.matrix(y_vals)
    port=xgb.DMatrix(data=port,label=y_vals,group=c(testa_port$entries_per_date)
                     )
    
    if('All' %in% GICS){
      XGBM<<-xgb.train(data=port,label=y_vals,nrounds = iter,verbose = 2,max_depth=dep,eta=0.7,gamma=0.1
                       ,min_child_weight=1,max_delta_step=1,subsample=0.8,colsample_bytree=0.8,num_feature=feats,
                       colsample_bylevel=0.8,colsample_bynode=0.8,alpha=alp,tree_method='hist'
                       ,eval_metric='ndcg',objective=obj[1],lambda=lamb
                       ,num_pairsample=samp
                       ,watchlist = list(train=port))
      preds=predict(XGBM,newdata = port)
      port=og_port
      port$XGBM=preds
      port$ActiveFwdRet=y_vals
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = maker,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
      
    }
    else{
      var_name=paste('XGBM_',j,sep='')
      print(paste('starting',var_name))
      assign(var_name,xgb.train(data=port,label=y_vals,nrounds = 1500,verbose = 2,max_depth=1,eta=0.7,gamma=0.1
                       ,min_child_weight=1,max_delta_step=1,subsample=0.8,colsample_bytree=0.8,
                       colsample_bylevel=0.8,colsample_bynode=0.8,alpha=1
                       ,eval_metric='map',objective='rank:pairwise'#,lambda=1999000
                       ,watchlist = list(train=port)),envir=.GlobalEnv)
      
      preds=predict(eval(parse(text=var_name)),newdata = port)
      port=og_port
      port$XGBM=preds
      port$ActiveFwdRet=y_vals
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = 20,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
    }

  }
  
  if('XGBM2' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting XGBM2 Model')
    port=copy(as.data.table(all_data))
    if('Value' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only & only_bench==F){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS','ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
      )]
    }
    if('All' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY',
                   'CFY','EY','eEY','SAL_eGrowth','EPS_eGrowth',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    setorder(port,Date,ID)
    testa_port=copy(port)
    testa_port[,entries:=1]
    testa_port[,entries_per_date:=sum(entries),by=c('Date')]
    testa_port=unique(testa_port,by=c('Date'))[,c('Date','entries_per_date')]
    
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    og_port=port
    port=as.matrix(port)
    y_vals=as.matrix(y_vals)
    port=xgb.DMatrix(data=port,label=y_vals,group=c(testa_port$entries_per_date))
    
    if('All' %in% GICS){
      XGBM2<<-xgb.train(data=port,label=y_vals,nrounds = 1500,verbose = 2,max_depth=dep+1,eta=0.7,gamma=0.1
                       ,min_child_weight=1,max_delta_step=1,subsample=0.8,colsample_bytree=0.8,
                       colsample_bylevel=0.8,colsample_bynode=0.8,alpha=1,tree_method='hist'
                       ,eval_metric='ndcg',objective=obj[1],lambda=50000,num_pairsample=samp
                       ,watchlist = list(train=port))
      preds=predict(XGBM2,newdata = port)
      port=og_port
      port$XGBM2=preds
      port$ActiveFwdRet=y_vals
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM2')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM2'),new_names = c('XGBM2'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM2',top_bot_pct = maker,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
      
    }
    else{
      var_name=paste('XGBM_',j,sep='')
      print(paste('starting',var_name))
      assign(var_name,xgb.train(data=port,label=y_vals,nrounds = 1500,verbose = 2,max_depth=1,eta=0.7,gamma=0.1
                                ,min_child_weight=1,max_delta_step=1,subsample=0.8,colsample_bytree=0.8,
                                colsample_bylevel=0.8,colsample_bynode=0.8,alpha=1
                                ,eval_metric='map',objective='rank:pairwise'#,lambda=1999000
                                ,watchlist = list(train=port)),envir=.GlobalEnv)
      
      preds=predict(eval(parse(text=var_name)),newdata = port)
      port=og_port
      port$XGBM=preds
      port$ActiveFwdRet=y_vals
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = 20,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
    }
    
  }
  
  if('XGBMN' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting XGBM2 Model')
    port=copy(as.data.table(all_data))
    if('Value' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only & only_bench==F){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS','ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
      )]
    }
    if('All' %in% only & only_bench==T){
      port=port[,c('ID','Date','N_ActiveFwdRet',
                   'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY',
                   'N_CFY','N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
      port[,ActiveFwdRet:=N_ActiveFwdRet]
      port[,N_ActiveFwdRet:=NULL]
      
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    setorder(port,Date,ID)
    testa_port=copy(port)
    testa_port[,entries:=1]
    testa_port[,entries_per_date:=sum(entries),by=c('Date')]
    testa_port=unique(testa_port,by=c('Date'))[,c('Date','entries_per_date')]
    
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    og_port=port
    port=as.matrix(port)
    y_vals=as.matrix(y_vals)
    port=xgb.DMatrix(data=port,label=y_vals,group=c(testa_port$entries_per_date))
    
    if('All' %in% GICS){
      XGBMN<<-xgb.train(data=port,label=y_vals,nrounds = 1500,verbose = 2,max_depth=dep,eta=0.7,gamma=0.1
                        ,min_child_weight=1,max_delta_step=1,subsample=0.8,colsample_bytree=0.8,
                        colsample_bylevel=0.8,colsample_bynode=0.8,alpha=1,tree_method='hist'
                        ,eval_metric='ndcg',objective=obj[1],lambda=50000,num_pairsample=samp
                        ,watchlist = list(train=port))
      preds=predict(XGBMN,newdata = port)
      port=og_port
      port$XGBMN=preds
      port$ActiveFwdRet=y_vals
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBMN')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBMN'),new_names = c('XGBMN'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBMN',top_bot_pct = maker,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBMN was successful')
      
    }
    else{
      var_name=paste('XGBM_',j,sep='')
      print(paste('starting',var_name))
      assign(var_name,xgb.train(data=port,label=y_vals,nrounds = 1500,verbose = 2,max_depth=1,eta=0.7,gamma=0.1
                                ,min_child_weight=1,max_delta_step=1,subsample=0.8,colsample_bytree=0.8,
                                colsample_bylevel=0.8,colsample_bynode=0.8,alpha=1
                                ,eval_metric='map',objective='rank:pairwise'#,lambda=1999000
                                ,watchlist = list(train=port)),envir=.GlobalEnv)
      
      preds=predict(eval(parse(text=var_name)),newdata = port)
      port=og_port
      port$XGBM=preds
      port$ActiveFwdRet=y_vals
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = 20,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
    }
    
  }
  
  if('XGBMS' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting XGBM Model')
    port=copy(as.data.table(all_data))
    if('Value' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==F){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only & only_bench==F){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS','ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
      )]
    }
    if('All' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY',
                   'CFY','EY','eEY','SAL_eGrowth','EPS_eGrowth',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
      port=port[,ActiveFwdRet:=-1*ActiveFwdRet]
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    setorder(port,Date,ID)
    testa_port=copy(port)
    testa_port[,entries:=1]
    testa_port[,entries_per_date:=sum(entries),by=c('Date')]
    testa_port=unique(testa_port,by=c('Date'))[,c('Date','entries_per_date')]
    
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    og_port=port
    port=as.matrix(port)
    y_vals=as.matrix(y_vals)
    port=xgb.DMatrix(data=port,label=y_vals,group=c(testa_port$entries_per_date))
    
    if('All' %in% GICS){
      XGBMS<<-xgb.train(data=port,label=y_vals,nrounds = 1500,verbose = 2,max_depth=dep,eta=0.7,gamma=0.1
                       ,min_child_weight=1,max_delta_step=1,subsample=0.8,colsample_bytree=0.8,
                       colsample_bylevel=0.8,colsample_bynode=0.8,alpha=1,tree_method='hist'
                       ,eval_metric='map',objective=obj[1],lambda=50000
                       ,watchlist = list(train=port))
      preds=predict(XGBMS,newdata = port)
      port=og_port
      port$XGBMS=preds
      port$ActiveFwdRet=y_vals
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBMS')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBMS'),new_names = c('XGBMS'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBMS',top_bot_pct = maker,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBMS was successful')
      
    }
    else{
      var_name=paste('XGBM_',j,sep='')
      print(paste('starting',var_name))
      assign(var_name,xgb.train(data=port,label=y_vals,nrounds = 1500,verbose = 2,max_depth=1,eta=0.7,gamma=0.1
                                ,min_child_weight=1,max_delta_step=1,subsample=0.8,colsample_bytree=0.8,
                                colsample_bylevel=0.8,colsample_bynode=0.8,alpha=1
                                ,eval_metric='map',objective='rank:pairwise'#,lambda=1999000
                                ,watchlist = list(train=port)),envir=.GlobalEnv)
      
      preds=predict(eval(parse(text=var_name)),newdata = port)
      port=og_port
      port$XGBM=preds
      port$ActiveFwdRet=y_vals
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = 20,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
    }
    
  }
  
  if('PCA_XGBM' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting PCA_XGBM Model')
    port=copy(all_data)
    port=as.data.frame(port)
    port=port[!duplicated(as.list(port))]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    pca_ed<<-h2o::h2o.prcomp(training_frame = port,k=pca_nums[1],impute_missing = T)
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.matrix(new_port)
    
    
    PCA_XGBM<<-xgboost(data=port,label=y_vals,nrounds = 5000)
    preds=predict(PCA_XGBM,port)
    port=as.data.table(port)
    port$PCA_XGBM=as.numeric(preds)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_XGBM')]
    port15=to_100_rank(port,cols_to_apply_to = c('PCA_XGBM'),new_names = c('PCA_XGBM'))
    portfolio15=Portfolio_maker(port15,cols_to_apply_to = 'PCA_XGBM',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio15,by=c('ID','Date'))
    
    print('PCA_XGBM was successful')
  }
  
  if('GXGBM' %in% num_of_algos){
    port123=copy(portfolio)
    port123[,GXGBM:=(GBM*0.3)+(XGBM*0.7)]
    port123=port123[,c('ID','Date','GXGBM')]
    port15=to_100_rank(port123,cols_to_apply_to = c('GXGBM'),new_names = c('GXGBM'))
    portfolio15=Portfolio_maker(port15,cols_to_apply_to = 'GXGBM',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio15,by=c('ID','Date'))
    
    print('GXGBM was successful')
    
    
  }
  
  if('Cart' %in% num_of_algos){
    set.seed(123456)
    print('Starting Cart Model')
    port=copy(as.data.table(all_data))
    if('Value' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),'ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    if('SI_prune' %in% only){
      port=port[,c('ID','Date','ActiveUtilisation','ActiveFwdRet')]
    }
    #port[ActiveFwdRet>quantile(ActiveFwdRet,0.8) | ActiveFwdRet<quantile(ActiveFwdRet,0.2),Quantile_maker:=1,by=c('Date')]
    #port=port[Quantile_maker==1]
    #port[,Quantile_maker:=NULL]
    setorder(port,Date,ID)
    testa_port=copy(port)
    require(rpart)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    input<<-port
    Cart<<-rpart(ActiveFwdRet~.,data=port,method='class',control=rpart.control(maxdepth = 4,cp=0))
    preds=predict(Cart,newdata = port)
    port=testa_port
    port$Cart=preds
    port=cbind(ids,port)
    abc=port[,c('ID','Date','Cart')]
    port1456=to_100_rank(port,cols_to_apply_to = c('Cart'),new_names = c('Cart'))
    portfolio1456=Portfolio_maker(port1456,cols_to_apply_to = 'Cart',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio1456,by=c('ID','Date'))
    print('Cart plotting was successful')
    plot(Cart,main=paste('Using Cart to see whats going on',train_EDATE))
    text(Cart,digits=3)
    
  }
  
  if('XCart' %in% num_of_algos){
    port123=copy(portfolio)
    port123=port123[,c('ID','Date','XGBM')]
    set.seed(123456)
    print('Starting Cart Model (run XGBM first though)')
    port=copy(as.data.table(all_data))
    if('Value' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                     'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                     'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                     'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                     'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','ActiveFwdRet','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                     'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                     'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                     'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                     'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('Quality' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                     'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                     'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','ActiveFwdRet','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                     'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                     'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('Momentum' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                     'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                     'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                     'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                     'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                     'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                     'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                     'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                     'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                     'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                     'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                     'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                     'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                     'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','ActiveFwdRet','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                     'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                     'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                     'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                     'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                     'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                     'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                     'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                     'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                     'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                     'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                     'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                     'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                     'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('SI' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c(names(SI_stuff),'ActiveFwdRet',
                     'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60'),with=F]
      }
      else{
        port=port[,c(names(SI_stuff),'ActiveFwdRet',
                     'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS','ActiveFwdRet',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
      )]
    }
    if('All' %in% only & only_bench==T){
      setorder(port,Date,ID)
      port=port[,c('ID','Date','ActiveFwdRet',
                   'Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY',
                   'CFY','EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
      
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','ActiveFwdRet','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    #port[ActiveFwdRet>quantile(ActiveFwdRet,0.8) | ActiveFwdRet<quantile(ActiveFwdRet,0.2),Quantile_maker:=1,by=c('Date')]
    #port=port[Quantile_maker==1]
    #port[,Quantile_maker:=NULL]
    setorder(port,Date,ID)
    port=merge(port123,port,by=c('ID','Date'))
    require(rpart)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL,ActiveFwdRet=NULL)]
    port[is.na(port)]=0
    
    Cart<<-rpart(XGBM~.,data=port,method='anova',control=rpart.control(minsplit=1,minbucket = 500,maxdepth = dep+3,cp=0))
    print('Cart plotting was successful')
    plot(Cart,main='Explaining XGBM via Cart')
    text(Cart,digits=3)
  }
  
  if(harmony==T){
    port=portfolio
    port[,Harmony:=(0.5*XGBM)+(0.5*Basic_Mean),]
    port=port[,c('ID','Date','Harmony')]
    port=to_100_rank(port,cols_to_apply_to = c('Harmony'),new_names = c('Harmony'))
    portfolio1233=Portfolio_maker(port,cols_to_apply_to = 'Harmony',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio1233,by=c('ID','Date'))
    
    print('Algorithms are harmonized')
    
  }
  
  return(portfolio)
}


ML_Builder_test=function(data=Factors_Test,num_of_algos=c(''),only=c('All','Value','Quality','Momentum','SI'),
                         bench=c('US','CA'),GICS=c('All'),only_bench=F,maker=20,harmony=F,macro=F){
  set.seed(123456)
  print('Starting Test Set')
  print(paste('Starting:',GICS))
  #localh2o=h2o::h2o.init()
  dat=copy(data)
  all_data=dat
  #train_names=names(all_data)[3:(length(names(all_data)))]
  print('Now ranking the mini factors from 0 to 100')
  all_data=all_data[,!c('i.LclPrice','i.LclOpnPrice','i.Shares','i.USD_FX','i.Price','i.Volume','i.AcutalValue_Q1',
                        'i.AcutalValue_Q1.1','i.AcutalValue_Q1.2','EQUITY','AcutalValue_Q1','i.LQA_Div','i.Vol_Median',
                        'Dividend','i.Dividend','i.MDV','i.MarketCap','i.TotRet','i.SO_Chg','ST_DEBT','LT_DEBT','BuyPct',
                        'HoldPct','SellPct',
                        'NOA','FCF','EVshr','BPS')]
  #Feature engineering
  print('Doing feature engineering')
  #Normalizing DT here
  all_them_cols=names(all_data)[3:(length(names(all_data)))]
  #all_data=calc_CSnorm_dt(all_data,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
  #                                        N_Volatility_185_0='Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',N_Mom_31_0='Mom_31_0',
  #                                        N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth')
  #                    ,grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
  #all_data[,i.PayFreq:=as.numeric(i.PayFreq)]
  #all_data=calc_CSnorm_dt(all_data,var_lst = c(all_them_cols),grp_col = c('Date'),InfNA = T,posttext = '_N',zscore = F,rnk=T,verbose = T)
  require(fastDummies)
  if('All' %in% GICS){
  } else{
    new_filter=paste('L1_',GICS[1],sep='')
    all_data=all_data[get(new_filter)==1]
  }
  print('passed new_filter')
  
  #Factors_Test1<<-all_data
  #num_entries<<-nrow(Factors_Test1)/12
  setorder(all_data,Date,ID)
  
  port=copy(all_data)
  port=calc_CSnorm_dt(port,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
                                     N_Neg_Volatility='-Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',
                                     N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth')
                 ,grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
  #CA Benchmark
  if(bench[1]=='CA' & 'All' %in% only){
    port[,Basic_Mean:=(1/3*((N_Mom_365_30+N_ARM_100_REG)/2)+1/3*((N_DY+N_CFY+N_EY+N_eEY)/4)
                       +1/3*((N_EQ_REG_RANK+N_Neg_Volatility)/2)),by=c('ID','Date')]
  }
  if(bench[1]=='CA' & 'Value' %in% only){
    port[,Basic_Mean:=((0.05*N_DY+0.05*N_CFY+0.05*N_EY+0.05*N_eEY)/4),by=c('ID','Date')]
  }
  if(bench[1]=='CA' & 'Quality' %in% only){
    port[,Basic_Mean:=((0.1*N_EQ_REG_RANK+0.1*N_Neg_Volatility)/2),by=c('ID','Date')]
  }
  if(bench[1]=='CA' & 'Momentum' %in% only){
    port[,Basic_Mean:=((0.25*N_Mom_365_30+0.25*N_ARM_100_REG)/2),by=c('ID','Date')]
  }
  #US Benchmark
  if(bench[1]=='US' & 'All' %in% only){
    port[,Basic_Mean:=((1/3)*((2*N_ARM_100_REG+2*N_Mom_365_30+N_SAL_eGrowth+N_EPS_eGrowth)/6)
                       +(1/3)*((N_DY+N_CFY+N_EY+N_eEY)/4)
                       +(1/3)*((N_EQ_REG_RANK+N_Neg_Volatility)/2)),by=c('ID','Date')]
  }
  if(bench[1]=='US' & 'Value' %in% only){
    port[,Basic_Mean:=((1/12)*N_DY+(1/12)*N_CFY+(1/12)*N_EY+(1/12)*N_eEY),by=c('ID','Date')]
  }
  if(bench[1]=='US' & 'Quality' %in% only){
    port[,Basic_Mean:=((1/3)*(N_EQ_REG_RANK+N_Neg_Volatility)),by=c('ID','Date')]
  }
  if(bench[1]=='US' & 'Momentum' %in% only){
    port[,Basic_Mean:=((1/9)*N_ARM_100_REG+(1/9)*N_Mom_365_30+(1/18)*N_SAL_eGrowth+(1/18)*N_EPS_eGrowth)
         ,by=c('ID','Date')]
  }
  #ALL
  if('SI' %in% only){
    port[,Basic_Mean:=(-1*Utilisation),by=c('ID','Date')]
  }
  if('SI_prune' %in% only){
    port[,Basic_Mean:=(-1*ActiveUtilisation),by=c('ID','Date')]
  }
  
  setorder(port,Date,ID)
  port=to_100_rank(port,cols_to_apply_to = c('Basic_Mean'),new_names = c('Basic_Mean'))
  portfolio=Portfolio_maker(port,cols_to_apply_to = 'Basic_Mean',top_bot_pct = 20,pds=test_pds)
  print('Basic Benchmark Ready')
  
  port123=copy(all_data)
  port123[,Basic_Mo:=Mom_365_30]
  port123=to_100_rank(port123,cols_to_apply_to = c('Basic_Mo'),new_names = c('Basic_Mo'))
  portfolio123=Portfolio_maker(port123,cols_to_apply_to = 'Basic_Mo',top_bot_pct = 20,pds=train_pds)
  portfolio=merge(portfolio,portfolio123,by=c('ID','Date'))
  print('Basic Momentum Ready')
  if('Lin_Reg' %in% num_of_algos){
    set.seed(123456)
    print('Starting Linear Regression')
    port=copy(all_data)
    port=as.data.table(port)
    port[is.na(port)]=0

    port$Linear_Regression=predict(lin_reg,port)
    
    port[is.na(Linear_Regression),Linear_Regression:=weighted.mean(
      c(EV_per_acutal, Price_per_acutal, i.Price_per_acutal, 
        i.Price_per_acutal.1, ASSETS, CASH, COGS, LT_DEBT, ST_DEBT, 
        EQUITY, DEBT_TO_EQUITY_RATIO, Consensus, Growth, i.Growth, 
        i.Consensus, MINORITY_INTEREST, PREFERRED_STOCK, AcutalValue_FY1, 
        i.AcutalValue_FY1, i.AcutalValue_FY1.1, i.AcutalValue_FY1.2, 
        i.AcutalValue_FY1.3, i.AcutalValue_FY1.4, i.AcutalValue_FY1.5, 
        i.SurpriseMean_FY1.5, i.AcutalValue_FY1.6, GICS_Industry_Ratio, 
        i.GICS_Industry_Ratio, i.GICS_Industry_Ratio.2, 
        GICS_Sector_Ratio, i.GICS_Sector_Ratio, i.GICS_Sector_Ratio.2, 
        BuyPct, HoldPct, SellPct, BuyPct_30_days, HoldPct_30_days, 
        SellPct_30_days, BuyPct_60_days, HoldPct_60_days, SellPct_60_days, 
        BuyPct_90_days, HoldPct_90_days, SellPct_90_days, Mom_365_30, 
        Mom_365_182, Mom_730_30, Mom_730_365, AcutalValue_Q1, i.AcutalValue_Q1, 
        i.AcutalValue_Q1.1, i.AcutalValue_Q1.2, i.AcutalValue_Q1.3, 
        i.AcutalValue_Q1.4, i.AcutalValue_Q1.5, Ratio, i.EV_per_acutal, 
        i.GICS_Industry_Ratio.3, i.GICS_Industry_Ratio.4, i.GICS_Industry_Ratio.6, 
        i.GICS_Industry_Ratio.7, i.GICS_Industry_Ratio.8, i.GICS_Industry_Ratio.10, 
        i.Price_per_acutal.2, i.Price_per_acutal.3, i.Price_per_acutal.4, 
        estEST1, estEST2, estEST3, estEST4, estEST5, estEST6, estEST7, 
        estEST8, estEST9, estEST14, estEST15, estEST16, estEST17, 
        estEST18, estEST19, estEST20, estEST21, estEST22, estEST23, 
        estEST24, estEST25, estEST30, estEST31, estEST32, estEST33, 
        estEST34, estEST35, estEST36, estEST37, estEST38, estEST41, 
        estEST42, estEST43, estEST44, estEST45, estEST46, estEST48, 
        estEST49, estEST53, estEST54, estEST55, estEST56, 
        estEST57, estEST58, lagged, increase, i.lagged, i.increase, 
        i.lagged.1, i.increase.1, estEST61, i.lagged.2, i.increase.2, 
        i.lagged.3, i.increase.3, i.lagged.4, i.increase.4, estEST64, 
        i.lagged.5, i.increase.5, i.lagged.6)
      ,w=c(lin_reg$coefficients[2:29],lin_reg$coefficients[31:106],lin_reg$coefficients[108:130]),na.rm = T),by=c('ID','Date')]
    abc=port[,c('ID','Date','Linear_Regression')]
    port1=to_100_rank(port,cols_to_apply_to = c('Linear_Regression'),new_names = c('Linear_Regression'))
    portfolio1=Portfolio_maker(port1,cols_to_apply_to = 'Linear_Regression',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio1,by=c('ID','Date'))
    
    print('lin reg was successful')
    
  }
  
  if('Elastic_Net' %in% num_of_algos){
    print('Starting Elastic Net')
    set.seed(123456)
    require(MASS)
    port=copy(all_data)
    port=as.data.table(port)

    port[,Elastic_Net:=weighted.mean(
      c(EV_per_acutal, Price_per_acutal, i.Price_per_acutal, 
        i.Price_per_acutal.1, ASSETS, CASH, COGS, LT_DEBT, ST_DEBT, 
        EQUITY, DEBT_TO_EQUITY_RATIO, Consensus, Growth, i.Growth, 
        i.Consensus, MINORITY_INTEREST, PREFERRED_STOCK, AcutalValue_FY1, 
        i.AcutalValue_FY1, i.AcutalValue_FY1.1, i.AcutalValue_FY1.2, 
        i.AcutalValue_FY1.3, i.AcutalValue_FY1.4, i.AcutalValue_FY1.5, 
        i.SurpriseMean_FY1.5, i.AcutalValue_FY1.6, GICS_Industry_Ratio, 
        i.GICS_Industry_Ratio, i.GICS_Industry_Ratio.2, 
        GICS_Sector_Ratio, i.GICS_Sector_Ratio, i.GICS_Sector_Ratio.2, 
        BuyPct, HoldPct, SellPct, BuyPct_30_days, HoldPct_30_days, 
        SellPct_30_days, BuyPct_60_days, HoldPct_60_days, SellPct_60_days, 
        BuyPct_90_days, HoldPct_90_days, SellPct_90_days, Mom_365_30, 
        Mom_365_182, Mom_730_30, Mom_730_365, AcutalValue_Q1, i.AcutalValue_Q1, 
        i.AcutalValue_Q1.1, i.AcutalValue_Q1.2, i.AcutalValue_Q1.3, 
        i.AcutalValue_Q1.4, i.AcutalValue_Q1.5, Ratio, i.EV_per_acutal, 
        i.GICS_Industry_Ratio.3, i.GICS_Industry_Ratio.4, i.GICS_Industry_Ratio.6, 
        i.GICS_Industry_Ratio.7, i.GICS_Industry_Ratio.8, i.GICS_Industry_Ratio.10, 
        i.Price_per_acutal.2, i.Price_per_acutal.3, i.Price_per_acutal.4, 
        estEST1, estEST2, estEST3, estEST4, estEST5, estEST6, estEST7, 
        estEST8, estEST9, estEST14, estEST15, estEST16, estEST17, 
        estEST18, estEST19, estEST20, estEST21, estEST22, estEST23, 
        estEST24, estEST25, estEST30, estEST31, estEST32, estEST33, 
        estEST34, estEST35, estEST36, estEST37, estEST38, estEST41, 
        estEST42, estEST43, estEST44, estEST45, estEST46, estEST48, 
        estEST49, estEST53, estEST54, estEST55, estEST56, 
        estEST57, estEST58, lagged, increase, i.lagged, i.increase, 
        i.lagged.1, i.increase.1, estEST61, i.lagged.2, i.increase.2, 
        i.lagged.3, i.increase.3, i.lagged.4, i.increase.4, estEST64, 
        i.lagged.5, i.increase.5, i.lagged.6)
      ,w=c(Elastic_Net$coef[2:29],Elastic_Net$coef[31:106],Elastic_Net$coef[108:130]),na.rm = T),by=c('ID','Date')]
    abc=port[,c('ID','Date','Elastic_Net')]
    port2=to_100_rank(port,cols_to_apply_to = c('Elastic_Net'),new_names = c('Elastic_Net'))
    portfolio2=Portfolio_maker(port2,cols_to_apply_to = 'Elastic_Net',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio2,by=c('ID','Date'))
    
    print('Elastic Net was successful')
  }
  
  if('Random_Forest' %in% num_of_algos){
    print('Starting Random Forest')
    set.seed(123456)
    port=copy(all_data)
    if('Value' %in% only){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    
    #port$Random_Forest=h2o::h2o.predict(Random_Forest,newdata = port)
    preds=h2o::h2o.predict(Random_Forest,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$Random_Forest=as.numeric(preds$predict)
    
    port=cbind(ids,port)
    abc=port[,c('ID','Date','Random_Forest')]
    port3=to_100_rank(port,cols_to_apply_to = c('Random_Forest'),new_names = c('Random_Forest'))
    portfolio3=Portfolio_maker(port3,cols_to_apply_to = 'Random_Forest',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio3,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('Random Forest was successful')
  }
  
  if('GBM' %in% num_of_algos){
    set.seed(123456)
    port=copy(all_data)
    if('Value' %in% only){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    print('Starting Gradient Boosted Model')
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    
    preds=h2o::h2o.predict(GBM,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$GBM=as.numeric(preds$predict)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','GBM')]
    port4=to_100_rank(port,cols_to_apply_to = c('GBM'),new_names = c('GBM'))
    portfolio4=Portfolio_maker(port4,cols_to_apply_to = 'GBM',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio4,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('Gradient Boosted Model was successful')
  }
  
  if('FFNN' %in% num_of_algos){
    #require(h2o)
    set.seed(123456)
    port=copy(all_data)
    if('Value' %in% only){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    print('Starting Feed Forward NN Model')
    #port=copy(all_data)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    
    preds=h2o::h2o.predict(FFNN,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$FFNN=as.numeric(preds$predict)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','FFNN')]
    port5=to_100_rank(port,cols_to_apply_to = c('FFNN'),new_names = c('FFNN'))
    portfolio5=Portfolio_maker(port5,cols_to_apply_to = 'FFNN',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio5,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('Feed Forward NN was successful')
  }
  
  if('SVM' %in% num_of_algos){
    require(e1071)
    set.seed(123456)
    print('Starting SVM')
    port=copy(all_data)
    port=as.data.frame(port)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    
    
    port$SVM=predict(SVM,port)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','SVM')]
    port6=to_100_rank(port,cols_to_apply_to = c('SVM'),new_names = c('SVM'))
    portfolio6=Portfolio_maker(port6,cols_to_apply_to = 'SVM',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio6,by=c('ID','Date'))
    
  }
  
  if('RNN' %in% num_of_algos){
    #Not working as of yet
    set.seed(123456)
    require(keras)
    require(tensorflow)
    all_d=calc_CSnorm_dt(all_d,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
                                              N_Volatility_185_0='Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',N_Mom_31_0='Mom_31_0',
                                              N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth'
    )
    ,grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
    port_edate_original=as.data.table(all_d)
    setorder(port_edate_original)
    port=as.data.table(All_Factors)
    
    port=port[,c('ID','Date','Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY','CFY',
                 'EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                 'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY','N_CFY',
                 'N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth','N_Mom_31_0')]
    setorder(port,Date,ID)
    invisible(lapply(names(port),function(.name) set(port, which(is.infinite(port[[.name]])), j = .name,value =NA)))
    invisible(lapply(names(port),function(.name) set(port, which(is.nan(port[[.name]])), j = .name,value =NA)))
    #Data prep for RNN
    port=merge(port,test_active1,by=c('ID','Date'))
    port[,Cor_Mom:=cor(Mom_365_30,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_ARM:=cor(ARM_100_REG,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EQ:=cor(EQ_REG_RANK,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Vol:=cor(Volatility_185_0,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_DY:=cor(DY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_CFY:=cor(CFY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EY:=cor(EY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_eEY:=cor(eEY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_SAL:=cor(SAL_eGrowth,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EPS:=cor(EPS_eGrowth,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Mom31:=cor(Mom_31_0,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    
    #Outputs to find
    if(bench[1]=='CA'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    if(bench[1]=='US'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    port[,Cor_Value:=cor(Value,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Momentum:=cor(Mom,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Qual:=cor(Qual,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    
    #Normalized inputs
    port[,Norm_Mom:=mean(Mom_365_30,na.rm = T),by=c('Date')]
    port[,Norm_ARM:=mean(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,Norm_EQ:=mean(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,Norm_Vol:=mean(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,Norm_DY:=mean(DY,na.rm = T),by=c('Date')]
    port[,Norm_CFY:=mean(CFY,na.rm = T),by=c('Date')]
    port[,Norm_EY:=mean(EY,na.rm = T),by=c('Date')]
    port[,Norm_eEY:=mean(eEY,na.rm = T),by=c('Date')]
    port[,Norm_SAL:=mean(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,Norm_EPS:=mean(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,Norm_Mom31:=mean(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,SD_Mom:=var(Mom_365_30,na.rm = T),by=c('Date')]
    port[,SD_ARM:=var(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,SD_EQ:=var(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,SD_Vol:=var(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,SD_DY:=var(DY,na.rm = T),by=c('Date')]
    port[,SD_CFY:=var(CFY,na.rm = T),by=c('Date')]
    port[,SD_EY:=var(EY,na.rm = T),by=c('Date')]
    port[,SD_eEY:=var(eEY,na.rm = T),by=c('Date')]
    port[,SD_SAL:=var(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,SD_EPS:=var(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,SD_Mom31:=var(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,IQR_Mom:=IQR(Mom_365_30,na.rm = T),by=c('Date')]
    port[,IQR_ARM:=IQR(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,IQR_EQ:=IQR(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,IQR_Vol:=IQR(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,IQR_DY:=IQR(DY,na.rm = T),by=c('Date')]
    port[,IQR_CFY:=IQR(CFY,na.rm = T),by=c('Date')]
    port[,IQR_EY:=IQR(EY,na.rm = T),by=c('Date')]
    port[,IQR_eEY:=IQR(eEY,na.rm = T),by=c('Date')]
    port[,IQR_SAL:=IQR(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_EPS:=IQR(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_Mom31:=IQR(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,K_Mom:=kurtosis(Mom_365_30,na.rm = T),by=c('Date')]
    port[,K_ARM:=kurtosis(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,K_EQ:=kurtosis(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,K_Vol:=kurtosis(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,K_DY:=kurtosis(DY,na.rm = T),by=c('Date')]
    port[,K_CFY:=kurtosis(CFY,na.rm = T),by=c('Date')]
    port[,K_EY:=kurtosis(EY,na.rm = T),by=c('Date')]
    port[,K_eEY:=kurtosis(eEY,na.rm = T),by=c('Date')]
    port[,K_SAL:=kurtosis(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,K_EPS:=kurtosis(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,K_Mom31:=kurtosis(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,S_Mom:=skewness(Mom_365_30,na.rm = T),by=c('Date')]
    port[,S_ARM:=skewness(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,S_EQ:=skewness(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,S_Vol:=skewness(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,S_DY:=skewness(DY,na.rm = T),by=c('Date')]
    port[,S_CFY:=skewness(CFY,na.rm = T),by=c('Date')]
    port[,S_EY:=skewness(EY,na.rm = T),by=c('Date')]
    port[,S_eEY:=skewness(eEY,na.rm = T),by=c('Date')]
    port[,S_SAL:=skewness(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,S_EPS:=skewness(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,S_Mom31:=skewness(Mom_31_0,na.rm = T),by=c('Date')]
    
    og_port=port
    port=unique(port,by=c('Date'))
    setorder(port,Date,ID)
    #Additional inputs to use
    port[,U_Cor_Mom:=shift(Cor_Mom,n=2,typ='lag')]
    port[,U_Cor_ARM:=shift(Cor_ARM,n=2,typ='lag')]
    port[,U_Cor_EQ:=shift(Cor_EQ,n=2,typ='lag')]
    port[,U_Cor_Vol:=shift(Cor_Vol,n=2,typ='lag')]
    port[,U_Cor_DY:=shift(Cor_DY,n=2,typ='lag')]
    port[,U_Cor_CFY:=shift(Cor_CFY,n=2,typ='lag')]
    port[,U_Cor_EY:=shift(Cor_EY,n=2,typ='lag')]
    port[,U_Cor_eEY:=shift(Cor_eEY,n=2,typ='lag')]
    port[,U_Cor_SAL:=shift(Cor_SAL,n=2,typ='lag')]
    port[,U_Cor_EPS:=shift(Cor_EPS,n=2,typ='lag')]
    port[,U_Cor_Mom31:=shift(Cor_Mom31,n=2,typ='lag')]
    port[,U_Value:=shift(Cor_Value,n=2,typ='lag')]
    port[,U_Momentum:=shift(Cor_Momentum,n=2,typ='lag')]
    port[,U_Qual:=shift(Cor_Qual,n=2,typ='lag')]
    port[,Num:=1]
    port[,Posn:=cumsum(Num)]
    starting_OS_date=port_edate_original$Date[1]
    port[Date==starting_OS_date,Position:=Posn]
    send_to_end=port[,c('Date','U_Value','U_Qual')]
    #Sending the last out one as the y sample, AKA, what we want to predict on
    in_sample=port[,c('Norm_Mom','Norm_ARM','Norm_EQ','Norm_Vol','Norm_DY','Norm_CFY','Norm_EY','Norm_eEY','Norm_SAL',
                      'Norm_EPS','Norm_Mom31',
                      'SD_Mom','SD_ARM','SD_EQ','SD_Vol','SD_DY','SD_CFY','SD_EY','SD_eEY','SD_SAL',
                      'SD_EPS','SD_Mom31',
                      'IQR_Mom','IQR_ARM','IQR_EQ','IQR_Vol','IQR_DY','IQR_CFY','IQR_EY','IQR_eEY','IQR_SAL',
                      'IQR_EPS','IQR_Mom31',
                      'K_Mom','K_ARM','K_EQ','K_Vol','K_DY','K_CFY','K_EY','K_eEY','K_SAL','K_EPS','K_Mom31',
                      'S_Mom','S_ARM','S_EQ','S_Vol','S_DY','S_CFY','S_EY','S_eEY','S_SAL','S_EPS','S_Mom31',
                      'U_Cor_Mom','U_Cor_ARM','U_Cor_EQ','U_Cor_Vol','U_Cor_DY','U_Cor_CFY','U_Cor_EY',
                      'U_Cor_eEY','U_Cor_SAL','U_Cor_EPS','U_Cor_Mom31','U_Value','U_Momentum','U_Qual')]
    Xs=in_sample[Norm_Mom==564654654]
    sample=length(unique(port_edate_original$Date))
    position=mean(port$Position,na.rm = T)-step_size-1  #Where the date is located-stepsize-1, we add +1 via sample to get back
    print(paste('First Date in OS:',starting_OS_date))
    for(l in seq(1,sample)){
      print(paste('Doing the first',l,'groups (rolling)'))
      first_in=in_sample[l+seq(0,step_size)+position,]
      Xs=rbind(Xs,first_in)
    }
    x=Xs
    n_col=ncol(x)
    x=as.matrix(x)
    x[is.infinite(x) | is.na(x) | is.nan(x)]=0
    
    #Time to re-arrange X and Y appropriately (to array)
    X=array(x,dim=c(sample,step_size,n_col))
    print('Starting RNN Predictions')
    Mo_Preds=RNN_Mo %>% predict(X)
    Val_Preds=RNN_Val %>% predict(X)
    Qual_Preds=RNN_Qual %>% predict(X)
    port_dates=unique(port_edate_original,by=c('Date'))
    port_dates[,Mo_Preds:=Mo_Preds]
    port_dates[,Val_Preds:=Val_Preds]
    port_dates[,Qual_Preds:=Qual_Preds]
    port_dates=port_dates[,c('Date','Mo_Preds','Val_Preds','Qual_Preds')]
    port_dates=merge(port_edate_original,port_dates,by=c('Date'))
    
    if(bench[1]=='CA'){
    #Constructing RNN as nums
      #Long
      port_dates[(Mo_Preds>Val_Preds) & (Mo_Preds>Qual_Preds),Long:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')] #Mo good
      port_dates[(Val_Preds>Mo_Preds) & (Val_Preds>Qual_Preds),Long:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')] #Val Good
      port_dates[(Qual_Preds>Mo_Preds) & (Qual_Preds>Val_Preds),Long:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')] #Qual Good
      #Short
      port_dates[(Mo_Preds<Val_Preds) & (Mo_Preds<Qual_Preds),Short:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port_dates[(Val_Preds<Mo_Preds) & (Val_Preds<Qual_Preds),Short:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')]
      port_dates[(Qual_Preds<Mo_Preds) & (Qual_Preds<Val_Preds),Short:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
      
    #RNN Benchmark to beat
    port_dates=merge(send_to_end,port_dates,by=c('Date'))
    #Long
    port_dates[(U_Momentum>U_Value) & (U_Momentum>U_Qual),Hyp_Long:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')] #Mo good
    port_dates[(U_Value>U_Momentum) & (U_Value>U_Qual),Hyp_Long:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')] #Val Good
    port_dates[(U_Qual>U_Momentum) & (U_Qual>U_Value),Hyp_Long:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')] #Qual Good
    #Short
    port_dates[(U_Momentum<U_Value) & (U_Momentum<U_Qual),Hyp_Short:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
    port_dates[(U_Value<U_Momentum) & (U_Value<U_Qual),Hyp_Short:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')]
    port_dates[(U_Qual<U_Momentum) & (U_Qual<U_Value),Hyp_Short:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    
    if(bench[1]=='US'){
      #Constructing RNN as nums
      #Long
      port_dates[(Mo_Preds>Val_Preds) & (Mo_Preds>Qual_Preds),Long:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')] #Mo good
      port_dates[(Val_Preds>Mo_Preds) & (Val_Preds>Qual_Preds),Long:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')] #Val Good
      port_dates[(Qual_Preds>Mo_Preds) & (Qual_Preds>Val_Preds),Long:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')] #Qual Good
      #Short
      port_dates[(Mo_Preds<Val_Preds) & (Mo_Preds<Qual_Preds),Short:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port_dates[(Val_Preds<Mo_Preds) & (Val_Preds<Qual_Preds),Short:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')]
      port_dates[(Qual_Preds<Mo_Preds) & (Qual_Preds<Val_Preds),Short:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]

      #RNN Benchmark to beat
      port_dates=merge(send_to_end,port_dates,by=c('Date'))
      #Long
      port_dates[(U_Momentum>U_Value) & (U_Momentum>U_Qual),Hyp_Long:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')] #Mo good
      port_dates[(U_Value>U_Momentum) & (U_Value>U_Qual),Hyp_Long:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')] #Val Good
      port_dates[(U_Qual>U_Momentum) & (U_Qual>U_Value),Hyp_Long:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')] #Qual Good
      #Short
      port_dates[(U_Momentum<U_Value) & (U_Momentum<U_Qual),Hyp_Short:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port_dates[(U_Value<U_Momentum) & (U_Value<U_Qual),Hyp_Short:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')]
      port_dates[(U_Qual<U_Momentum) & (U_Qual<U_Value),Hyp_Short:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    
    port=port_dates[,c('ID','Date','Long','Short','Hyp_Long','Hyp_Short')]
    port15=to_100_rank(port,cols_to_apply_to = c('Long','Short','Hyp_Long','Hyp_Short'),new_names = c('Long','Short','Hyp_Long','Hyp_Short'))
    port15[Long>=50,RNN:=Long]
    port15[Short<=50,RNN:=Short]
    port15[Hyp_Long>=50,Hyp_RNN:=Hyp_Long]
    port15[Hyp_Short<=50,Hyp_RNN:=Hyp_Short]
    portfolio15=Portfolio_maker(port15,cols_to_apply_to = 'RNN',top_bot_pct = maker,pds=test_pds)
    portfolio=merge(portfolio,portfolio15,by=c('ID','Date'))
    portfolio16=Portfolio_maker(port15,cols_to_apply_to = 'Hyp_RNN',top_bot_pct = maker,pds=test_pds)
    portfolio=merge(portfolio,portfolio16,by=c('ID','Date'))
    
    print('RNN was successful')
    
  }
  
  if('RNN_Mo' %in% num_of_algos){
    #Not working as of yet
    set.seed(123456)
    require(keras)
    require(tensorflow)
    all_d=calc_CSnorm_dt(All_Factors,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
                                            N_Volatility_185_0='Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',N_Mom_31_0='Mom_31_0',
                                            N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth'
    )
    ,grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
    highest_date=max(all_data$Date)
    all_d=all_d[Date<=as.Date(highest_date)]
    port_edate_original=as.data.table(all_d)
    setorder(port_edate_original)
    port=as.data.table(all_d)
    
    if(macro==F){
      port=port[,c('ID','Date','Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY','CFY',
                   'EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                   'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY','N_CFY',
                   'N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth','N_Mom_31_0')]
    }
    else{
      port=port[,c('ID','Date','Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY','CFY',
                   'EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                   'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY','N_CFY',
                   'N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth','N_Mom_31_0',macro_names),with=F]
    }

    setorder(port,Date,ID)
    invisible(lapply(names(port),function(.name) set(port, which(is.infinite(port[[.name]])), j = .name,value =NA)))
    invisible(lapply(names(port),function(.name) set(port, which(is.nan(port[[.name]])), j = .name,value =NA)))
    #Data prep for RNN
    port=merge(port,test_active1,by=c('ID','Date'))
    #Outputs to find
    if(bench[1]=='CA'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    if(bench[1]=='US'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    port[,Cor_Value:=cor(Value,ActiveFwdRet,use='pairwise.complete',method='pearson'),by=c('Date')]
    port[,Cor_Momentum:=cor(Mom,ActiveFwdRet,use='pairwise.complete',method='pearson'),by=c('Date')]
    port[,Cor_Qual:=cor(Qual,ActiveFwdRet,use='pairwise.complete',method='pearson'),by=c('Date')]
    
    #Grabbing previous Rets
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'Mom',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_Mom')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_Mom:=sum(overall_weight_Mom*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'ARM_100_REG',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_ARM_100_REG')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_ARM:=sum(overall_weight_ARM_100_REG*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'EQ_REG_RANK',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_EQ_REG_RANK')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_EQ:=sum(overall_weight_EQ_REG_RANK*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'Mom_31_0',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_Mom_31_0')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_Mom31:=sum(overall_weight_Mom_31_0*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'DY',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_DY')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_DY:=sum(overall_weight_DY*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'CFY',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_CFY')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_CFY:=sum(overall_weight_CFY*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'EY',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_EY')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_EY:=sum(overall_weight_EY*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'eEY',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_eEY')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_eEY:=sum(overall_weight_eEY*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'EPS_eGrowth',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_EPS_eGrowth')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_EPS:=sum(overall_weight_EPS_eGrowth*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'Volatility_185_0',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_Volatility_185_0')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_Vol:=sum(overall_weight_Volatility_185_0*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    thingy=copy(port)
    port_thingy=Portfolio_maker(thingy,cols_to_apply_to = 'SAL_eGrowth',top_bot_pct = 20,pds=train_pds)[,c('ID','Date','overall_weight_SAL_eGrowth')]
    port=merge(port_thingy,port,by=c('ID','Date'))
    port[,Cor_SAL:=sum(overall_weight_SAL_eGrowth*ActiveFwdRet,na.rm = T)*1,by=c('Date')]
    port[,Cor_Value:=cor(Value,ActiveFwdRet,use='pairwise.complete',method='pearson'),by=c('Date')]
    port[,Cor_Qual:=cor(Qual,ActiveFwdRet,use='pairwise.complete',method='pearson'),by=c('Date')]
    
    
    #Normalized inputs
    port[,Norm_Mom:=mean(Mom_365_30,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_Mom)][,Norm_Mom:=Norm_Mom/temp_max]
    port[,Norm_ARM:=mean(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,Norm_EQ:=mean(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,Norm_Vol:=mean(Volatility_185_0,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_Vol)][,Norm_Mom:=Norm_Vol/temp_max]
    port[,Norm_DY:=mean(DY,na.rm = T),by=c('Date')]#[,temp_DY:=cummaxNA(Norm_DY)][,Norm_DY:=Norm_DY/temp_max]
    port[,Norm_CFY:=mean(CFY,na.rm = T),by=c('Date')]#[,temp_CFY:=cummaxNA(Norm_CFY)][,Norm_CFY:=Norm_CFY/temp_max]
    port[,Norm_EY:=mean(EY,na.rm = T),by=c('Date')]#[,temp_EY:=cummaxNA(Norm_EY)][,Norm_EY:=Norm_EY/temp_max]
    port[,Norm_eEY:=mean(eEY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_eEY)][,Norm_eEY:=Norm_eEY/temp_max]
    port[,Norm_SAL:=mean(SAL_eGrowth,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_SAL)][,Norm_SAL:=Norm_SAL/temp_max]
    port[,Norm_EPS:=mean(EPS_eGrowth,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_EPS)][,Norm_EPS:=Norm_EPS/temp_max]
    port[,Norm_Mom31:=mean(Mom_31_0,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(Norm_Mom31)][,Norm_Mom31:=Norm_Mom31/temp_max]
    
    port[,SD_Mom:=var(Mom_365_30,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_Mom)][,SD_Mom:=SD_Mom/temp_max]
    port[,SD_ARM:=var(ARM_100_REG,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_ARM)][,SD_ARM:=SD_ARM/temp_max]
    port[,SD_EQ:=var(EQ_REG_RANK,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_EQ)][,SD_EQ:=SD_EQ/temp_max]
    port[,SD_Vol:=var(Volatility_185_0,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_Vol)][,SD_Vol:=SD_Vol/temp_max]
    port[,SD_DY:=var(DY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_DY)][,SD_DY:=SD_DY/temp_max]
    port[,SD_CFY:=var(CFY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_CFY)][,SD_CFY:=SD_CFY/temp_max]
    port[,SD_EY:=var(EY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_EY)][,SD_EY:=SD_EY/temp_max]
    port[,SD_eEY:=var(eEY,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_eEY)][,SD_eEY:=SD_eEY/temp_max]
    port[,SD_SAL:=var(SAL_eGrowth,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_SAL)][,SD_SAL:=SD_SAL/temp_max]
    port[,SD_EPS:=var(EPS_eGrowth,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_EPS)][,SD_EPS:=SD_EPS/temp_max]
    port[,SD_Mom31:=var(Mom_31_0,na.rm = T),by=c('Date')]#[,temp_max:=cummaxNA(SD_Mom31)][,SD_Mom31:=SD_Mom31/temp_max]
    
    port[,IQR_Mom:=IQR(Mom_365_30,na.rm = T),by=c('Date')]
    port[,IQR_ARM:=IQR(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,IQR_EQ:=IQR(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,IQR_Vol:=IQR(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,IQR_DY:=IQR(DY,na.rm = T),by=c('Date')]
    port[,IQR_CFY:=IQR(CFY,na.rm = T),by=c('Date')]
    port[,IQR_EY:=IQR(EY,na.rm = T),by=c('Date')]
    port[,IQR_eEY:=IQR(eEY,na.rm = T),by=c('Date')]
    port[,IQR_SAL:=IQR(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_EPS:=IQR(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_Mom31:=IQR(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,K_Mom:=kurtosis(Mom_365_30,na.rm = T),by=c('Date')]
    port[,K_ARM:=kurtosis(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,K_EQ:=kurtosis(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,K_Vol:=kurtosis(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,K_DY:=kurtosis(DY,na.rm = T),by=c('Date')]
    port[,K_CFY:=kurtosis(CFY,na.rm = T),by=c('Date')]
    port[,K_EY:=kurtosis(EY,na.rm = T),by=c('Date')]
    port[,K_eEY:=kurtosis(eEY,na.rm = T),by=c('Date')]
    port[,K_SAL:=kurtosis(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,K_EPS:=kurtosis(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,K_Mom31:=kurtosis(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,S_Mom:=skewness(Mom_365_30,na.rm = T),by=c('Date')]
    port[,S_ARM:=skewness(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,S_EQ:=skewness(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,S_Vol:=skewness(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,S_DY:=skewness(DY,na.rm = T),by=c('Date')]
    port[,S_CFY:=skewness(CFY,na.rm = T),by=c('Date')]
    port[,S_EY:=skewness(EY,na.rm = T),by=c('Date')]
    port[,S_eEY:=skewness(eEY,na.rm = T),by=c('Date')]
    port[,S_SAL:=skewness(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,S_EPS:=skewness(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,S_Mom31:=skewness(Mom_31_0,na.rm = T),by=c('Date')]
    
    og_port=port
    port=unique(port,by=c('Date'))
    setorder(port,Date,ID)
    #Additional inputs to use
    port[,U_Cor_Mom:=shift(Cor_Mom,n=2,typ='lag')]
    port[,U_Cor_ARM:=shift(Cor_ARM,n=2,typ='lag')]
    port[,U_Cor_EQ:=shift(Cor_EQ,n=2,typ='lag')]
    port[,U_Cor_Vol:=shift(Cor_Vol,n=2,typ='lag')]
    port[,U_Cor_DY:=shift(Cor_DY,n=2,typ='lag')]
    port[,U_Cor_CFY:=shift(Cor_CFY,n=2,typ='lag')]
    port[,U_Cor_EY:=shift(Cor_EY,n=2,typ='lag')]
    port[,U_Cor_eEY:=shift(Cor_eEY,n=2,typ='lag')]
    port[,U_Cor_SAL:=shift(Cor_SAL,n=2,typ='lag')]
    port[,U_Cor_EPS:=shift(Cor_EPS,n=2,typ='lag')]
    port[,U_Cor_Mom31:=shift(Cor_Mom31,n=2,typ='lag')]
    port[,U_Value:=shift(Cor_Value,n=2,typ='lag')]
    port[,U_Qual:=shift(Cor_Qual,n=2,typ='lag')]
    port[,Num:=1]
    port[,Posn:=cumsum(Num)]
    starting_OS_date=min(all_data$Date)
    port[Date==starting_OS_date,Position:=Posn]
    send_to_end=port[,c('Date','U_Value','U_Qual')]
    
    port[U_Cor_Mom>=0,Last_Mom_Pos:=1]
    port[U_Cor_Mom<=0,Last_Mom_Neg:=1]
    #Sending the last out one as the y sample, AKA, what we want to predict on
    if(macro==F){
      in_sample=port[,c('Norm_Mom','Norm_ARM','Norm_EQ','Norm_Vol','Norm_DY','Norm_CFY','Norm_EY','Norm_eEY','Norm_SAL',
                        'Norm_EPS','Norm_Mom31',
                        'SD_Mom','SD_ARM','SD_EQ','SD_Vol','SD_DY','SD_CFY','SD_EY','SD_eEY','SD_SAL',
                        'SD_EPS','SD_Mom31',
                        'IQR_Mom','IQR_ARM','IQR_EQ','IQR_Vol','IQR_DY','IQR_CFY','IQR_EY','IQR_eEY','IQR_SAL',
                        'IQR_EPS','IQR_Mom31',
                        'K_Mom','K_ARM','K_EQ','K_Vol','K_DY','K_CFY','K_EY','K_eEY','K_SAL','K_EPS','K_Mom31',
                        'S_Mom','S_ARM','S_EQ','S_Vol','S_DY','S_CFY','S_EY','S_eEY','S_SAL','S_EPS','S_Mom31',
                        'U_Cor_Mom','U_Cor_ARM','U_Cor_EQ','U_Cor_Vol','U_Cor_DY','U_Cor_CFY','U_Cor_EY',
                        'U_Cor_eEY','U_Cor_SAL','U_Cor_EPS','U_Cor_Mom31','U_Value','U_Momentum','U_Qual')]
    }
    else{
      in_sample=port[,c('SD_Mom','SD_ARM','SD_EQ','SD_Vol','SD_DY','SD_CFY','SD_EY','SD_eEY','SD_SAL',
                        'SD_EPS','SD_Mom31',
                        'U_Cor_Mom','U_Cor_ARM','U_Cor_EQ','U_Cor_DY','U_Cor_CFY','U_Cor_EY',
                        'U_Cor_eEY','U_Cor_SAL','U_Cor_EPS','U_Cor_Mom31','U_Value','U_Qual','Last_Mom_Pos',
                        'Last_Mom_Neg',macro_names),with=F]
    }
    
    Xs=in_sample[U_Cor_Mom==564654654]
    sample=length(unique(all_data$Date))
    position=mean(port$Position,na.rm = T)-step_size-1  #Where the date is located-stepsize-1, we add +1 via sample to get back
    print(paste('First Date in OS:',starting_OS_date))
    for(l in seq(1,sample)){
      print(paste('Doing the first',l,'groups (rolling)'))
      first_in=in_sample[l+seq(0,step_size)+position,]
      Xs=rbind(Xs,first_in)
    }
    x=Xs
    n_col=ncol(x)
    x=as.matrix(x)
    x[is.infinite(x) | is.na(x) | is.nan(x)]=0
    
    #Time to re-arrange X and Y appropriately (to array)
    X=array(x,dim=c(sample,step_size,n_col))
    print('Starting RNN Predictions')
    set.seed(123456)
    Mo_Preds=RNN_Mo %>% predict(X) #Class
    the_preds<<-Mo_Preds
    print(paste('Preds:',Mo_Preds))
    port_dates=unique(all_data,by=c('Date'))
    port_dates[,Mo_Preds:=Mo_Preds]
    #port_dates[(Mo_Preds==0),Mo_Preds:=0.5][(Mo_Preds==1),Mo_Preds:=-0.5] #Class
    port_dates=port_dates[,c('Date','Mo_Preds')]
    port_dates=merge(all_data,port_dates,by=c('Date'))
    port_dates=calc_CSnorm_dt(port_dates,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
                                                    N_Volatility_185_0='Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',N_Mom_31_0='Mom_31_0',
                                                    N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth'
    ),grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
    
    if(bench[1]=='CA'){
      #Constructing RNN as nums
      #Long
      port_dates[Mo_Preds>=0,RNN_Mo:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')] #Mo good
      #Short
      #port_dates[Mo_Preds<=0,RNN_Mo:=-mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port_dates[Mo_Preds<=0,RNN_Mo:=0]
      
      #RNN Benchmark to beat
      port_dates=merge(send_to_end,port_dates,by=c('Date'))
      #Long
      port_dates[,Hyp_RNN:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')] #Mo good
    }
    
    if(bench[1]=='US'){
      #Constructing RNN as nums
      #Long
      port_dates[Mo_Preds>=0,RNN_Mo:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')] #Mo good
      
      #Short
      port_dates[Mo_Preds<=0,RNN_Mo:=-mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      
      #RNN Benchmark to beat
      port_dates=merge(send_to_end,port_dates,by=c('Date'))
      #Long
      port_dates[,Hyp_RNN:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')] #Mo good
    }
    
    port15=port_dates[,c('ID','Date','RNN_Mo','Hyp_RNN')]
    portfolio15=Portfolio_maker(port15,cols_to_apply_to = 'RNN_Mo',top_bot_pct = maker,pds=test_pds)
    portfolio=merge(portfolio,portfolio15,by=c('ID','Date'))
    portfolio16=Portfolio_maker(port15,cols_to_apply_to = 'Hyp_RNN',top_bot_pct = maker,pds=test_pds)
    portfolio=merge(portfolio,portfolio16,by=c('ID','Date'))
    
    print('RNN_Mo was successful')
    
  }
  
  if('RNN_Class' %in% num_of_algos){
    #Not working as of yet
    set.seed(123456)
    require(keras)
    require(tensorflow)
    port_edate_original=as.data.table(all_data)
    setorder(port_edate_original)
    port=as.data.table(All_Factors)
    port=calc_CSnorm_dt(port,var_lst = list(N_Mom_365_30='Mom_365_30',N_ARM_100_REG='ARM_100_REG',N_EQ_REG_RANK='EQ_REG_RANK',
                                            N_Volatility_185_0='Volatility_185_0',N_DY='DY',N_CFY='CFY',N_EY='EY',N_eEY='eEY',N_Mom_31_0='Mom_31_0',
                                            N_SAL_eGrowth='SAL_eGrowth',N_EPS_eGrowth='EPS_eGrowth'
    )
    ,grp_col = c('Date'),InfNA = T,zscore = F,rnk=T,verbose = T)
    
    port=port[,c('ID','Date','Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY','CFY',
                 'EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                 'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY','N_CFY',
                 'N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth','N_Mom_31_0')]
    setorder(port,Date,ID)
    invisible(lapply(names(port),function(.name) set(port, which(is.infinite(port[[.name]])), j = .name,value =NA)))
    invisible(lapply(names(port),function(.name) set(port, which(is.nan(port[[.name]])), j = .name,value =NA)))
    #Data prep for RNN
    port=merge(port,test_active1,by=c('ID','Date'))
    port[,Cor_Mom:=cor(Mom_365_30,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_ARM:=cor(ARM_100_REG,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EQ:=cor(EQ_REG_RANK,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Vol:=cor(Volatility_185_0,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_DY:=cor(DY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_CFY:=cor(CFY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EY:=cor(EY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_eEY:=cor(eEY,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_SAL:=cor(SAL_eGrowth,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_EPS:=cor(EPS_eGrowth,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Mom31:=cor(Mom_31_0,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    
    #Outputs to find
    if(bench[1]=='CA'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    if(bench[1]=='US'){
      port[,Value:=mean(c(N_DY,N_CFY,N_EY,N_eEY),na.rm = T),by=c('ID','Date')]
      port[,Mom:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port[,Qual:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    port[,Cor_Value:=cor(Value,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Momentum:=cor(Mom,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    port[,Cor_Qual:=cor(Qual,ActiveFwdRet,use='pairwise.complete',method='spearman'),by=c('Date')]
    
    #Normalized inputs
    port[,Norm_Mom:=mean(Mom_365_30,na.rm = T),by=c('Date')]
    port[,Norm_ARM:=mean(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,Norm_EQ:=mean(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,Norm_Vol:=mean(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,Norm_DY:=mean(DY,na.rm = T),by=c('Date')]
    port[,Norm_CFY:=mean(CFY,na.rm = T),by=c('Date')]
    port[,Norm_EY:=mean(EY,na.rm = T),by=c('Date')]
    port[,Norm_eEY:=mean(eEY,na.rm = T),by=c('Date')]
    port[,Norm_SAL:=mean(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,Norm_EPS:=mean(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,Norm_Mom31:=mean(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,SD_Mom:=var(Mom_365_30,na.rm = T),by=c('Date')]
    port[,SD_ARM:=var(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,SD_EQ:=var(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,SD_Vol:=var(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,SD_DY:=var(DY,na.rm = T),by=c('Date')]
    port[,SD_CFY:=var(CFY,na.rm = T),by=c('Date')]
    port[,SD_EY:=var(EY,na.rm = T),by=c('Date')]
    port[,SD_eEY:=var(eEY,na.rm = T),by=c('Date')]
    port[,SD_SAL:=var(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,SD_EPS:=var(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,SD_Mom31:=var(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,IQR_Mom:=IQR(Mom_365_30,na.rm = T),by=c('Date')]
    port[,IQR_ARM:=IQR(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,IQR_EQ:=IQR(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,IQR_Vol:=IQR(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,IQR_DY:=IQR(DY,na.rm = T),by=c('Date')]
    port[,IQR_CFY:=IQR(CFY,na.rm = T),by=c('Date')]
    port[,IQR_EY:=IQR(EY,na.rm = T),by=c('Date')]
    port[,IQR_eEY:=IQR(eEY,na.rm = T),by=c('Date')]
    port[,IQR_SAL:=IQR(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_EPS:=IQR(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,IQR_Mom31:=IQR(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,K_Mom:=kurtosis(Mom_365_30,na.rm = T),by=c('Date')]
    port[,K_ARM:=kurtosis(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,K_EQ:=kurtosis(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,K_Vol:=kurtosis(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,K_DY:=kurtosis(DY,na.rm = T),by=c('Date')]
    port[,K_CFY:=kurtosis(CFY,na.rm = T),by=c('Date')]
    port[,K_EY:=kurtosis(EY,na.rm = T),by=c('Date')]
    port[,K_eEY:=kurtosis(eEY,na.rm = T),by=c('Date')]
    port[,K_SAL:=kurtosis(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,K_EPS:=kurtosis(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,K_Mom31:=kurtosis(Mom_31_0,na.rm = T),by=c('Date')]
    
    port[,S_Mom:=skewness(Mom_365_30,na.rm = T),by=c('Date')]
    port[,S_ARM:=skewness(ARM_100_REG,na.rm = T),by=c('Date')]
    port[,S_EQ:=skewness(EQ_REG_RANK,na.rm = T),by=c('Date')]
    port[,S_Vol:=skewness(Volatility_185_0,na.rm = T),by=c('Date')]
    port[,S_DY:=skewness(DY,na.rm = T),by=c('Date')]
    port[,S_CFY:=skewness(CFY,na.rm = T),by=c('Date')]
    port[,S_EY:=skewness(EY,na.rm = T),by=c('Date')]
    port[,S_eEY:=skewness(eEY,na.rm = T),by=c('Date')]
    port[,S_SAL:=skewness(SAL_eGrowth,na.rm = T),by=c('Date')]
    port[,S_EPS:=skewness(EPS_eGrowth,na.rm = T),by=c('Date')]
    port[,S_Mom31:=skewness(Mom_31_0,na.rm = T),by=c('Date')]
    
    og_port=port
    port=unique(port,by=c('Date'))
    setorder(port,Date,ID)
    #Additional inputs to use
    port[,U_Cor_Mom:=shift(Cor_Mom,n=2,typ='lag')]
    port[,U_Cor_ARM:=shift(Cor_ARM,n=2,typ='lag')]
    port[,U_Cor_EQ:=shift(Cor_EQ,n=2,typ='lag')]
    port[,U_Cor_Vol:=shift(Cor_Vol,n=2,typ='lag')]
    port[,U_Cor_DY:=shift(Cor_DY,n=2,typ='lag')]
    port[,U_Cor_CFY:=shift(Cor_CFY,n=2,typ='lag')]
    port[,U_Cor_EY:=shift(Cor_EY,n=2,typ='lag')]
    port[,U_Cor_eEY:=shift(Cor_eEY,n=2,typ='lag')]
    port[,U_Cor_SAL:=shift(Cor_SAL,n=2,typ='lag')]
    port[,U_Cor_EPS:=shift(Cor_EPS,n=2,typ='lag')]
    port[,U_Cor_Mom31:=shift(Cor_Mom31,n=2,typ='lag')]
    port[,U_Value:=shift(Cor_Value,n=2,typ='lag')]
    port[,U_Momentum:=shift(Cor_Momentum,n=2,typ='lag')]
    port[,U_Qual:=shift(Cor_Qual,n=2,typ='lag')]
    port[,Num:=1]
    port[,Posn:=cumsum(Num)]
    starting_OS_date=port_edate_original$Date[1]
    port[Date==starting_OS_date,Position:=Posn]
    send_to_end=port[,c('Date','U_Value','U_Momentum','U_Qual')]
    #Sending the last out one as the y sample, AKA, what we want to predict on
    in_sample=port[,c('Norm_Mom','Norm_ARM','Norm_EQ','Norm_Vol','Norm_DY','Norm_CFY','Norm_EY','Norm_eEY','Norm_SAL',
                      'Norm_EPS','Norm_Mom31',
                      'SD_Mom','SD_ARM','SD_EQ','SD_Vol','SD_DY','SD_CFY','SD_EY','SD_eEY','SD_SAL',
                      'SD_EPS','SD_Mom31',
                      'IQR_Mom','IQR_ARM','IQR_EQ','IQR_Vol','IQR_DY','IQR_CFY','IQR_EY','IQR_eEY','IQR_SAL',
                      'IQR_EPS','IQR_Mom31',
                      'K_Mom','K_ARM','K_EQ','K_Vol','K_DY','K_CFY','K_EY','K_eEY','K_SAL','K_EPS','K_Mom31',
                      'S_Mom','S_ARM','S_EQ','S_Vol','S_DY','S_CFY','S_EY','S_eEY','S_SAL','S_EPS','S_Mom31',
                      'U_Cor_Mom','U_Cor_ARM','U_Cor_EQ','U_Cor_Vol','U_Cor_DY','U_Cor_CFY','U_Cor_EY',
                      'U_Cor_eEY','U_Cor_SAL','U_Cor_EPS','U_Cor_Mom31','U_Value','U_Momentum','U_Qual')]
    Xs=in_sample[Norm_Mom==564654654]
    sample=length(unique(port_edate_original$Date))
    position=mean(port$Position,na.rm = T)-step_size
    print(paste('First Date in OS:',starting_OS_date))
    for(l in seq(1,sample)){
      print(paste('Doing the first',l,'groups (rolling)'))
      first_in=in_sample[l+seq(0,step_size)+position,]
      Xs=rbind(Xs,first_in)
    }
    x=Xs
    n_col=ncol(x)
    x=as.matrix(x)
    x[is.infinite(x) | is.na(x) | is.nan(x)]=0
    
    #Time to re-arrange X and Y appropriately (to array)
    X=array(x,dim=c(sample,step_size,n_col))
    print('Starting RNN Predictions')
    Long_Preds=RNN_Long %>% predict_classes(X)
    Short_Preds=RNN_Short %>% predict_classes(X)
    port_dates=unique(port_edate_original,by=c('Date'))
    port_dates[,Long_Preds:=Long_Preds]
    port_dates[,Short_Preds:=Short_Preds]
    port_dates=port_dates[,c('Date','Long_Preds','Short_Preds')]
    port_dates=merge(port_edate_original,port_dates,by=c('Date'))
    
    if(bench[1]=='CA'){
      #Constructing RNN as nums
      #Long
      #Value Mo, then Qual
      port_dates[Long_Preds==1,Long:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')] #Value
      port_dates[Long_Preds==0,Long:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')] #Mo
      port_dates[Long_Preds==2,Long:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')] #Qual
      #Short
      port_dates[Short_Preds==1,Short:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port_dates[Short_Preds==0,Short:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')]
      port_dates[Short_Preds==2,Short:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
      
      #RNN Benchmark to beat
      port_dates=merge(send_to_end,port_dates,by=c('Date'))
      #Long
      port_dates[(U_Momentum>U_Value) & (U_Momentum>U_Qual),Hyp_Long:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')] #Mo good
      port_dates[(U_Value>U_Momentum) & (U_Value>U_Qual),Hyp_Long:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')] #Val Good
      port_dates[(U_Qual>U_Momentum) & (U_Qual>U_Value),Hyp_Long:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')] #Qual Good
      #Short
      port_dates[(U_Momentum<U_Value) & (U_Momentum<U_Qual),Hyp_Short:=mean(c(N_Mom_365_30,N_ARM_100_REG),na.rm = T),by=c('ID','Date')]
      port_dates[(U_Value<U_Momentum) & (U_Value<U_Qual),Hyp_Short:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')]
      port_dates[(U_Qual<U_Momentum) & (U_Qual<U_Value),Hyp_Short:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    
    if(bench[1]=='US'){
      #Constructing RNN as nums
      #Long
      #Val, Mo, then Qual
      port_dates[Long_Preds==1,Long:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')] #Mo good
      port_dates[Long_Preds==0,Long:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')] #Val Good
      port_dates[Long_Preds==2,Long:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')] #Qual Good
      #Short
      port_dates[Short_Preds==1,Short:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port_dates[Short_Preds==0,Short:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')]
      port_dates[Short_Preds==2,Short:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
      
      #RNN Benchmark to beat
      port_dates=merge(send_to_end,port_dates,by=c('Date'))
      #Long
      port_dates[(U_Momentum>U_Value) & (U_Momentum>U_Qual),Hyp_Long:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')] #Mo good
      port_dates[(U_Value>U_Momentum) & (U_Value>U_Qual),Hyp_Long:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')] #Val Good
      port_dates[(U_Qual>U_Momentum) & (U_Qual>U_Value),Hyp_Long:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')] #Qual Good
      #Short
      port_dates[(U_Momentum<U_Value) & (U_Momentum<U_Qual),Hyp_Short:=mean(c(N_Mom_365_30,N_Mom_365_30,N_ARM_100_REG,N_ARM_100_REG,N_EPS_eGrowth,N_SAL_eGrowth),na.rm = T),by=c('ID','Date')]
      port_dates[(U_Value<U_Momentum) & (U_Value<U_Qual),Hyp_Short:=mean(c(N_EY,N_CFY,N_eEY,N_DY),na.rm = T),by=c('ID','Date')]
      port_dates[(U_Qual<U_Momentum) & (U_Qual<U_Value),Hyp_Short:=mean(c(N_Volatility_185_0,N_EQ_REG_RANK),na.rm = T),by=c('ID','Date')]
    }
    
    port=port_dates[,c('ID','Date','Long','Short','Hyp_Long','Hyp_Short')]
    port15=to_100_rank(port,cols_to_apply_to = c('Long','Short','Hyp_Long','Hyp_Short'),new_names = c('Long','Short','Hyp_Long','Hyp_Short'))
    port15[Long>=50,RNN_Class:=Long]
    port15[Short<=50,RNN_Class:=Short]
    port15[Hyp_Long>=50,Hyp_RNN:=Hyp_Long]
    port15[Hyp_Short<=50,Hyp_RNN:=Hyp_Short]
    portfolio15=Portfolio_maker(port15,cols_to_apply_to = 'RNN_Class',top_bot_pct = maker,pds=test_pds)
    portfolio=merge(portfolio,portfolio15,by=c('ID','Date'))
    portfolio16=Portfolio_maker(port15,cols_to_apply_to = 'Hyp_RNN',top_bot_pct = maker,pds=test_pds)
    portfolio=merge(portfolio,portfolio16,by=c('ID','Date'))
    
    print('RNN was successful')
    
  }
  
  if('PCA_Regr' %in% num_of_algos){
    print('Starting PCA_Regression')
    set.seed(123456)
    port=copy(all_data)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.table(new_port)
    
    port$PCA_Linear_Regression=as.numeric(predict(PCA_lin_reg,port))
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_Linear_Regression')]
    port8=to_100_rank(port,cols_to_apply_to = c('PCA_Linear_Regression'),new_names = c('PCA_Linear_Regression'))
    portfolio8=Portfolio_maker(port8,cols_to_apply_to = 'PCA_Linear_Regression',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio8,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('PCA Regression was successful')
  }
  
  if('PCA_Random_Forest' %in% num_of_algos){
    print('Starting PCA_Random_Forest')
    set.seed(123456)
    port=copy(all_data)
    if('Value' %in% only){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    new_port=(h2o::h2o.predict(pca_ed,port))
    port=new_port
    port=h2o::as.h2o(port)
    
    preds=h2o::h2o.predict(PCA_Random_Forest,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$PCA_Random_Forest=as.numeric(preds$predict)
      
    port=as.data.table(port)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_Random_Forest')]
    port9=to_100_rank(port,cols_to_apply_to = c('PCA_Random_Forest'),new_names = c('PCA_Random_Forest'))
    portfolio9=Portfolio_maker(port9,cols_to_apply_to = 'PCA_Random_Forest',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio9,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('PCA_Random_Forest was successful')
  }
  
  if('PCA_GBM' %in% num_of_algos){
    #require(h2o)
    set.seed(123456)
    port=copy(all_data)
    if('Value' %in% only){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    print('Starting PCA_Gradient Boosted Model')
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    new_port=h2o::h2o.predict(pca_ed,port)
    port=new_port
    
    
    preds=h2o::h2o.predict(PCA_GBM,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$PCA_GBM=as.numeric(preds$predict)
    port=as.data.table(port)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_GBM')]
    port10=to_100_rank(port,cols_to_apply_to = c('PCA_GBM'),new_names = c('PCA_GBM'))
    portfolio10=Portfolio_maker(port10,cols_to_apply_to = 'PCA_GBM',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio10,by=c('ID','Date'))
    #h2o::h2o.removeAll()
    
    print('PCA_Gradient Boosted Model was successful')
  }
  
  if('PCA_FFNN' %in% num_of_algos){
    #require(h2o)
    set.seed(123456)
    if('Value' %in% only){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    print('Starting PCA_FFNN Model')
    port=copy(all_data)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    new_port=h2o::h2o.predict(pca_ed,port)
    port=new_port
    
    preds=h2o::h2o.predict(PCA_FFNN,newdata = port)
    preds=as.data.table(preds)
    port=as.data.table(port)
    port$PCA_FFNN=as.numeric(preds$predict)
    port=as.data.table(port)
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_FFNN')]
    port11=to_100_rank(port,cols_to_apply_to = c('PCA_FFNN'),new_names = c('PCA_FFNN'))
    portfolio11=Portfolio_maker(port11,cols_to_apply_to = 'PCA_FFNN',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio11,by=c('ID','Date'))
    
    
    print('PCA_FFNN was successful')
  }
  
  if('PCA_SVM' %in% num_of_algos){
    require(e1071)
    set.seed(123456)
    print('Starting PCA_SVM Model')
    port=copy(all_data)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.table(new_port)
    
    
    port$PCA_SVM=as.numeric(predict(PCA_SVM,port))
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_SVM')]
    port12=to_100_rank(port,cols_to_apply_to = c('PCA_SVM'),new_names = c('PCA_SVM'))
    portfolio12=Portfolio_maker(port12,cols_to_apply_to = 'PCA_SVM',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio12,by=c('ID','Date'))
    
    print('PCA_SVM was successful')
  }
  
  if('PCA_KNN' %in% num_of_algos){
    require(e1071)
    set.seed(123456)
    print('Starting PCA_KNN Model')
    port=copy(all_data)
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.data.table(new_port)
    
    
    port$PCA_KNN=as.numeric(predict(PCA_KNN,port))
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_KNN')]
    port13=to_100_rank(port,cols_to_apply_to = c('PCA_KNN'),new_names = c('PCA_KNN'))
    portfolio13=Portfolio_maker(port13,cols_to_apply_to = 'PCA_KNN',top_bot_pct = 20,pds=test_pds)
    portfolio=merge(portfolio,portfolio13,by=c('ID','Date'))
    
    print('PCA_KNN was successful')
  }
  
  if('XGBM' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting XGBOOST')
    port=copy(all_data)
    if('Value' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                     'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                     'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                     'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                     'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                     'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                     'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                     'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                     'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('Quality' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                     'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                     'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                     'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                     'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                     'L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('Momentum' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                     'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                     'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                     'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                     'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                     'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                     'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                     'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                     'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                     'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                     'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                     'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                     'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                     'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60')]
      }
      else{
        port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                     'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                     'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                     'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                     'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                     'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                     'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                     'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                     'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                     'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                     'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                     'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                     'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                     'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('SI' %in% only & only_bench==F){
      if(macro==F){
        port=port[,c(names(SI_stuff),
                     'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60'),with=F]
      }
      else{
        port=port[,c(names(SI_stuff),
                     'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                     'L1_45','L1_50','L1_55','L1_60',macro_names),with=F]
      }
      
    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
                   )]
    }
    if('All' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY',
                   'CFY','EY','eEY','SAL_eGrowth','EPS_eGrowth','Mom_31_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60','VIX_IDX','GC_SPOT','LMS4K_IDX','OR_SPOT',
                   'TXZOK_IDX','NY_SPOT')]
      
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    testa_port=copy(port)
    testa_port[,entries:=1]
    testa_port[,entries_per_date:=sum(entries),by=c('Date')]
    testa_port=unique(testa_port,by=c('Date'))[,c('Date','entries_per_date')]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    (length(names(port)))
    port[is.na(port)]=0
    og_port=port
    port=as.matrix(port)
    port[is.infinite(port) | is.na(port) | is.nan(port)]=0
    port=xgb.DMatrix(data=port,group=c(testa_port$entries_per_date))
    
    if('All' %in% GICS){
      preds=predict(XGBM,newdata = port)
      port=og_port
      port$XGBM=preds
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = maker,pds=test_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
      
    }
    else{
      var_name=paste('XGBM_',j,sep='')
      print(paste('Starting',var_name))
      
      preds=predict(eval(parse(text=var_name)),newdata = port)
      port=og_port
      port$XGBM=preds
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = 20,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
    }
  }
  
  if('XGBM2' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting XGBOOST')
    port=copy(all_data)
    if('Value' %in% only & only_bench==F){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==F){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==F){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only & only_bench==F){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
      )]
    }
    if('All' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY',
                   'CFY','EY','eEY','SAL_eGrowth','EPS_eGrowth',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    testa_port=copy(port)
    testa_port[,entries:=1]
    testa_port[,entries_per_date:=sum(entries),by=c('Date')]
    testa_port=unique(testa_port,by=c('Date'))[,c('Date','entries_per_date')]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    (length(names(port)))
    port[is.na(port)]=0
    og_port=port
    port=as.matrix(port)
    port=xgb.DMatrix(data=port,group=c(testa_port$entries_per_date))
    
    if('All' %in% GICS){
      preds=predict(XGBM2,newdata = port)
      port=og_port
      port$XGBM2=preds
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM2')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM2'),new_names = c('XGBM2'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM2',top_bot_pct = maker,pds=test_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM2 was successful')
      
    }
    else{
      var_name=paste('XGBM_',j,sep='')
      print(paste('Starting',var_name))
      
      preds=predict(eval(parse(text=var_name)),newdata = port)
      port=og_port
      port$XGBM=preds
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = 20,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
    }
  }
  
  if('XGBMN' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting XGBOOST')
    port=copy(all_data)
    if('Value' %in% only & only_bench==F){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==F){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==F){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only & only_bench==F){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
      )]
    }
    if('All' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'N_Mom_365_30','N_ARM_100_REG','N_EQ_REG_RANK','N_Volatility_185_0','N_DY',
                   'N_CFY','N_EY','N_eEY','N_SAL_eGrowth','N_EPS_eGrowth',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    testa_port=copy(port)
    testa_port[,entries:=1]
    testa_port[,entries_per_date:=sum(entries),by=c('Date')]
    testa_port=unique(testa_port,by=c('Date'))[,c('Date','entries_per_date')]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    (length(names(port)))
    port[is.na(port)]=0
    og_port=port
    port=as.matrix(port)
    port=xgb.DMatrix(data=port,group=c(testa_port$entries_per_date))
    
    if('All' %in% GICS){
      preds=predict(XGBMN,newdata = port)
      port=og_port
      port$XGBMN=preds
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBMN')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBMN'),new_names = c('XGBMN'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBMN',top_bot_pct = maker,pds=test_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBMN was successful')
      
    }
    else{
      var_name=paste('XGBM_',j,sep='')
      print(paste('Starting',var_name))
      
      preds=predict(eval(parse(text=var_name)),newdata = port)
      port=og_port
      port$XGBM=preds
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = 20,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
    }
  }
  
  if('XGBMS' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting XGBOOST')
    port=copy(all_data)
    if('Value' %in% only & only_bench==F){
      port=port[,c('ID','Date','EV_per_acutal','Price_per_acutal','i.Price_per_acutal','i.Price_per_acutal.1',
                   'i.EV_per_acutal','i.Price_per_acutal.2','i.Price_per_acutal.3','i.Price_per_acutal.4',
                   'i.SO_Chg_Yld','i.PayFreq','EY','BY','CFY','eEY','eCFY','DY','FCFY','EBITDAy','eEBITDAy','SALY',
                   'eSALY','TBY','estEST1','estEST2','estEST3','estEST4','estEST5','estEST6','estEST7',
                   'estEST8','estEST9','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==F){
      port=port[,c('ID','Date','DEBT_TO_EQUITY_RATIO','Ratio','EQ_REG_RANK','ACCRUALS','DEBT_EQUITY',
                   'FCF_Margin','NI_Margin','CASH_MC','Asset_Turn','RnD_Sales','CAPEX_FCF','CAPEX_Dep','eROE',
                   'eND_EBT','Volatility_185_0','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40','L1_45',
                   'L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==F){
      port=port[,c('ID','Date','Consensus','Growth','i.Growth','i.Consensus','BuyPct_30_days',
                   'BuyPct_60_days','BuyPct_90_days','HoldPct_30_days','HoldPct_60_days','HoldPct_90_days',
                   'SellPct_30_days','SellPct_60_days','SellPct_90_days','Mom_365_30','Mom_365_182','Mom_730_30',
                   'Mom_730_365','ARM_100_REG','SAL_eGrowth','EPS_eGrowth','estEST10','estEST11','estEST12',
                   'estEST13','estEST14','estEST15','estEST16','estEST17','estEST18','estEST19','estEST20',
                   'estEST21','estEST22','estEST23','estEST24','estEST25','estEST26','estEST27','estEST28',
                   'estEST29','estEST30','estEST31','estEST32','estEST33','estEST34','estEST35','estEST36',
                   'estEST37','estEST38','estEST39','estEST40','estEST41','estEST42','estEST43','estEST44',
                   'estEST45','estEST46','estEST47','estEST48','estEST49','estEST50','estEST51','estEST52',
                   'estEST53','estEST54','estEST55','estEST56','estEST57','estEST58','estEST59','estEST60',
                   'estEST62','estEST63','estEST64','estEST65','estEST66','estEST67','estEST68','estEST69',
                   'estEST70','estEST71','estEST72','estEST73','estEST74','estEST75','estEST76','estEST77',
                   'estEST78','estEST79','estEST80','estEST81','estEST82','estEST83','estEST84','estEST85',
                   'estEST86','estEST87','L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('SI' %in% only & only_bench==F){
      port=port[,c(names(SI_stuff),
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'),with=F]
    }
    if('SI_prune' %in% only & only_bench==F){
      port=port[,c('ID','Date','s_Active_Available_BO_Inventory_Quant','s_Broker_Deman_Quantity',
                   'ActiveUtilisation','DCBS',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60'
      )]
    }
    if('All' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'Mom_365_30','ARM_100_REG','EQ_REG_RANK','Volatility_185_0','DY',
                   'CFY','EY','eEY','SAL_eGrowth','EPS_eGrowth',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Value' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'DY','CFY','EY','eEY',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Quality' %in% only & only_bench==T){
      port=port[,c('ID','Date',
                   'EQ_REG_RANK','Volatility_185_0',
                   'L1_NA','L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    if('Momentum' %in% only & only_bench==T){
      port=port[,c('ID','Date','Mom_365_30',
                   'ARM_100_REG','SAL_eGrowth','EPS_eGrowth','L1_NA',
                   'L1_10','L1_15','L1_20','L1_25','L1_30','L1_35','L1_40',
                   'L1_45','L1_50','L1_55','L1_60')]
    }
    testa_port=copy(port)
    testa_port[,entries:=1]
    testa_port[,entries_per_date:=sum(entries),by=c('Date')]
    testa_port=unique(testa_port,by=c('Date'))[,c('Date','entries_per_date')]
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    (length(names(port)))
    port[is.na(port)]=0
    og_port=port
    port=as.matrix(port)
    port=xgb.DMatrix(data=port,group=c(testa_port$entries_per_date))
    
    if('All' %in% GICS){
      preds=-1*predict(XGBMS,newdata = port)
      port=og_port
      port$XGBMS=preds
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBMS')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBMS'),new_names = c('XGBMS'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBMS',top_bot_pct = maker,pds=test_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBMS was successful')
      
    }
    else{
      var_name=paste('XGBM_',j,sep='')
      print(paste('Starting',var_name))
      
      preds=predict(eval(parse(text=var_name)),newdata = port)
      port=og_port
      port$XGBM=preds
      port=cbind(ids,port)
      abc=port[,c('ID','Date','XGBM')]
      port14=to_100_rank(port,cols_to_apply_to = c('XGBM'),new_names = c('XGBM'))
      portfolio14=Portfolio_maker(port14,cols_to_apply_to = 'XGBM',top_bot_pct = 20,pds=train_pds)
      portfolio=merge(portfolio,portfolio14,by=c('ID','Date'))
      
      print('XGBM was successful')
    }
  }
  
  if('PCA_XGBM' %in% num_of_algos){
    require(xgboost)
    set.seed(123456)
    print('Starting PCA_XGBM Model')
    port=copy(as.data.frame(all_data))
    port=as.data.table(port)
    ids=port[,c('ID','Date')]
    port[,`:=`(ID=NULL,Date=NULL)]
    y_vals=port$ActiveFwdRet
    port[,ActiveFwdRet:=NULL]
    port[is.na(port)]=0
    port=h2o::as.h2o(port)
    
    new_port=h2o::h2o.predict(pca_ed,port)
    port=as.matrix(new_port)
    
    preds=predict(PCA_XGBM,port)
    port=as.data.table(port)
    port$PCA_XGBM=preds
    port=cbind(ids,port)
    abc=port[,c('ID','Date','PCA_XGBM')]
    port15=to_100_rank(port,cols_to_apply_to = c('PCA_XGBM'),new_names = c('PCA_XGBM'))
    portfolio15=Portfolio_maker(port15,cols_to_apply_to = 'PCA_XGBM',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio15,by=c('ID','Date'))
    
    print('PCA_XGBM was successful')
  }
  
  if('GXGBM' %in% num_of_algos){
    port123=copy(portfolio)
    port123[,GXGBM:=(GBM*0.3)+(XGBM*0.7)]
    port123=port123[,c('ID','Date','GXGBM')]
    port15=to_100_rank(port123,cols_to_apply_to = c('GXGBM'),new_names = c('GXGBM'))
    portfolio15=Portfolio_maker(port15,cols_to_apply_to = 'GXGBM',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio15,by=c('ID','Date'))
    
    print('GXGBM was successful')
    
    
  }
  
  if(harmony==T){
    port=portfolio
    port[,Harmony:=(0.5*XGBM)+(0.5*Basic_Mean),]
    port=port[,c('ID','Date','Harmony')]
    port=to_100_rank(port,cols_to_apply_to = c('Harmony'),new_names = c('Harmony'))
    portfolio1233=Portfolio_maker(port,cols_to_apply_to = 'Harmony',top_bot_pct = 20,pds=train_pds)
    portfolio=merge(portfolio,portfolio1233,by=c('ID','Date'))
    
    print('Algorithms are harmonized')
    
  }
  
  return(portfolio)
}



EZ_RNN_REG_Train=function(train_data,y_col,step_to_go_back=5,units_in_first=600,
                          units_in_second=600,drop_data=0.25,iterations=150){
  print("Here's a quick guide:\n data=you input the data here, make sure that besides the ID, and Date
        The other columns have already been aggregated on a per Date basis.\n  y_col is the y column you are
        trying to solve for.\n  Step_to_go_back is how many dates back you want the RNN to study")
  set.seed(123456)
  require(keras)
  require(tensorflow)
  port=as.data.table(train_data)
  setorder(port,Date,ID)
  step=step_to_go_back
  port=unique(port,by=c('Date'))
  
  #Split output from inputs, kick out date, and ID. then run TimeseriesGenerator from keras, batch_size=1,
  #   and length=step_size
  Ys=port[Date==123456789,c(y_col),with=F][,`:=`(ID=NULL,Date=NULL)]
  Xs=port[Date==123456789,!c(y_col),with=F][,`:=`(ID=NULL,Date=NULL)]
  port=port[,`:=`(ID=NULL,Date=NULL)]
  
  x<<-Xs
  y<<-Ys
  portie<<-port
  sample=length(unique(train_data$Date))-step
  for(l in seq(1,sample)){
    print(paste('Doing the first',l,'groups (rolling)'))
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
  EZ_RNN<<- keras_model_sequential() %>%
    bidirectional(layer_lstm(units=units_in_first,return_sequences = T),input_shape=c(step,n_col)) %>%  #Change units only
    layer_dropout(drop_data) %>% #Change the number to whatever
    bidirectional(layer_lstm(units=units_in_second,return_sequences = F)) %>%  #Change units only
    layer_dropout(drop_data) %>% #Change the number to whatever
    layer_dense(units=1) #Don't touch this
  EZ_RNN %>% compile(loss = 'mse',optimizer = 'adam') #Change optimizer & loss, to whatever is allowed on keras,
  filepath="weights.best.hdf5"
  dank_back=callback_model_checkpoint(filepath,monitor='val_loss',verbose=1,save_best_only = T,save_weights_only = T,mode='min',period=3)
  EZ_RNN  %>% fit(X,Y,epochs=iterations,validation_split=0.20,verbose=1,callbacks=list(dank_back),batch_size=200)
  EZ_RNN %>% load_model_weights_hdf5(filepath = filepath)
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
  setorder(port,Date,ID)
  step_size=step_to_go_back
  
  test_set=all_d[Date>=as.Date(test_data_start_date) & Date<=as.Date(test_data_end_date)]
  
  #Data prep for RNN
  port=unique(port,by=c('Date'))
  setorder(port,Date,ID)
  port[,Num:=1]
  port[,Posn:=cumsum(Num)]
  starting_OS_date=as.Date(test_data_start_date)
  port[Date==starting_OS_date,Position:=Posn]
  #Sending the last out one as the y sample, AKA, what we want to predict on
  #Oldie
  position=mean(port$Position,na.rm = T)-step_size-1
  sample=length(unique(test_set$Date))
  Xs=port[Date==123456789,!c(y_col),with=F][,`:=`(ID=NULL,Date=NULL,Position=NULL,Num=NULL,Posn=NULL)]
  port=port[,`:=`(ID=NULL,Date=NULL,Position=NULL,Num=NULL,Posn=NULL)][,!c(y_col),with=F]
  print(paste('First Date in OS:',starting_OS_date))
  for(l in seq(1,sample)){
    print(paste('Doing the first',l,'groups (rolling)'))
    first_in=port[l+seq(0,step_size)+position,]
    Xs=rbind(Xs,first_in)
  }
  x=Xs
  n_col=ncol(x)
  x=as.matrix(x)
  x[is.infinite(x) | is.na(x) | is.nan(x)]=0
  #Ys[,RNN_CLASS:=0][Cor_Mom<0,RNN_CLASS:=1] #Class
  #y=to_categorical(Ys$RNN_CLASS,num_classes = 2) #Class
  
  
  #Time to re-arrange X and Y appropriately (to array)
  X=array(x,dim=c(sample,step_size,n_col))
  print('Starting RNN Predictions')
  set.seed(123456)
  RNN_Preds=EZ_RNN %>% predict(X) #Class
  Mo_Preds=RNN_Preds
  print(paste('Preds:',Mo_Preds))
  port_dates=unique(all_data,by=c('Date'))[Date>=as.Date(test_data_start_date) & Date<=as.Date(test_data_end_date)]
  port_dates[,RNN_Predictions:=RNN_Preds]
  port_dates=port_dates[,c('Date','RNN_Predictions')]
  port_dates=merge(all_data,port_dates,by=c('Date'))
  
}

EZ_RNN_REG=function(all_data,y_col,back_steps,test_data_start_date,test_data_end_date,
                    units_in_first=600,units_in_second=600,drop_data=0.25,iterations=150){
  training=all_data[Date<as.Date(test_data_start_date)]
  training_chunk=EZ_RNN_REG_Train(training,y_col,step_to_go_back = back_steps,units_in_first,units_in_second,
                                  drop_data,iterations)
  
  testing_chunk=EZ_RNN_REG_Test(all_data,y_col,step_to_go_back = back_steps,test_data_start_date,test_data_end_date)
  return(testing_chunk)
}