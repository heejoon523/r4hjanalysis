library(RPostgreSQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(tableone)
library(forestplot)

hj_DB_write <- function(TBNAME, DF, COPY_ROWNAMES=TRUE, LOWER_COLNAMES=TRUE)
{
  con <- hj_DB_con()
  dbSendQuery(con, paste0("drop table if exists ", TBNAME))
  # df<-hj_tableone_add_thousands_comma(DF)
  # df<-hj_tableone_reformat(DF)
  if(LOWER_COLNAMES) colnames(DF)<-tolower(colnames(DF))
  if(COPY_ROWNAMES) {
    DF <- cbind(rownames(DF), DF)
    names(DF)[1] <- "rownames"
  }
  print (TBNAME)
  dbWriteTable(con, TBNAME, DF, row.names=FALSE)
  dbDisconnect(con)
  return (DF)
}

hj_TableOne_addThousandsSeperator <-function(DF)
{
  df_values <- data.frame(lapply(DF, function(x) { # DF의 col 1열씩 넘김
    DF_col<-t(as.data.frame(x)) # DF의 col 1개 내의 row 별로 1행씩 넘김
    res<-t(data.frame(lapply(DF_col,function(x){ # comma 붙이고 행열 변환
      res <- x %>%
        gsub("\\(","( ", .) %>% # 괄호 전후에 space 삽입해서 split 가능하게 함
        gsub(")"," )",.) %>%
        # gsub("\\[","( ", .) %>%
        # gsub("\\]"," )",.) %>%
        gsub("\\[","[ ", .) %>%
        gsub("]"," ]",.) %>%
        gsub(","," - ",.) %>%
        strsplit(., split="\\s") %>%
        unlist %>%
        # gsub(".00$", "", .) %>%
        # ifelse(grepl("\\<",.),.,gsub(".00$", "", .)) %>%
        ifelse(grepl("\\.",.),.,gsub("(?!^)(?=(?:\\d{3})+$)", ",", ., perl=T)) %>%
        paste(., collapse=" ") %>%
        gsub("\\s","",.) %>%
        gsub("\\("," (",.) %>%
        gsub("\\["," [",.)
      return (res)
    })))
    return (res)
  }))
  df <- cbind(rownames=row.names(DF), df_values) # row 행 이름을 별도의 변수로 저장
  colnames(df)<- tolower(colnames(df)) # col 열 이름을 소문자로 (pgsql과 excel 연결용)
  rownames(df)<- NULL # row 행 이름은 없앰
  return(df)
}

hj_TableOne_reformat <-function(DF, COLS=3, SVY=FALSE)
{
  df<-hj_TableOne_addThousandsSeperator(DF)
  tdf <- as.data.frame(t(df), stringsAsFactors = FALSE)
  COLS<-COLS+1
  # tdf2<-gsub("\\(","",tdf)
  print(tdf)
  tdf_result <- lapply(tdf,function(x){
    row_values <-x
    row_name<-row_values[1]
    # row_values[2:COLS] <- gsub("\\(|\\)","",row_values[2:COLS])

    if (grepl("^n$",row_name)) {row_values[2:COLS]=paste0("(n=",row_values[2:COLS],")")}
    else if (grepl("\\(mean \\(SD\\)\\)",row_name)) {row_values[2:COLS]=gsub("\\(|\\)","",gsub(" ","±", row_values[2:COLS]))}
    else if (grepl("\\median \\[IQR\\]\\)",row_name)) {row_values[2:COLS]=gsub("\\[","(",gsub("\\]",")", row_values[2:COLS]))}
    # else if (grepl("\\(\\%\\)",row_name)) {row_values[2:COLS]=gsub(")","%)",row_values[2:COLS])}
    else if (grepl("\\(\\%\\)",row_name)) {
      if (SVY) {
        row_values[2:COLS]=gsub(")","%",row_values[2:COLS])
        row_values[2:COLS]=gsub("^.*[(]","",row_values[2:COLS])
      }
      else {row_values[2:COLS]=gsub(")","%)",row_values[2:COLS])}
    }
    else  {row_values[2:COLS]=gsub(")","%)",row_values[2:COLS])}

    # # plt, ALT에서 .0 없애기
    if(row_name %in% c("plt_a (median [IQR])","alt_a (median [IQR])", "rituximab_cumdose (median [IQR])"))
    {
      row_values[2:COLS] <- gsub(".0 "," ",row_values[2:COLS])
      row_values[2:COLS] <- gsub(".0)",")",row_values[2:COLS])
      row_values[2:COLS] <- gsub(".0-","-",row_values[2:COLS])
    }
    return(row_values)
  })
  res<-data.frame(t(data.frame(tdf_result)))
  colnames(res)<-colnames(df)
  rownames(res)<-NULL

  # p를 소수점 3자리 -> 2자리 기준으로 변경 ** input이 3자리가 아닐 경우 함수 변경 필요
  res$p2<-res$p
  res$p<- hj_reformatDigit_p(res$p)
  return(res)
}

hj_reformatDigit_p <- function(P_VALUES, pDigits_from=3, pDigits_to=2)
{
  # p 표현 조정
  # input digit은 3자리
  # 원칙
  # 0.01보다 크면 2자리로
  # 0.01 ~ 0.001은 3자리로
  # 0.001보다 작으면 <0.001로
  # 변형
  # 0.050과 같거나 크면 2자리로
  # 0.045-0.050는 3자리로
  # 0.045-0.010은 2자리로
  # 0.01 ~ 0.001은 3자리로
  # 0.001보다 작으면 <0.001로

  # hj_reformatDigit_p(c("<0.001","","0.135","0.125","0.123","0.012","0.001","0.050","0.049","0.046","0.045","0.044"))
  # hj_reformatDigit_p(c("0.051","0.050","0.049","0.046","0.045","0.044", "0.012", "0.011", "0.010", "0.009","0.008", "0.001"))
  # p_values <- as.character(P_VALUES)
  # print("P_VALUES")
  # print(P_VALUES)
  p_values <- as.character(P_VALUES)

  # p <- lapply(p_values, function(x){
  #    if(x=="<0.001" | x=="" | x=="NaN") return (x)
  #   x<-as.numeric(x)
  #    print(paste("x=",x))
  #   if (x>=0.050) x<-round(x,digits=2)
  #   else if (x<=0.045 & x>=0.010) x<-round(x,digits=2)
  #   x<-as.character(x)
  #   return(x)
  # })

  # ?format
  pDigits_adj<-10^(3-pDigits_from)
  p <- lapply(p_values, function(x){
    if(grepl("<",x) | x=="" | x=="NaN" | x=="NA") return (x)
    # print(paste("x=",x))
    x<-as.numeric(x)
    # print ("x")
    # print (x)
    if (x>=0.050*pDigits_adj) x<-round(x,digits=pDigits_to)
    else if (x<=0.045*pDigits_adj & x>=0.010*pDigits_adj) x<-round(x,digits=pDigits_to)
    # x<-as.character(x)
    x<-format(x, nsmall=pDigits_to)
    return(x)
  })

  res<-unlist(p)
  return(res)
}
# forest_result_uni <- hj_forest_list(forest_data, forest_vars, "hcc", "hbeag_a", cox_covs, MUST_COV="hbeag_a", P_THRESHOLD=0.05, multiCox=FALSE)


hj_TableOne_DBwrite <- function(DB_TABLE_NAME, DATA, STRATA, VARS, VARS_NONNORMAL=NULL, OVERALL=FALSE, SMD=FALSE, SVY=FALSE, pDigits=3, showAllLevels = FALSE)
{
  if(SVY) tb1 <- svyCreateTableOne(vars=VARS, data=DATA, strata = STRATA, addOverall=OVERALL)
  else tb1 <- CreateTableOne(vars=VARS, strata = STRATA, data=DATA, addOverall=OVERALL)
  tb1_print <- as.data.frame(print(tb1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE, nonnormal = VARS_NONNORMAL, smd=SMD, pDigits=pDigits, contDigits=1, showAllLevels = showAllLevels))
  tb1_print_reformat <- hj_TableOne_reformat(tb1_print, SVY=SVY)
  hj_DB_write(DB_TABLE_NAME, tb1_print_reformat, COPY_ROWNAMES=FALSE)
  return (tb1_print_reformat)
}

# hj_TableOne_DBwrite("tb_iptw_tb1", IPTWdataLC, "HBeAg", tb1_vars, tb1_vars_nonnormal, OVERALL = TRUE, SMD=TRUE, SVY=TRUE)

hj_cox_multi_covariates <- function(df_data, time, event, covariates, p_threshold, WEIGHT=NULL, MUST_COV=NULL)
{
  if (!is.numeric(event)) event<-as.numeric(event)
  hj_cox_formulas <- sapply(covariates, function(x) as.formula(paste('Surv(time, event)~', x)))

  if (is.null(WEIGHT)) hj_cox_models <- lapply(hj_cox_formulas, function(x){coxph(x, data = df_data)})
  else hj_cox_models <- lapply(hj_cox_formulas, function(x){coxph(x, data = df_data, weights = df_data$w)})

  hj_cox_results <- lapply(hj_cox_models, function(x){
    x <- summary(x)
    p_value<-x$wald["pvalue"]
    #p.value<-round(x$wald["pvalue"],digit=dgt)
    return (p_value)
  })
  t(as.data.frame(hj_cox_results, check.names = TRUE)) %>%
    as.data.frame %>%
    subset(pvalue<p_threshold) %>%
    rownames -> res_covariates

  # if(!is.null(MUST_COV)) res_covariates <- as.character(unlist(merge(data.frame(cov=res_covariates), data.frame(cov=MUST_COV), key=cov, all=TRUE)))
  if(!is.null(MUST_COV)) res_covariates <- as.character(unlist(merge(data.frame(cov=res_covariates), data.frame(cov=MUST_COV), key=cov, all=TRUE)))

  res_covariates %>% paste0(collapse = "+") -> res
  # paste0(collapse = "+") %>% return
  # paste0(collapse = "+") -> res


  print(paste("covariates:",res))
  return (res)
}

hj_cox <- function(df_data, t_time, t_event, COV, multiCox=FALSE, dgt=2, p_threshold=0.2, WEIGHT=NULL, DETAILED=FALSE, ROBUST_VAR=NULL, MUST_COV=NULL, pDigits=3)
{
  df_name <- deparse(substitute(df_data))
  covariates <- COV
  # plt_a 는 제외 ----
  if (multiCox==TRUE) covariates <- COV[!(COV %in% c("plt_a"))]

  time <- unlist(df_data[t_time])
  event <- unlist(df_data[t_event])
  if (!is.null(WEIGHT)) df_data$w<-unlist(df_data[WEIGHT])

  if (!is.numeric(event)) event<-as.numeric(event)
  if (!multiCox) print("--- Univariable Cox Analysis ---")
  if (multiCox) print("### Multivariable Cox Analysis ###")
  print(paste("data: ", df_name, " P threshold:", p_threshold))
  #if (multiCox) covariates <-paste0(covariates,collapse="+")
  if (multiCox) covariates <-hj_cox_multi_covariates(df_data, time, event, covariates, p_threshold, WEIGHT, MUST_COV=MUST_COV)

  # if(!is.null(MUST_COV)) covariates <- as.character(unlist(merge(data.frame(cov=covariates), data.frame(cov=MUST_COV), key=cov, all=TRUE)))
  # print(covariates)
  # print(paste("*ESS:",MUST_COV))
  if (!is.null(ROBUST_VAR)) covariates <- paste0(covariates, "+cluster(", ROBUST_VAR, ")")
  print(paste0("time: ", t_time, " event: ", t_event))
  hj_cox_formulas <- sapply(covariates, function(x) as.formula(paste('Surv(time, event)~', x)))
  if (is.null(WEIGHT)) hj_cox_models <- lapply(hj_cox_formulas, function(x){coxph(x, data = df_data)})
  else  hj_cox_models <- lapply(hj_cox_formulas, function(x){coxph(x, data = df_data, weights = df_data$w)})

  hj_cox_results <- lapply(hj_cox_models, function(x){
    coxsmry <- summary(x)

    # 자리수 안보정
    # HR <-format(round(coxsmry$coef[,"exp(coef)"], digits=dgt), nsmall=dgt)
    # HR.confint.lower <- format(round(coxsmry$conf.int[,"lower .95"],dgt), nsmall=dgt)
    # HR.confint.upper <- format(round(coxsmry$conf.int[,"upper .95"],dgt), nsmall=dgt)

    # 자리수 보정
    {
      HR.confint.lower <- format(round(coxsmry$conf.int[,"lower .95"],dgt), nsmall=dgt)
      HR.confint.upper <- format(round(coxsmry$conf.int[,"upper .95"],dgt), nsmall=dgt)
      digits <- rep(dgt, times=length(HR.confint.lower))
      digits[HR.confint.lower==HR.confint.upper]<-dgt+1

      HRs<-coxsmry$coef[,"exp(coef)"]
      CIs_lower<-coxsmry$conf.int[,"lower .95"]
      CIs_upper<-coxsmry$conf.int[,"upper .95"]
      coxsmry2<-as.data.frame(cbind(HRs, CIs_lower, CIs_upper, digits))
      for (i in 1:nrow(coxsmry2))
      {
        coxsmry2[i,"HRs2"] <- format(round(coxsmry2[i,"HRs"],coxsmry2[i,"digits"]), nsmall=coxsmry2[i,"digits"])
        coxsmry2[i,"CIs_lower2"] <- format(round(coxsmry2[i,"CIs_lower"],coxsmry2[i,"digits"]), nsmall=coxsmry2[i,"digits"])
        coxsmry2[i,"CIs_upper2"] <- format(round(coxsmry2[i,"CIs_upper"],coxsmry2[i,"digits"]), nsmall=coxsmry2[i,"digits"])
      }
      HR <- coxsmry2$HRs2
      HR.confint.lower <- coxsmry2$CIs_lower2
      HR.confint.upper <- coxsmry2$CIs_upper2
    }

    # print(coxsmry2)
    HR <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    # p.value<-round(x$coef[,"Pr(>|z|)"], digits=dgt)
    p_value<-coxsmry$coef[,"Pr(>|z|)"]

    sig<-p_value
    sig[p_value>=0.1]<-" "
    sig[p_value<0.1]<-"."
    sig[p_value<0.05]<-"*"
    sig[p_value<0.01]<-"**"
    sig[p_value<0.001]<-"***"

    p<-format(round(p_value, digits=pDigits), nsmall=pDigits)
    p[p_value<(0.1^pDigits)]<-paste0("<0.",paste0(rep("0",pDigits-1),collapse=""),"1")
    #p[p.value>=(0.1^dgt)]<-format(p.value, nsmall=dgt)
    varname <- rownames(coxsmry$coefficients)

    if (DETAILED) {
      HRmed <- coxsmry$coef[,"exp(coef)"]
      HRlower <- coxsmry$conf.int[,"lower .95"]
      HRupper <- coxsmry$conf.int[,"upper .95"]
      res<-rbind(HR, p, sig, p_value, varname, HRmed, HRlower, HRupper)
      # res<-rbind(HR, p, sig, p.value, varname, HR.num, HR.confint.lower, HR.confint.upper)
    }
    else
    {
      res<-rbind(HR, p, sig, p_value, varname)
    }
    return(res)
  })
  res <- as.data.frame(t(as.data.frame(hj_cox_results, check.names = TRUE)))
  # rownames(res)<-unlist(lapply(rownames(res), function(x) {tail(unlist(strsplit(x,"\\.")),1)}))
  rownames(res)<-res$varname
  # res$varname <- NULL

  # p를 소수점 3자리 -> 2자리 기준으로 변경 ** input이 3자리가 아닐 경우 함수 변경 필요
  res$p2<-res$p
  res$p<- hj_reformatDigit_p(res$p)

  return(res)
}

# hj_cox_DBwrite("tbcox1", myfacdataLC,"month_hcc_os1", "hcc", cox_cov, P_THRESHOLD=0.05)
# format(1.234560, nsmall=2)

hj_cox_DBwrite <- function(DB_TABLE_NAME, DATA, TIME, EVENT, COX_COV, P_THRESHOLD=0.2, WEIGHT=NULL, ROBUST_VAR=NULL, MUST_COV=NULL){
  res_cox_uni <- hj_cox(DATA, TIME, EVENT, COX_COV, WEIGHT=WEIGHT, ROBUST_VAR=ROBUST_VAR)
  print(res_cox_uni)
  res_cox_multi <- hj_cox(DATA, TIME, EVENT, COX_COV, multiCox=TRUE, p_threshold=P_THRESHOLD, WEIGHT=WEIGHT, ROBUST_VAR=ROBUST_VAR, MUST_COV=MUST_COV)
  print(res_cox_multi)
  hj_DB_write(paste0(DB_TABLE_NAME,"u"), res_cox_uni)
  hj_DB_write(paste0(DB_TABLE_NAME,"m"), res_cox_multi)
  return(TRUE)
}
hj_PSM_match.data <- function(MATCHIT_OBJ){
  PSM_data <- match.data(MATCHIT_OBJ)
  PSM_matrix <- MATCHIT_OBJ$match.matrix %>% na.omit
  ID_treats <- rownames(PSM_matrix)
  ID_controls <- (PSM_matrix)
  print(length(ID_treats))

  PSM_data$PSM_ID <- NULL
  for (i in 1:length(ID_treats))
  {
    PSM_data[PSM_data$ID==ID_treats[i],"PSM_ID"] <- i
    PSM_data[PSM_data$ID==ID_controls[i],"PSM_ID"] <- i + 0.1
  }
  res <- PSM_data[order(PSM_data$PSM_ID),]
  res$PSM_ID <- floor(res$PSM_ID)
  return(res)
}
# ?survfit.object

# forest_result <- hj_forest_list(forest_data, forest_vars, "hcc", "hbeag_a", cox_vars)
hj_figure_CumInc <- function(DF_DATA, MONTH_TIME, EVENT, STRATA, WEIGHT=NULL, SUBTITLE=NULL, BREAK_TIME_BY=24, FONT_SIZE=NULL, PALETTE="Lancet", MONTH_TO_YEAR=FALSE) {
  df <- DF_DATA
  df_name <- deparse(substitute(DF_DATA))
  time <- df[MONTH_TIME]
  if(MONTH_TO_YEAR) df[MONTH_TIME]<-time/12
  surv_formula_txt <- paste0("Surv(",MONTH_TIME,",",EVENT,")~",STRATA)
  surv_formula <- as.formula(surv_formula_txt)
  print(surv_formula_txt)
  fit <- surv_fit(as.formula(surv_formula_txt), data=df) # survfit으로 하면 symbol 에러
  if(!is.null(WEIGHT)) fit <- surv_fit(as.formula(surv_formula_txt), data=df, weights=eval(parse(text=paste0(df_name,"$",WEIGHT)))) # survfit으로 하면 symbol 에러

  LINETYPE <- c(1,5)
  PALETTE <- c("red", "black")

  res<-NULL


  # if(is.null(FONT_SIZE)){
  #   res$plot<-ggsurvplot(fit, size=1, break.time.by = BREAK_TIME_BY, fun="event", palette = PALETTE, linetype = LINETYPE, censor.size=1, censor.shape="|",
  #                        title="Cumulative incidence of HCC development", subtitle=SUBTITLE, xlab="Time (months)")
  # }
  # else {
  #   res$plot<-ggsurvplot(fit, size=FONT_SIZE/10, break.time.by = BREAK_TIME_BY, fun="event", palette = PALETTE, linetype = LINETYPE, censor.size=FONT_SIZE/10, censor.shape="|",
  #                        # title="Cumulative incidence of HCC development", subtitle=SUBTITLE,
  #                        xlab="", ylab="",
  #                        # xlab="Time (months)", ylab="Cumulative incidence (%)",
  #                        surv.scale="percent",
  #
  #                        font.legend = c(FONT_SIZE, "plain", "black"),
  #                        legend="none",
  #                        ggtheme=theme_survminer(
  #                          font.main = c(FONT_SIZE, "bold", "black"),
  #                          font.submain = c(FONT_SIZE, "plain", "black"),
  #                          font.caption = c(FONT_SIZE, "plain", "black"),
  #                          font.x = c(FONT_SIZE, "bold", "black"),
  #                          font.y = c(FONT_SIZE, "bold", "black"),
  #                          font.tickslab = c(FONT_SIZE, "plain", "black")
  #                        ))
  # }
  res$risktable <- ggrisktable(fit, data=df, break.time.by = BREAK_TIME_BY)
  res$DB_risktable <- res$risktable$data %>% select(strata, time, n.risk)
  res$DB_risktable <- as.data.frame(cast(res$DB_risktable, strata~time, value="n.risk"))
  res$plot_full <- ggsurvplot(fit, size=1, title="Cumulative incidence of HCC development", subtitle=SUBTITLE,
                              break.time.by = BREAK_TIME_BY, risk.table = TRUE, fun="event", palette = PALETTE, pval.method=TRUE, pval = TRUE,
                              censor.size=2, censor.shape="|", xlab="Time (months)")
  # res$risktable_all <- ggrisktable(fit, data=df, break.time.by = 1, risk.table.type="nrisk_cumevents")

  INTEREST_YEARS <- c(2,5,10)
  # risktable_interest <- res$risktable_all$data %>%
  #   subset(time %in% INTEREST_YEARS) %>%
  #   mutate(cumInc = cum.n.event / n.risk,
  #          cumInc_percent = round(cumInc*100, digits=1))
  #
  #
  # print(risktable_interest)
  #
  # res$smy <- summary(fit, times=INTEREST_YEARS)

  smry <- summary(fit, times=INTEREST_YEARS)
  # df_smry <- data.frame(strata=tmp2$strata, time=tmp2$time, surv=tmp2$surv, cuminc_pct=(1-tmp2$surv)*100, cumhaz=tmp2$cumhaz*100)
  df_smry <- data.frame(strata_cumIncPct=smry$strata, time=smry$time+0.01, cuminc_pct=round((1-smry$surv)*100,digits=1))
  res$DB_cumIncPctTable <- as.data.frame(cast(df_smry, strata_cumIncPct~time, value="cuminc_pct"))

  res$DB_risktable2 <- cbind(res$DB_risktable, res$DB_cumIncPctTable)

  res$plot<-ggsurvplot(fit, legend.labs=c("HBeAg-positive","HBeAg-negative"), fun = "event",
                       break.time.by = BREAK_TIME_BY,
                       palette = c("red","black"), linetype = c(1,5), censor=FALSE,
                       xlab="Time (years)", ylab="Cumulative incidence",
                       risk.table = T, risk.table.y.text.col=FALSE, tables.theme = theme_cleantable(),
                       tables.height = 0.2,  xlim = c(0,12)
  )

  if(is.null(FONT_SIZE)){
    res$plot<-ggsurvplot(fit, size=1, break.time.by = BREAK_TIME_BY, fun="event", palette = PALETTE, linetype = LINETYPE, censor.size=1, censor.shape="|",
                         title="Cumulative incidence of HCC development", subtitle=SUBTITLE, xlab="Time (months)")
  }
  else {
    res$plot<-ggsurvplot(fit, size=FONT_SIZE/15, break.time.by = BREAK_TIME_BY, fun="event", palette = PALETTE, linetype = LINETYPE, censor=FALSE,
                         # xlab="", ylab="",
                         xlab="Time (years)", ylab="Cumulative incidence (%)",
                         legend.labs=c("HBeAg-positive","HBeAg-negative"),
                         surv.scale="percent",

                         font.legend = c(FONT_SIZE, "plain", "black"),
                         legend="none",
                         ggtheme=theme_survminer(
                           font.main = c(FONT_SIZE+1, "bold", "black"),
                           font.submain = c(FONT_SIZE+1, "plain", "black"),
                           font.caption = c(FONT_SIZE+1, "plain", "black"),
                           font.x = c(FONT_SIZE+1, "bold", "black"),
                           font.y = c(FONT_SIZE+1, "bold", "black"),
                           font.tickslab = c(FONT_SIZE, "plain", "black")
                         ),
                         risk.table = T, risk.table.y.text.col=FALSE, tables.theme = theme_cleantable()
                         ,tables.height = 0.25,  xlim = c(0,12)
                         ,fontsize=2.5
    )
  }
  return(res)
}

hj_figure_CumInc_DBwrite <- function(DB_TABLE_NAME, DF_DATA, MONTH_TIME, EVENT, STRATA, WEIGHT=NULL, SUBTITLE=NULL, BREAK_TIME_BY=24, FONT_SIZE=NULL, PALETTE="XXX", MONTH_TO_YEAR=FALSE) {
  res <- hj_figure_CumInc(DF_DATA, MONTH_TIME, EVENT, STRATA, SUBTITLE=SUBTITLE, WEIGHT=WEIGHT, BREAK_TIME_BY=BREAK_TIME_BY, FONT_SIZE=FONT_SIZE, PALETTE=PALETTE, MONTH_TO_YEAR=MONTH_TO_YEAR)
  # hj_DB_write(DB_TABLE_NAME, res$DB_risktable, COPY_ROWNAMES = FALSE)
  hj_DB_write(DB_TABLE_NAME, res$DB_risktable2, COPY_ROWNAMES = FALSE)
  if(is.null(FONT_SIZE)){
    pic_size_x=8
    pic_size_y=6
  }
  else {
    pic_size_x=FONT_SIZE*1.5
    pic_size_y=FONT_SIZE
  }
  pic_size_x=9
  pic_size_y=8.5

  ggsave(plot=print(res$plot), file=paste0(DB_TABLE_NAME,".tiff"), width=pic_size_x, height=pic_size_y, unit="cm", dpi=300)
  return(res)
}

hj_logrank <- function(DF_DATA, TIME, EVENT, STRATA, STRATIFIED_VAR=NULL, WEIGHT=NULL, ROUND_DIGIT=NULL) {
  print(deparse(substitute(DF_DATA)))
  res <- as.list(NULL)
  surv_formula_txt <- paste0("Surv(",TIME,",",EVENT,")~",STRATA)
  # surv_formula <- as.formula(surv_formula_txt)
  print(surv_formula_txt)
  tmp<-summary(coxph(as.formula(surv_formula_txt), data = DF_DATA))
  res$logrank <- tmp$logtest["pvalue"]

  if(!is.null(STRATIFIED_VAR)) {
    surv_formula_txt <- paste0(surv_formula_txt, "+strata(", STRATIFIED_VAR,")")
    print(surv_formula_txt)
    tmp<-summary(coxph(as.formula(surv_formula_txt), data = DF_DATA))
    res$logrank_stratified <- tmp$logtest["pvalue"]
  }
  if(!is.null(WEIGHT)) {
    tmp<-summary(coxph(as.formula(surv_formula_txt), data = DF_DATA, weights=eval(parse(text=WEIGHT))))
    res$logrank_weighted <- tmp$logtest["pvalue"]
  }
  res<-as.data.frame(res)
  if(!is.null(ROUND_DIGIT)) res <- round(res, digits=ROUND_DIGIT)
  return(res)
}




hj_forest_list <- function(DF, SUBGROUP_VARs, EVENT_VAR, INTEREST_VAR, COX_VARS, MUST_COV=NULL, P_THRESHOLD=0.05, multiCox=FALSE) {
  res1 <- c(TRUE, NA,  NA, "Number of patients", NA, NA, NA, NA, NA)
  res2 <- c(TRUE, "Subgroup", "HBeAg-negative", "HBeAg-positive", "HR (95% CI)", "P value", NA, NA, NA)
  if (multiCox==TRUE) res2 <- c(TRUE, "Subgroup", "HBeAg-negative", "HBeAg-positive", "adjusted HR (95% CI)", "P value", NA, NA, NA)
  # res2 <- c(TRUE, "Subgroup", "HBeAg-positive1", "HBeAg-negative", "aHR (95% CI)", "P value", NA, NA, NA)
  forest_elements <- lapply(SUBGROUP_VARs, function(x) {hj_forest_element(DF, x[2], x[1], EVENT_VAR, INTEREST_VAR, COX_VARS, MUST_COV=MUST_COV, P_THRESHOLD=P_THRESHOLD, multiCox=multiCox)})
  res99<-do.call(rbind, lapply(forest_elements, data.frame, stringsAsFactors=FALSE))
  return (rbind(res1, res2, res99))
}

hj_forest_element <- function(DF, SUBGROUP_VAR, SUBGROUP_NAME, EVENT_VAR, INTEREST_VAR, COX_VARS, MUST_COV=NULL, P_THRESHOLD=0.05, multiCox=FALSE) {
  df <- DF
  df$subgroup_var <- unlist(df[SUBGROUP_VAR])
  if(!is.factor(df$subgroup_var)) { df$subgroup_var <- as.factor(df$subgroup_var) }

  results <- sapply(levels(df$subgroup_var), function(x) {
    print(paste0(SUBGROUP_VAR,"=",x))
    res <- NULL
    df2<-df %>% subset(.$subgroup_var==x)
    interest0e <- nrow(df2 %>% subset(.[INTEREST_VAR]==0 & .[EVENT_VAR]==1) )
    interest0n <- nrow(df2 %>% subset(.[INTEREST_VAR]==0) )
    interest1e <- nrow(df2 %>% subset(.[INTEREST_VAR]==1 & .[EVENT_VAR]==1) )
    interest1n <- nrow(df2 %>% subset(.[INTEREST_VAR]==1) )
    cox_cov <- COX_VARS

    # res_cox_multi <- hj_cox(df2, "month_hcc_os1", "hcc", cox_cov, TRUE, dgt=2, p_threshold=1, DETAILED=TRUE) %>% subset (varname==paste0(INTEREST_VAR,"1"))
    if (!is.null(MUST_COV)) res_cox_multi <- hj_cox(df2, "month_hcc_os1", "hcc", cox_cov, multiCox=multiCox, dgt=2, p_threshold=P_THRESHOLD, DETAILED=TRUE, MUST_COV=MUST_COV) %>% subset (varname==paste0(INTEREST_VAR,"1"))
    else res_cox_multi <- hj_cox(df2, "month_hcc_os1", "hcc", cox_cov, multiCox=multiCox, dgt=2, p_threshold=P_THRESHOLD, DETAILED=TRUE) %>% subset (varname==paste0(INTEREST_VAR,"1"))

    # print("res_cox_multi")
    # print(res_cox_multi)

    res$is.title <- FALSE
    res$name <- x
    res$interest0num <- paste0(interest0e,"/",interest0n," (",round(interest0e/interest0n*100, digit=2),"%)")
    res$interest1num <- paste0(interest1e,"/",interest1n," (",round(interest1e/interest1n*100, digit=2),"%)")
    res$aHR <- as.character(res_cox_multi$HR)
    # res$p_value <- as.numeric(as.character(res_cox_multi$p))
    res$p_value <- as.character(res_cox_multi$p)
    res$HRmed <- as.numeric(as.character(res_cox_multi$HRmed))
    res$HRlower <- as.numeric(as.character(res_cox_multi$HRlower))
    res$HRupper <- as.numeric(as.character(res_cox_multi$HRupper))
    return(res)
  })
  res1 <- data.frame(is.title=TRUE,
                     # name=as.character(SUBGROUP_NAME),
                     name=SUBGROUP_NAME,
                     interest0num=NA,
                     interest1num=NA,
                     aHR=NA,
                     p_value=NA,
                     HRmed=NA,
                     HRlower=NA,
                     HRupper=NA
  )
  res1$name <- as.character(res1$name)
  res2 <- as.data.frame(t(as.data.frame(results)))
  res <- rbind(res1,res2)
  return (res)
}
