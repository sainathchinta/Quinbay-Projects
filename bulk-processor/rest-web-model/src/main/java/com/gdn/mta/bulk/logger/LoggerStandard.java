package com.gdn.mta.bulk.logger;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerStandard {
  
  private static final Logger LOGGER = LoggerFactory.getLogger(LoggerStandard.class);
  
  private static String DELIMITER = " | ";
  private static String METHOD_NAME = "methodName=";
  private static String BP_CODE = "bpCode=";
  private static String USERNAME = "username=";
  private static String REQUEST_ID = "requestId=";
  private static String STORE_ID = "storeId=";
  private static String CHANNEL_ID = "channelId=";
  private static String CLIENT_ID = "clientId=";
  private static String DETAIL = "detail=";
  private static String REQUEST = "request=";
  private static String STATUS = "status=";
  private static String ERROR_TYPE = "errorType=";
  private static String ERROR_CODE = "errorCode=";
  private static String ERROR_MSG = "errorMsg=";
  
  public static String getLogInfoTemplate(LoggerAttributeModel loggerModel){
    try{
      if(loggerModel.getRequest() != null){
        loggerModel.setRequest(loggerModel.getRequest().replace("=", ":"));
      }
        
      return  
          METHOD_NAME + loggerModel.getClassName().getClass().getSimpleName() + "." + loggerModel.getMethodName() + DELIMITER + 
          USERNAME + loggerModel.getUsername() + DELIMITER +
          BP_CODE + loggerModel.getBpCode() + DELIMITER +
          REQUEST_ID + loggerModel.getRequestId() + DELIMITER + 
          CLIENT_ID + loggerModel.getClientId() + DELIMITER + 
          STATUS + LoggerStatus.INFO.getValue() + DELIMITER + 
          DETAIL + loggerModel.getDetail() + DELIMITER +
          REQUEST + loggerModel.getRequest();
    } catch(Exception e){
      LOGGER.error("logger standar error: getLogInfoTemplate" + e.getMessage(), e);
    }
    
    return null;
  }
  
  public static String getLogErrorTemplate(LoggerAttributeModel loggerModel, Exception e){
    return  
        METHOD_NAME + loggerModel.getClassName().getClass().getSimpleName() + "." + loggerModel.getMethodName() + DELIMITER + 
        USERNAME + loggerModel.getUsername() + DELIMITER +
        BP_CODE + loggerModel.getBpCode() + DELIMITER +
        REQUEST_ID + loggerModel.getRequestId() + DELIMITER + 
        CLIENT_ID + loggerModel.getClientId() + DELIMITER + 
        STATUS + LoggerStatus.ERROR.getValue() + DELIMITER + 
        DETAIL + loggerModel.getDetail() + DELIMITER +
        ERROR_TYPE + e.getClass().getSimpleName() + DELIMITER +
        ERROR_MSG + e.getMessage();
  }
  
  public static String getSimpleLogErrorTemplate(LoggerAttributeModel loggerModel, Exception e){
    return  
        METHOD_NAME + loggerModel.getClassName().getClass().getSimpleName() + "." + loggerModel.getMethodName() + DELIMITER + 
        STATUS + LoggerStatus.ERROR.getValue() + DELIMITER + 
        ERROR_TYPE + e.getClass().getSimpleName() + DELIMITER +
        REQUEST + loggerModel.getRequest() + DELIMITER +
        ERROR_MSG + e.getMessage();
  }
  
  public static String getLogErrorTemplateWithErrorCode(LoggerAttributeModel loggerModel, Exception e, String errorCode){
    return  
        METHOD_NAME + loggerModel.getClassName().getClass().getSimpleName() + "." + loggerModel.getMethodName() + DELIMITER + 
        USERNAME + loggerModel.getUsername() + DELIMITER +
        BP_CODE + loggerModel.getBpCode() + DELIMITER +
        REQUEST_ID + loggerModel.getRequestId() + DELIMITER + 
        CLIENT_ID + loggerModel.getClientId() + DELIMITER + 
        STATUS + LoggerStatus.ERROR.getValue() + DELIMITER + 
        DETAIL + loggerModel.getDetail() + DELIMITER +
        ERROR_TYPE + e.getClass().getSimpleName() + DELIMITER +
        ERROR_CODE + errorCode + DELIMITER +
        ERROR_MSG + e.getMessage();
  }
  
  public static String convertErrorTemplate(Object object, String methodName, LoggerAttributeModel loggerModel, Exception e, Object message){
    if(loggerModel != null){
      return LoggerStandard.getLogErrorTemplate(loggerModel, e);
    } else {
      loggerModel = new LoggerAttributeModel(object, methodName, String.valueOf(message));
      return LoggerStandard.getSimpleLogErrorTemplate(loggerModel, e);
    }
  }
  
}