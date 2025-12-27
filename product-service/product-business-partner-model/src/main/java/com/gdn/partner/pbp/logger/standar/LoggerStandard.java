package com.gdn.partner.pbp.logger.standar;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;

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
  private static String ASPECT = "aspect=";
  private static String ERROR_TYPE = "errorType=";
  private static String ERROR_MSG = "errorMsg=";
  
  private static String STATE = "wfState=";
  private static String PRODUCT_CODE = "productCode=";  
  
  /**
   * <p>To generate logger INFO standard for product workflow</p>
   * 
   * @param state
   * @param storeId
   * @param productCode
   * @return
   */
  public static String getProductWorkflowLog(String state, String storeId, String productCode){
    return STATE + state + DELIMITER +
        STORE_ID + storeId + DELIMITER + 
        PRODUCT_CODE + productCode;
  }
  
  /**
   * <p>To generate logger INFO standard</p>
   * 
   * @param loggerAttribute
   * @return
   */
  public static String getLogInfoTemplate(LoggerAttributeModel loggerAttribute){
    try{
      if(loggerAttribute.getRequest() != null){
        loggerAttribute.setRequest(loggerAttribute.getRequest().replace("=", ":"));
      }
        
      return  
          METHOD_NAME + loggerAttribute.getClassName().getClass().getSimpleName() + "." + loggerAttribute.getMethodName() + DELIMITER + 
          USERNAME + loggerAttribute.getUsername() + DELIMITER + 
          REQUEST_ID + loggerAttribute.getRequestId() + DELIMITER + 
          CLIENT_ID + loggerAttribute.getClientId() + DELIMITER + 
          STATUS + LoggerStatus.INFO.getValue() + DELIMITER + 
          DETAIL + loggerAttribute.getDetail() + DELIMITER +
          REQUEST + loggerAttribute.getRequest();
    } catch(Exception e){
      LOGGER.error("logger standar error: getLogInfoTemplate" + e.getMessage(), e);
    }
    
    return null;
  }
  
  /**
   * <p>To generate generic logger standard. It will be appended base on controller event, like: 
   * 1. when throwing error 
   * 2. or before returning success response (audit log)</p>
   * 
   * @param loggerAttribute
   * @return
   */
  public static String getGenericLogTemplate(LoggerAttributeModel loggerAttribute){
    try{
      return  
          METHOD_NAME + loggerAttribute.getClassName().getClass().getSimpleName() + "." + loggerAttribute.getMethodName() + DELIMITER + 
          USERNAME + loggerAttribute.getUsername() + DELIMITER + 
          REQUEST_ID + loggerAttribute.getRequestId() + DELIMITER + 
          CLIENT_ID + loggerAttribute.getClientId() + DELIMITER + 
          DETAIL + loggerAttribute.getDetail();
    } catch(Exception e){
      LOGGER.error("logger standar error: getGenericLogTemplate" + e.getMessage(), e);
    }
    
    return null;
  }
  
  /**
   * <p>To append audit log information from generic log template</p>
   * 
   * @param genericLogTemplate
   * @param
   * @param
   * @return
   */
  public static String appendAuditLogTemplate(String genericLogTemplate){
    return genericLogTemplate + DELIMITER +
        STATUS + LoggerStatus.SUCCESS.getValue();
  }
  
  /**
   * <p>To append log error information from generic log template</p>
   * 
   * @param
   * @param errorMessage
   * @param e
   * @return
   */
  public static String appendLogErrorTemplate(String errorMessage, Exception e){
    try{
      return MDC.get(LoggerParam.GENERIC_LOGGER.getParam()) + DELIMITER +
          STATUS + LoggerStatus.ERROR.getValue() + DELIMITER +
          ERROR_TYPE + e.getClass().getSimpleName() + DELIMITER +
          ERROR_MSG + errorMessage;
    } catch(Exception ex){
      LOGGER.error("logger standar error: appendLogErrorTemplate" + ex.getMessage(), ex);
    }
    
    return null;
  }
  
  public static String appendLogErrorTemplate(String errorMessage){
    try{
      return MDC.get(LoggerParam.GENERIC_LOGGER.getParam()) + DELIMITER +
          STATUS + LoggerStatus.ERROR.getValue() + DELIMITER +
          ERROR_MSG + errorMessage;
    } catch(Exception ex){
      LOGGER.error("logger standar error: appendLogErrorTemplate" + ex.getMessage(), ex);
    }
    
    return null;
  }
  
  /**
   * <p>To append log error information from generic log template</p>
   * 
   * @param
   * @param errorMessage
   * @param e
   * @return
   */
  public static String appendLogErrorTemplateThrowable(String errorMessage, Throwable e){
    return MDC.get(LoggerParam.GENERIC_LOGGER.getParam()) + DELIMITER +
        STATUS + LoggerStatus.ERROR.getValue() + DELIMITER +
        ERROR_TYPE + e.getClass().getSimpleName() + DELIMITER +
        ERROR_MSG + errorMessage;
  }
  
}
