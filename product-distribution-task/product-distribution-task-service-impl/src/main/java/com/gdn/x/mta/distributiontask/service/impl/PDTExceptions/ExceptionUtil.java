package com.gdn.x.mta.distributiontask.service.impl.PDTExceptions;

/**
 * Created by Vishal on 15/12/16.
 */
public final class ExceptionUtil {

  public static void checkConditions(boolean expression, String exceptionMsg, String exceptionCode)
      throws PDTGeneralException {
    if (!expression) {
      throw new PDTGeneralException(exceptionMsg, exceptionCode);
    }
  }

  public static void checkConditions(boolean expression, String exceptionMsg)
      throws PDTGeneralException {
    if (!expression) {
      throw new PDTGeneralException(exceptionMsg);
    }
  }
}
