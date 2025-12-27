package com.gdn.x.productcategorybase.config;

import java.lang.reflect.Method;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler;

public class AsyncExceptionHandler implements AsyncUncaughtExceptionHandler {
  private static final Logger LOGGER = LoggerFactory.getLogger(AsyncExceptionHandler.class);

  @Override
  public void handleUncaughtException(Throwable throwable, Method method, Object... obj) {
    LOGGER.error("Exception message : {}, Method name : {} , Exception : {}", throwable.getMessage(),
        method.getName(), throwable);
    for (Object param : obj) {
      LOGGER.error("Parameter value - {}", param);
    }
  }
}
