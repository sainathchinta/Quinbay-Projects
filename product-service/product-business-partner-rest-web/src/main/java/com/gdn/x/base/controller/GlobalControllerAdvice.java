package com.gdn.x.base.controller;

import jakarta.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;

/**
 * handles app specific and spring internal exceptions
 */
@ControllerAdvice(basePackages = {"com.gdn"})
public class GlobalControllerAdvice extends ResponseEntityExceptionHandler {
  private static final Logger LOG = LoggerFactory.getLogger(GlobalControllerAdvice.class);
  private static final String ERROR_MSG_VIEW = "Error occurred on the system";

  private void writeErrorLog(String errorMessage, Exception e) {
    GlobalControllerAdvice.LOG.error(LoggerStandard.appendLogErrorTemplate(errorMessage, e), e);
  }

  private void writeErrorLog(String errorMessage, Throwable e) {
    GlobalControllerAdvice.LOG.error(LoggerStandard.appendLogErrorTemplateThrowable(errorMessage, e), e);
  }

  @ExceptionHandler(value = {ApplicationException.class})
  @ResponseBody
  public GdnBaseRestResponse handleApplicationException(ApplicationException applicationException,
      HttpServletResponse response) {
    String errorMessage = applicationException.getErrorMessage();

    writeErrorLog(errorMessage, applicationException);
    response.setStatus(HttpStatus.OK.value());
    return new GdnBaseRestResponse.Builder().setSuccess(false)
        .setErrorCode(applicationException.getErrorCodes().getCode()).setErrorMessage(errorMessage).build();
  }

  @ExceptionHandler(value = {ApplicationRuntimeException.class})
  @ResponseBody
  public GdnBaseRestResponse handleApplicationRuntimeException(ApplicationRuntimeException applicationRuntimeException,
      HttpServletResponse response) {
    String errorMessage = applicationRuntimeException.getErrorMessage();
    writeErrorLog(errorMessage, applicationRuntimeException);
    response.setStatus(HttpStatus.OK.value());
    return new GdnBaseRestResponse.Builder().setSuccess(false)
        .setErrorCode(applicationRuntimeException.getErrorCodes().getCode()).setErrorMessage(errorMessage).build();
  }

  @Override
  protected ResponseEntity<Object> handleExceptionInternal(Exception exception, Object body, HttpHeaders headers,
      HttpStatusCode status, WebRequest request) {
    String message = exception.getMessage();
    writeErrorLog(message, exception);

    return new ResponseEntity<>(
        new GdnBaseRestResponse.Builder().setSuccess(false).setErrorCode(ErrorCategory.UNSPECIFIED.getCode())
            .setErrorMessage(ERROR_MSG_VIEW).build(), headers, status);
  }

  @ExceptionHandler(value = {Throwable.class})
  @ResponseBody
  public GdnBaseRestResponse handleThrowable(Throwable throwable, HttpServletResponse response) {
    String message = throwable.getMessage();
    writeErrorLog(message, throwable);
    response.setStatus(HttpStatus.OK.value());
    return new GdnBaseRestResponse.Builder().setSuccess(false).setErrorCode(ErrorCategory.UNSPECIFIED.getCode())
        .setErrorMessage(ERROR_MSG_VIEW).build();
  }
}