package com.gdn.micro.graphics.config;

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
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;

/**
 * Created by yudhi.k.surtan on 9/16/2016.
 */
@RestControllerAdvice(basePackages = {"com.gdn.x","com.gdn.micro"})
public class GraphicsGlobalAdvise extends ResponseEntityExceptionHandler {

  private static final Logger LOG = LoggerFactory.getLogger(GraphicsGlobalAdvise.class);

  @ExceptionHandler(value = {ApplicationException.class})
  @ResponseBody
  public GdnBaseRestResponse handleApplicationException(ApplicationException applicationException,
      HttpServletResponse response) {
    String errorMessage = applicationException.getErrorMessage();
    GraphicsGlobalAdvise.LOG.error(errorMessage, applicationException);
    LOG.info(""+ErrorCategory.DATA_NOT_FOUND.equals(applicationException.getErrorCodes()));
    if(ErrorCategory.DATA_NOT_FOUND.equals(applicationException.getErrorCodes())) {
      response.setStatus(HttpStatus.NOT_FOUND.value());
    } else {
      response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
    }
    return new GdnBaseRestResponse.Builder().setSuccess(false)
        .setErrorCode(applicationException.getErrorCodes().getCode()).setErrorMessage(errorMessage)
        .build();
  }

  @ExceptionHandler(value = {ApplicationRuntimeException.class})
  @ResponseBody
  public GdnBaseRestResponse handleApplicationRuntimeException(
      ApplicationRuntimeException applicationRuntimeException, HttpServletResponse response) {
    String errorMessage = applicationRuntimeException.getErrorMessage();
    GraphicsGlobalAdvise.LOG.error(errorMessage, applicationRuntimeException);
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
    return new GdnBaseRestResponse.Builder().setSuccess(false)
        .setErrorCode(applicationRuntimeException.getErrorCodes().getCode())
        .setErrorMessage(errorMessage).build();
  }

  @Override
  protected ResponseEntity<Object> handleExceptionInternal(Exception exception, Object body,
      HttpHeaders headers, HttpStatusCode status, WebRequest request) {
    String message = exception.getMessage();
    GraphicsGlobalAdvise.LOG.error(message, exception);
    return new ResponseEntity<>(
        new GdnBaseRestResponse.Builder().setSuccess(false)
        .setErrorCode(ErrorCategory.UNSPECIFIED.getCode())
        .setErrorMessage(ErrorCategory.UNSPECIFIED.getMessage() + message).build(),
        headers, status);
  }

  @ExceptionHandler(value = {Throwable.class})
  @ResponseBody
  public GdnBaseRestResponse handleThrowable(Throwable throwable, HttpServletResponse response) {
    String message = throwable.getMessage();
    GraphicsGlobalAdvise.LOG.error(message, throwable);
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
    return new GdnBaseRestResponse.Builder().setSuccess(false)
        .setErrorCode(ErrorCategory.UNSPECIFIED.getCode()).setErrorMessage(message).build();
  }

}
