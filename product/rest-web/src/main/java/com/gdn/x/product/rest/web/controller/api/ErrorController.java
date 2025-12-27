package com.gdn.x.product.rest.web.controller.api;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.product.service.exceptions.ValidationException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import com.gdn.common.exception.ApplicationRuntimeException;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestControllerAdvice(basePackages = {"com.gdn.x.product"})
public class ErrorController {

  @ExceptionHandler(ValidationException.class)
  public GdnBaseRestResponse handleValidationException(ValidationException e, HttpServletRequest request,
      HttpServletResponse response) {
    response.setStatus(HttpStatus.BAD_REQUEST.value());
    response.setContentType(MediaType.APPLICATION_JSON_VALUE);
    log.error(e.getMessage(), e);
    GdnBaseRestResponse responseBody = new GdnBaseRestResponse();
    responseBody.setSuccess(false);
    responseBody.setErrorCode(ErrorCategory.VALIDATION.getCode());
    responseBody.setErrorMessage(e.getMessage());
    responseBody.setRequestId(request.getParameter("requestId"));
    return responseBody;
  }

  @ExceptionHandler({ApplicationException.class})
  public GdnBaseRestResponse handleApplicationException(ApplicationException applicationException,
    HttpServletResponse response) {
    String errorMessage = applicationException.getErrorMessage();
    log.error(errorMessage, applicationException);
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
    response.setContentType(MediaType.APPLICATION_JSON_VALUE);
    return (new GdnBaseRestResponse.Builder()).setSuccess(false)
      .setErrorCode(applicationException.getErrorCodes().getCode())
      .setErrorMessage(applicationException.getErrorMessage()).build();
  }

  @ExceptionHandler({ApplicationRuntimeException.class})
  public GdnBaseRestResponse handleApplicationRuntimeException(
    ApplicationRuntimeException applicationRuntimeException, HttpServletResponse response) {
    String errorMessage = applicationRuntimeException.getErrorMessage();
    log.error(errorMessage, applicationRuntimeException);
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
    response.setContentType(MediaType.APPLICATION_JSON_VALUE);
    return (new GdnBaseRestResponse.Builder()).setSuccess(false)
      .setErrorCode(applicationRuntimeException.getErrorCodes().getCode())
      .setErrorMessage(applicationRuntimeException.getErrorMessage()).build();
  }

  @ExceptionHandler({Throwable.class})
  public GdnBaseRestResponse handleThrowable(Throwable throwable, HttpServletResponse response) {
    String message = throwable.getMessage();
    log.error("#handleThrowable response: {}, message: {}", response, message, throwable);
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
    response.setContentType(MediaType.APPLICATION_JSON_VALUE);
    return (new GdnBaseRestResponse.Builder()).setSuccess(false)
      .setErrorCode(ErrorCategory.UNSPECIFIED.getCode())
      .setErrorMessage(ErrorCategory.UNSPECIFIED.getMessage()).build();
  }


}
