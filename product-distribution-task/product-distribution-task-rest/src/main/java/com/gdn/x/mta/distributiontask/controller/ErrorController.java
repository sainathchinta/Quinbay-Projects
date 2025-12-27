package com.gdn.x.mta.distributiontask.controller;


import com.gdn.common.exception.ApplicationRuntimeException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestControllerAdvice(basePackages = {"com.gdn.x.mta.distributiontask", "com.gdn.partners.pdt"})
public class ErrorController {

  @ExceptionHandler(ValidationException.class)
  public GdnBaseRestResponse handleValidationException(ValidationException e,
      HttpServletRequest request, HttpServletResponse response) {
    response.setStatus(HttpStatus.OK.value());
    response.setContentType(MediaType.APPLICATION_JSON_VALUE);
    log.error(e.getMessage(), e);
    GdnBaseRestResponse responseBody = new GdnBaseRestResponse();
    responseBody.setSuccess(false);
    responseBody.setErrorCode(e.getErrorCode());
    responseBody.setErrorMessage(e.getErrorMessage());
    responseBody.setRequestId(request.getParameter("requestId"));
    return responseBody;
  }

  @ExceptionHandler({Throwable.class})
  public GdnBaseRestResponse handleThrowable(Throwable throwable, HttpServletResponse response) {
    String message = throwable.getMessage();
    log.error("#handleThrowable response: {}, message: {}", response, message, throwable);
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.value());
    response.setContentType(MediaType.APPLICATION_JSON_VALUE);
    return (new GdnBaseRestResponse.Builder()).setSuccess(false)
        .setErrorCode(com.gdn.common.enums.ErrorCategory.UNSPECIFIED.getCode())
        .setErrorMessage(com.gdn.common.enums.ErrorCategory.UNSPECIFIED.getMessage()).build();
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ApplicationRuntimeException.class)
  public GdnBaseRestResponse applicationRuntimeException(ApplicationRuntimeException e) {
    log.error(e.getMessage(), e);
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    response.setSuccess(false);
    return response;
  }
}
