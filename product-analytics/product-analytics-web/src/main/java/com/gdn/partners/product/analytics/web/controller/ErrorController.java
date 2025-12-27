package com.gdn.partners.product.analytics.web.controller;


import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import com.blibli.oss.common.error.ErrorControllerHandler;
import com.blibli.oss.common.response.Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.product.analytics.service.impl.exception.ClientException;
import com.gdn.partners.product.analytics.service.impl.exception.FileException;
import com.gdn.partners.product.analytics.service.impl.exception.ValidationException;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestControllerAdvice
public class ErrorController implements ErrorControllerHandler {

  @Getter
  @Autowired
  private MessageSource messageSource;

  @Getter
  private Logger logger = log;

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ClientException.class)
  public Response<Object> clientException(ClientException e) {
    log.error(e.getMessage(), e);
    Response<Object> response = new Response<>();
    response.setCode(HttpStatus.INTERNAL_SERVER_ERROR.value());
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.name());
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(FileException.class)
  public Response<Object> fileException(FileException e) {
    log.error(e.getMessage(), e);
    Response<Object> response = new Response<>();
    response.setCode(HttpStatus.INTERNAL_SERVER_ERROR.value());
    response.setStatus(HttpStatus.INTERNAL_SERVER_ERROR.name());
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ApplicationRuntimeException.class)
  public BaseResponse applicationRuntimeException(ApplicationRuntimeException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    response.setSuccess(false);
    return response;
  }

  @ExceptionHandler(ValidationException.class)
  public GdnBaseRestResponse handleValidationException(ValidationException e, HttpServletRequest request,
      HttpServletResponse response) {
    response.setStatus(HttpStatus.OK.value());
    response.setContentType(MediaType.APPLICATION_JSON_VALUE);
    log.error(e.getMessage(), e);
    GdnBaseRestResponse responseBody = new GdnBaseRestResponse();
    responseBody.setSuccess(false);
    responseBody.setErrorCode(ErrorCategory.VALIDATION.getCode());
    responseBody.setErrorMessage(e.getMessage());
    responseBody.setRequestId(request.getParameter("requestId"));
    return responseBody;
  }

}
