package com.gdn.x.productcategorybase.controller;

import com.gdn.x.productcategorybase.dto.response.BaseResponse;
import com.gdn.x.productcategorybase.exception.ValidationException;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.handler.ErrorControllerHandler;

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
  @ExceptionHandler(ApplicationRuntimeException.class)
  public BaseResponse applicationRuntimeException(ApplicationRuntimeException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(e.getErrorCodes()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.OK)
  @ExceptionHandler(ValidationException.class)
  public BaseResponse validationException(ValidationException e){
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getErrorMessage());
    response.setErrorCode(e.getErrorCode());
    response.setSuccess(false);
    return response;
  }
}
