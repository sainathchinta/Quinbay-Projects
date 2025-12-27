package com.gdn.partners.pcu.internal.web.controller;

import java.io.IOException;

import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.exception.ApplicationException;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageNotFoundException;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.internal.web.handler.ErrorControllerHandler;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * @author Pradeep Reddy
 */
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
  public BaseResponse clientException(ClientException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(ConstraintViolationException.class)
  public BaseResponse constraintViolationException(ConstraintViolationException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.NOT_FOUND)
  @ExceptionHandler(ImageNotFoundException.class)
  public BaseResponse imageNotFoundException(ImageNotFoundException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(ErrorMessages.IMAGE_NOT_FOUND);
    response.setErrorCode(String.valueOf(HttpStatus.NOT_FOUND.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(InvalidStateException.class)
  public BaseResponse invalidStateException(InvalidStateException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ImageValidationException.class)
  public BaseResponse imageValidationException(ImageValidationException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ApplicationException.class)
  public BaseResponse applicationException(ApplicationException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(IOException.class)
  public BaseResponse ioException(IOException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    response.setSuccess(false);
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

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(ApiIncorrectInputDataException.class)
  public BaseResponse apiIncorrectInputDataException(ApiIncorrectInputDataException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getErrorMessage());
    response.setErrorCode(e.getErrorCode());
    response.setSuccess(false);
    return response;
  }
}
