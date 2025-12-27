package com.gdn.partners.pcu.master.web.controller;

import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.service.impl.exception.ValidationException;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.MessageSource;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.master.model.ErrorCodes;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.service.impl.exception.ActivationValidationException;
import com.gdn.partners.pcu.master.service.impl.exception.ClientException;
import com.gdn.partners.pcu.master.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.master.web.handler.ErrorControllerHandler;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 02/11/2018 AD.
 */
@Slf4j
@RestControllerAdvice
public class ErrorController implements ErrorControllerHandler {

  @Getter
  @Autowired
  private MessageSource messageSource;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Getter
  private Logger logger = log;

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ClientException.class)
  public BaseResponse clientException(ClientException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    if (!StringUtils.isEmpty(e.getMessage())) {
      response.setErrorMessage(e.getMessage());
    } else {
      response.setErrorMessage(ErrorMessages.SYSTEM_ERROR_MESSAGE);
    }
    if (ErrorCodes.INVALID_CATEGORY_ERROR.getErrorMessage().equals(e.getMessage())) {
      response.setErrorCode(ErrorCodes.INVALID_CATEGORY_ERROR.getErrorCode());
    } else {
      response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    }
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ActivationValidationException.class)
  public BaseResponse activateValidationException(ActivationValidationException e) {
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
    response.setErrorCode(e.getErrorCodes().getCode());
    if (Constants.CAN_NOT_PROCESS_INVALID_INPUT_DATA_EXCEEDS_THE_SIZE_CHART_NAME_LENGTH.equals(
        e.getMessage())) {
      response.setErrorCode(ErrorCodes.SIZE_CHART_NAME_EXCEEDED_LENGTH.getErrorCode());
    }
    if (e.getMessage().contains(ErrorCodes.SIZE_CHART_CANNOT_BE_DELETED.getErrorMessage())) {
      response.setErrorCode(ErrorCodes.SIZE_CHART_CANNOT_BE_DELETED.getErrorCode());
    }
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

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(ValidationException.class)
  public BaseResponse validationException(ValidationException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(StringUtils.isBlank(e.getErrorCode()) ?
        String.valueOf(HttpStatus.BAD_REQUEST.value()) :
        e.getErrorCode());
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(MethodArgumentTypeMismatchException.class)
  public BaseResponse methodArgumentTypeMismatchException(MethodArgumentTypeMismatchException e){
    log.error(e.getMessage() + ", Logged in username: {}, clientId: {}",
        clientParameterHelper.getUsername(), clientParameterHelper.getClientId(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(ErrorMessages.SYSTEM_ERROR_MESSAGE);
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    return response;
  }
}
