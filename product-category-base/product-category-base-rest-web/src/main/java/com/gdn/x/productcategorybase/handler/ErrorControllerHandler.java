package com.gdn.x.productcategorybase.handler;

import static com.gdn.x.productcategorybase.ErrorMessage.SELLER_CODE_OR_BRAND_CODE_SHOULD_NOT_BE_NULL_OR_EMPTY;

import org.slf4j.Logger;
import org.springframework.context.MessageSource;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;

import com.gdn.x.productcategorybase.dto.response.BaseResponse;
import org.springframework.web.servlet.NoHandlerFoundException;

import javax.validation.ConstraintViolationException;

public interface ErrorControllerHandler {
  Logger getLogger();

  MessageSource getMessageSource();

  @ResponseStatus(HttpStatus.OK)
  @ExceptionHandler({NoHandlerFoundException.class})
  default BaseResponse noHandlerFoundException(NoHandlerFoundException e) {
    this.getLogger().warn(NoHandlerFoundException.class.getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.OK.name());
    response.setErrorCode(String.valueOf(HttpStatus.OK.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler({Throwable.class})
  default BaseResponse throwable(Throwable e) {
    this.getLogger().error(e.getClass().getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.INTERNAL_SERVER_ERROR.name());
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler({ConstraintViolationException.class})
  default BaseResponse constraintViolation(ConstraintViolationException e) {
    this.getLogger().error(e.getClass().getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(SELLER_CODE_OR_BRAND_CODE_SHOULD_NOT_BE_NULL_OR_EMPTY.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    return response;
  }
}
