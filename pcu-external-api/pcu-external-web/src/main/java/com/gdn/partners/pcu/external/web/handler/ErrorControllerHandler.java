package com.gdn.partners.pcu.external.web.handler;

import org.slf4j.Logger;
import org.springframework.context.MessageSource;
import org.springframework.http.HttpStatus;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.validation.BindException;
import org.springframework.web.HttpMediaTypeNotAcceptableException;
import org.springframework.web.HttpMediaTypeNotSupportedException;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.servlet.NoHandlerFoundException;

import com.gdn.partners.core.web.dto.BaseResponse;

/**
 * @author Pradeep Reddy
 */
public interface ErrorControllerHandler {
  Logger getLogger();

  MessageSource getMessageSource();

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler({MethodArgumentNotValidException.class})
  default BaseResponse methodArgumentNotValidException(MethodArgumentNotValidException e) {
    this.getLogger().warn(MethodArgumentNotValidException.class.getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.BAD_REQUEST.name());
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler({BindException.class})
  default BaseResponse bindException(BindException e) {
    this.getLogger().warn(BindException.class.getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.BAD_REQUEST.name());
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.NOT_FOUND)
  @ExceptionHandler({NoHandlerFoundException.class})
  default BaseResponse noHandlerFoundException(NoHandlerFoundException e) {
    this.getLogger().warn(NoHandlerFoundException.class.getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.NOT_FOUND.name());
    response.setErrorCode(String.valueOf(HttpStatus.NOT_FOUND.value()));
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

  @ResponseStatus(HttpStatus.METHOD_NOT_ALLOWED)
  @ExceptionHandler({HttpRequestMethodNotSupportedException.class})
  default BaseResponse httpRequestMethodNotSupportedException(HttpRequestMethodNotSupportedException e) {
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.BAD_REQUEST.name());
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler({HttpMessageNotReadableException.class})
  default BaseResponse httpMessageNotReadableException(HttpMessageNotReadableException e) {
    this.getLogger().warn(HttpMessageNotReadableException.class.getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.BAD_REQUEST.name());
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
  @ExceptionHandler({HttpMediaTypeNotSupportedException.class})
  default BaseResponse httpMediaTypeNotSupportedException(HttpMediaTypeNotSupportedException e) {
    this.getLogger().warn(HttpMediaTypeNotSupportedException.class.getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.UNSUPPORTED_MEDIA_TYPE.name());
    response.setErrorCode(String.valueOf(HttpStatus.UNSUPPORTED_MEDIA_TYPE.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.NOT_ACCEPTABLE)
  @ExceptionHandler({HttpMediaTypeNotAcceptableException.class})
  default BaseResponse httpMediaTypeNotAcceptableException(HttpMediaTypeNotAcceptableException e) {
    this.getLogger().warn(HttpMediaTypeNotAcceptableException.class.getName(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(HttpStatus.NOT_ACCEPTABLE.name());
    response.setErrorCode(String.valueOf(HttpStatus.NOT_ACCEPTABLE.value()));
    response.setSuccess(false);
    return response;
  }
}
