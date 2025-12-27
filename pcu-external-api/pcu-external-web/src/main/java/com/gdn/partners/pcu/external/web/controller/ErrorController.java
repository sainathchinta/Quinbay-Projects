package com.gdn.partners.pcu.external.web.controller;

import javax.validation.ConstraintViolationException;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.core.security.exception.UnauthorizedException;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.Response;
import com.gdn.partners.pcu.external.service.impl.exception.EditProductException;
import com.gdn.partners.pcu.external.service.impl.exception.ProductListingGenericException;
import com.gdn.partners.pcu.external.web.model.response.ApiErrorCode;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import feign.FeignException;
import org.slf4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.MessageSource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.MissingServletRequestParameterException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.method.annotation.MethodArgumentTypeMismatchException;
import org.springframework.web.multipart.MaxUploadSizeExceededException;
import org.springframework.web.multipart.MultipartException;
import org.springframework.web.multipart.support.MissingServletRequestPartException;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.impl.exception.ApplicationException;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.ImageNotFoundException;
import com.gdn.partners.pcu.external.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.external.service.impl.exception.InvalidStateException;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.web.handler.ErrorControllerHandler;
import com.google.zxing.WriterException;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.Optional;

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


  @Value("${spring.servlet.multipart.max-file-size}")
  private String springServletMultipartMaxFileSize;

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ClientException.class)
  public BaseResponse clientException(ClientException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE);
    response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    response.setSuccess(false);
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

  @ResponseStatus(HttpStatus.UNSUPPORTED_MEDIA_TYPE)
  @ExceptionHandler(ImageValidationException.class)
  public BaseResponse imageValidationException(ImageValidationException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.UNSUPPORTED_MEDIA_TYPE.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(ApplicationException.class)
  public BaseResponse applicationException(ApplicationException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(e.getCode()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(ConstraintViolationException.class)
  public BaseResponse constraintViolationException(ConstraintViolationException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getConstraintViolations().iterator().next().getMessageTemplate());
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(WriterException.class)
  public BaseResponse writerException(WriterException e) {
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
    if (ErrorCategory.VALIDATION.equals(e.getErrorCodes())) {
      response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    } else {
      response.setErrorCode(String.valueOf(HttpStatus.INTERNAL_SERVER_ERROR.value()));
    }
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
  @ExceptionHandler(ProductListingGenericException.class)
  public BaseResponse productListingGenericException(ProductListingGenericException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(e.getErrorCode());
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
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(ApiIncorrectInputDataException.class)
  public BaseResponse ApiInCorrectDataException(ApiIncorrectInputDataException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    if (e.getErrorCode().equals(com.gdn.mta.product.enums.ApiErrorCode.PRODUCT_LIMIT_REACHED.getCode())) {
      response.setErrorMessage(ErrorMessages.ERROR_PRODUCT_LIMIT_EXCEEDED);
    } else {
      response.setErrorMessage(e.getErrorMessage());
    }
    response.setErrorCode(e.getErrorCode());
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(EditProductException.class)
  public SingleBaseResponse<EditProductWebResponse> EditProductException(EditProductException e) {
    log.error(e.getMessage(), e);
    SingleBaseResponse<EditProductWebResponse> response = new SingleBaseResponse<EditProductWebResponse>();
    response.setValue(e.getEditProductWebResponse());
    response.setErrorMessage(e.getErrorMessage());
    response.setErrorCode(e.getErrorCode());
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(MissingServletRequestParameterException.class)
  public BaseResponse missingRequestParameterException(MissingServletRequestParameterException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(e.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(MethodArgumentTypeMismatchException.class)
  public BaseResponse methodArgumentTypeMismatchException(MethodArgumentTypeMismatchException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(ErrorMessages.INVALID_REQUEST);
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    response.setSuccess(false);
    return response;
  }

  @ExceptionHandler(FeignException.BadRequest.class)
  public ResponseEntity<BaseResponse> handleFeignBadRequest(FeignException.BadRequest ex) {
    log.error("Feign BadRequest: {}", ex.contentUTF8(), ex);
    BaseResponse response = new BaseResponse();
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    response.setSuccess(false);
    String errorBody = ex.contentUTF8();
    if (errorBody != null && !errorBody.isBlank()) {
      try {
        Response<?> feignResponse = new ObjectMapper().readValue(errorBody, new TypeReference<>() {
        });

        response.setErrorMessage(
          Optional.ofNullable(feignResponse.getErrors()).map(errors -> errors.get("message"))
            .filter(messages -> !messages.isEmpty()).map(List::getFirst)
            .orElse(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE));

      } catch (Exception parseEx) {
        response.setErrorMessage(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE);
      }
    } else {
      response.setErrorMessage(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE);
    }

    return ResponseEntity.badRequest().body(response);
  }
  
  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(MissingServletRequestPartException.class)
  public BaseResponse missingServletRequestPartException(MissingServletRequestPartException e) {
    log.error(e.getMessage(), e);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(ErrorMessages.INVALID_REQUEST);
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(MultipartException.class)
  public BaseResponse multipartException(MultipartException multipartException) {
    log.error(multipartException.getMessage(), multipartException);
    BaseResponse response = new BaseResponse();
    log.error(multipartException.getMessage());
    response.setErrorMessage(ErrorMessages.INVALID_FILE_TYPE_NOT_SUPPORTED);
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.BAD_REQUEST)
  @ExceptionHandler(MaxUploadSizeExceededException.class)
  public BaseResponse maxUploadSizeExceededException(
    MaxUploadSizeExceededException maxUploadSizeExceededException) {
    log.error(maxUploadSizeExceededException.getMessage(), maxUploadSizeExceededException);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(String.format(ApiErrorCode.INVALID_EXCEL_SIZE.getDesc(),
      springServletMultipartMaxFileSize));
    response.setErrorCode(String.valueOf(HttpStatus.BAD_REQUEST.value()));
    response.setSuccess(false);
    return response;
  }

  @ResponseStatus(HttpStatus.UNAUTHORIZED)
  @ExceptionHandler(UnauthorizedException.class)
  public BaseResponse unauthorizedException(
    UnauthorizedException maxUploadSizeExceededException) {
    log.error(maxUploadSizeExceededException.getMessage(), maxUploadSizeExceededException);
    BaseResponse response = new BaseResponse();
    response.setErrorMessage(ErrorCategory.AUTHORIZATION.getMessage());
    response.setErrorCode(String.valueOf(HttpStatus.UNAUTHORIZED.value()));
    response.setSuccess(false);
    return response;
  }
}
