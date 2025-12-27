package com.gdn.partners.pcu.external.web.sample;

import java.util.HashSet;

import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import javax.validation.Path;
import javax.validation.metadata.ConstraintDescriptor;

import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.impl.exception.ApplicationException;
import com.gdn.partners.pcu.external.service.impl.exception.ClientException;
import com.gdn.partners.pcu.external.service.impl.exception.ImageNotFoundException;
import com.gdn.partners.pcu.external.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.external.service.impl.exception.InvalidStateException;
import com.google.zxing.WriterException;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
public class SampleController {

  @GetMapping(value = "/sample/clientException")
  public void clientException() {
    throw new ClientException(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE);
  }

  @GetMapping(value = "/sample/imageNotFoundException")
  public void imageNotFoundException() {
    throw new ImageNotFoundException(ErrorMessages.IMAGE_NOT_FOUND);
  }

  @GetMapping(value = "/sample/imageValidationException")
  public void imageValidationException() {
    throw  new ImageValidationException(ErrorMessages.IMAGE_VALIDATION_ERR_MESSAGE);
  }

  @GetMapping(value = "/sample/applicationException")
  public void applicationException() {
    throw new ApplicationException(HttpStatus.BAD_REQUEST, ApiErrorCode.ITEM_IS_SUSPENDED.getCode(),
        ApiErrorCode.ITEM_IS_SUSPENDED.getDesc());
  }

  @GetMapping(value = "/sample/writerException")
  public void writerException() throws WriterException {
    throw new WriterException(ErrorMessages.WRITER_EXCEPTION_ERR_MESSAGE);
  }

  @GetMapping(value = "/sample/applicationRuntimeException")
  public void applicationRuntimeException() throws ApplicationRuntimeException {
    throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
  }

  @GetMapping(value = "/sample/invalidStateException")
  public void invalidStateException() {
    throw new InvalidStateException(ErrorMessages.INVALID_STATE);
  }

  @GetMapping(value = "/sample/constraintViolationException")
  public void constraintViolationException() {
    ConstraintViolation<String> constraintViolation = getStringConstraintViolation();
    HashSet<ConstraintViolation<String>> constraintViolationSet = new HashSet<>();
    constraintViolationSet.add(constraintViolation);
    throw new ConstraintViolationException(HttpStatus.BAD_REQUEST.name(), constraintViolationSet);
  }

  @GetMapping(value = "/sample/validationException")
  public void validationException() {
    throw new ValidationException(ErrorMessages.INVALID_ATTRIBUTE);
  }

  private ConstraintViolation<String> getStringConstraintViolation() {
    return new ConstraintViolation<String>() {
      @Override
      public String getMessage() {
        return "message";
      }

      @Override
      public String getMessageTemplate() {
        return "message template";
      }

      @Override
      public String getRootBean() {
        return null;
      }

      @Override
      public Class<String> getRootBeanClass() {
        return null;
      }

      @Override
      public Object getLeafBean() {
        return null;
      }

      @Override
      public Object[] getExecutableParameters() {
        return new Object[0];
      }

      @Override
      public Object getExecutableReturnValue() {
        return null;
      }

      @Override
      public Path getPropertyPath() {
        return null;
      }

      @Override
      public Object getInvalidValue() {
        return null;
      }

      @Override
      public ConstraintDescriptor<?> getConstraintDescriptor() {
        return null;
      }

      @Override
      public <U> U unwrap(Class<U> aClass) {
        return null;
      }
    };
  }
}