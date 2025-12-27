package com.gdn.partners.pcu.internal.web.sample;

import java.io.IOException;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.exception.ApplicationException;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageNotFoundException;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import io.swagger.annotations.Api;

@Api
@RestController
public class SampleController {

  @GetMapping(value = "/sample/clientException")
  public void clientException() {
    throw new ClientException(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE);
  }

  @GetMapping(value = "/sample/constraintViolation")
  public void constraintViolationException() {
    throw new ConstraintViolationException(ErrorMessages.CONSTRAINT_EXCEPTION_ERROR_MESSAGE);
  }

  @GetMapping(value = "/sample/imageNotFoundException")
  public void imageNotFoundException() {
    throw new ImageNotFoundException(ErrorMessages.IMAGE_NOT_FOUND);
  }

  @GetMapping(value = "/sample/invalidStateException")
  public void invalidStateException() {
    throw new InvalidStateException(ErrorMessages.BRAND_HAS_PRODUCTS_MAPPED);
  }

  @GetMapping(value = "/sample/imageNotValidException")
  public void imageValidationException() {
    throw new ImageValidationException(ErrorMessages.IMAGE_VALIDATION_ERR_MESSAGE);
  }

  @GetMapping(value = "/sample/applicationException")
  public void applicationException() {
    throw new ApplicationException(ErrorMessages.ERR_INVALID_RESPONSE);
  }


  @GetMapping(value = "/sample/applicationRuntimeException")
  public void applicationRuntimeException() throws ApplicationRuntimeException {
    throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_REQUEST);
  }

  @GetMapping(value = "/sample/ioException")
  public void ioException() throws IOException {
    throw new IOException(ErrorMessages.ERR_INVALID_RESPONSE);
  }
}
