package com.gdn.partners.pcu.master.web.sample;

import java.io.IOException;

import com.gdn.partners.pcu.master.service.impl.exception.ValidationException;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.exception.ApplicationException;
import com.gdn.partners.pcu.master.model.ErrorCodes;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.service.impl.exception.ActivationValidationException;
import com.gdn.partners.pcu.master.service.impl.exception.ClientException;
import com.gdn.partners.pcu.master.service.impl.exception.InvalidStateException;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Sample Controller")
@RestController
public class SampleController {

  @GetMapping(value = "/sample/clientException")
  public void clientException() {
    throw new ClientException(ErrorMessages.ERR_INVALID_RESPONSE);
  }

  @GetMapping(value = "/sample/clientException/invalidCategory")
  public void clientExceptionInvalidCategory() {
    throw new ClientException(ErrorCodes.INVALID_CATEGORY_ERROR.getErrorMessage());
  }

  @GetMapping(value = "/sample/invalidStateException")
  public void invalidStateException() {
    throw new InvalidStateException(ErrorMessages.ERR_INVALID_RESPONSE);
  }


  @GetMapping(value = "/sample/applicationException")
  public void applicationException() {
    throw new ApplicationException(ErrorMessages.ERR_INVALID_RESPONSE);
  }

  @GetMapping(value = "/sample/activateValidationException")
  public void activateValidationException() throws IOException {
    throw new ActivationValidationException(ErrorMessages.ERR_INVALID_RESPONSE);
  }

  @GetMapping(value = "/sample/validationException")
  public void validationException() throws IOException {
    throw new ValidationException(ErrorMessages.ERR_INVALID_RESPONSE);
  }

  @GetMapping(value = "/sample/applicationRuntimeException")
  public void applicationRunTimeException() throws IOException {
    throw new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION, ErrorMessages.UNAUTHORIZED_ERR_MESSAGE);
  }
}
