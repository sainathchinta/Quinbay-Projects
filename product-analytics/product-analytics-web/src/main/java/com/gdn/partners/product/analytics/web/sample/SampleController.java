package com.gdn.partners.product.analytics.web.sample;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.model.ErrorMessages;
import com.gdn.partners.product.analytics.service.impl.exception.ClientException;
import com.gdn.partners.product.analytics.service.impl.exception.FileException;
import com.gdn.partners.product.analytics.service.impl.exception.ValidationException;

@Tag(name = "Sample Controller", description = "Sample Controller")
@RestController
public class SampleController {

  @GetMapping(value = "/sample/clientException")
  public void clientException() {
    throw new ClientException(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE);
  }

  @GetMapping(value = "/sample/fileException")
  public void fileException() {
    throw new FileException(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE, new ApplicationRuntimeException());
  }

  @GetMapping(value = "/sample/applicationRuntimeException")
  public void applicationRuntimeException() {
    throw new ApplicationRuntimeException();
  }

  @GetMapping(value = "/sample/validationException")
  public void validationException() {
    throw new ValidationException(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE);
  }

}
