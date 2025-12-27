package com.gdn.partners.pcu.internal.web.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import java.io.IOException;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageNotFoundException;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.internal.service.impl.exception.InvalidStateException;
import jakarta.servlet.ServletException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.util.NestedServletException;

import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.sample.SampleController;

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class ErrorControllerTest extends TestHelper {

  private static final String CLIENT_EXCEPTION_PATH = "/sample/clientException";
  private static final String CONSTRAINT_VIOLATION_EXCEPTION_PATH = "/sample/constraintViolation";
  private static final String IMAGE_NOT_FOUND_EXCEPTION_PATH = "/sample/imageNotFoundException";
  private static final String INVALID_STATE_EXCEPTION_PATH = "/sample/invalidStateException";
  private static final String IMAGE_NOT_VALID_EXCEPTION = "/sample/imageNotValidException";
  private static final String APPLICATION_EXCEPTION = "/sample/applicationException";
  private static final String IO_EXCEPTION = "/sample/ioException";
  private static final String APPLICATION_RUNTIME_EXCEPTION = "/sample/applicationRuntimeException";
  private static final String API_INCORRECT_INPUT_DATA_EXCEPTION = "/sample/apiIncorrectInputDataException";
  private static final String HOST = "host";
  private static final String LOCALHOST = "localhost:";

  @Value("${local.server.port}")
  private int port;

  @InjectMocks
  private SampleController sampleController;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(sampleController).build();
  }

  @Test
  public void clientExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(CLIENT_EXCEPTION_PATH).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    try {
      mockMvc.perform(requestBuilder);
    }catch (ServletException e){
      Assertions.assertEquals(ErrorMessages.CLIENT_EXCEPTION_ERROR_MESSAGE , e.getCause().getMessage());
    }

  }

  @Test
  public void constraintViolationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(CONSTRAINT_VIOLATION_EXCEPTION_PATH)
            .header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    }catch (ServletException e){
      Assertions.assertEquals(ErrorMessages.CONSTRAINT_EXCEPTION_ERROR_MESSAGE , e.getCause().getMessage());
    }
  }

  @Test
  public void imageNotFoundExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(IMAGE_NOT_FOUND_EXCEPTION_PATH)
            .header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    }catch (ServletException e){
      Assertions.assertEquals(ErrorMessages.IMAGE_NOT_FOUND , e.getCause().getMessage());
    }
  }

  @Test
  public void invalidStateException() throws Exception {
    MockHttpServletRequestBuilder requestBuilder = get(INVALID_STATE_EXCEPTION_PATH)
            .header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    }catch (ServletException e){
      Assertions.assertEquals(ErrorMessages.BRAND_HAS_PRODUCTS_MAPPED , e.getCause().getMessage());
    }
  }

  @Test
  public void imageValidationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(IMAGE_NOT_VALID_EXCEPTION)
            .header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    }catch (ServletException e){
      Assertions.assertEquals(ErrorMessages.IMAGE_VALIDATION_ERR_MESSAGE , e.getCause().getMessage());
    }
  }

  @Test
  public void applicationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(APPLICATION_EXCEPTION)
            .header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    }catch (ServletException e){
      Assertions.assertEquals(ErrorMessages.ERR_INVALID_RESPONSE , e.getCause().getMessage());
    }
  }


  @Test
  public void ioExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(IO_EXCEPTION)
            .header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    }catch (IOException e){
      Assertions.assertEquals(ErrorMessages.ERR_INVALID_RESPONSE , e.getMessage());
    }
  }

  @Test
  public void applicationRuntimeException() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(APPLICATION_RUNTIME_EXCEPTION)
            .header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    }catch (ServletException e){
      Assertions.assertEquals(ErrorMessages.INVALID_REQUEST , e.getCause().getMessage().substring(36));
    }
  }

}