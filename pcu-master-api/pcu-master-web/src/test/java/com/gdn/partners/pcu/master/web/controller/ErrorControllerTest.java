package com.gdn.partners.pcu.master.web.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import jakarta.servlet.ServletException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.ErrorCodes;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.web.helper.TestHelper;
import com.gdn.partners.pcu.master.web.sample.SampleController;

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class ErrorControllerTest extends TestHelper {

  @InjectMocks
  private SampleController sampleController;

  private static final String CLIENT_EXCEPTION_PATH = "/sample/clientException";
  private static final String CLIENT_EXCEPTION__INVALID_CATEGORY_PATH = "/sample/clientException/invalidCategory";
  private static final String INVALID_STATE_EXCEPTION_PATH = "/sample/invalidStateException";
  private static final String ACTIVATE_VALIDATION_EXCEPTION_PATH = "/sample/activateValidationException";
  private static final String APPLICATION_RUN_TIME_EXCEPTION_PATH = "/sample/applicationRuntimeException";
  private static final String VALIDATION_EXCEPTION_PATH = "/sample/validationException";
  private static final String HOST = "host";
  private static final String LOCALHOST = "localhost:";

  @Value("${local.server.port}")
  private int port;

  @BeforeEach
  void setUp() throws Exception {
    mockMvc = MockMvcBuilders.standaloneSetup(sampleController).build();
  }

  @Test
  void clientExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(CLIENT_EXCEPTION_PATH).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException e) {
      Assertions.assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, e.getCause().getMessage());
    }
  }

  @Test
  void applicationRuntimeExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(APPLICATION_RUN_TIME_EXCEPTION_PATH).header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException e) {
      Assertions.assertEquals(ErrorCategory.AUTHORIZATION.getMessage() + ErrorMessages.UNAUTHORIZED_ERR_MESSAGE,
          e.getCause().getMessage());
    }
  }

  @Test
  void clientExceptionInvalidCategoryTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(CLIENT_EXCEPTION__INVALID_CATEGORY_PATH).header(HOST, LOCALHOST + String.valueOf(port))
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException e) {
      Assertions.assertEquals(ErrorCodes.INVALID_CATEGORY_ERROR.getErrorMessage(), e.getCause().getMessage());
    }
  }

  @Test
  void invalidStateException() throws Exception {
    MockHttpServletRequestBuilder requestBuilder = get(INVALID_STATE_EXCEPTION_PATH)
        .header(HOST, LOCALHOST + String.valueOf(port))
        .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException e) {
      Assertions.assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, e.getCause().getMessage());
    }
  }

  @Test
  void activateValidationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder = get(ACTIVATE_VALIDATION_EXCEPTION_PATH)
        .header(HOST, LOCALHOST + String.valueOf(port))
        .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException e) {
      Assertions.assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, e.getCause().getMessage());
    }
  }

  @Test
  void validationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder = get(VALIDATION_EXCEPTION_PATH)
      .header(HOST, LOCALHOST + String.valueOf(port))
      .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException e) {
      Assertions.assertEquals(ErrorMessages.ERR_INVALID_RESPONSE, e.getCause().getMessage());
    }
  }
}
