package com.gdn.partners.pcu.external.web.controller;


import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.sample.SampleController;
import jakarta.servlet.ServletException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.web.util.NestedServletException;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class ErrorControllerTest extends TestHelper {

  private static final String CLIENT_EXCEPTION_PATH = "/sample/clientException";
  private static final String WRITER_EXCEPTION_PATH = "/sample/writerException";
  private static final String PRODUCT_LISTING_GENERIC_PATH = "/sample/productListingGenericException";
  private static final String IMAGE_NOT_FOUND_EXCEPTION_PATH = "/sample/imageNotFoundException";
  private static final String IMAGE_VALIDATION_EXCEPTION = "/sample/imageValidationException";
  private static final String APPLICATION_EXCEPTION = "/sample/applicationException";
  private static final String CONSTRAINT_VIOLATION_EXCEPTION = "/sample/constraintViolationException";
  private static final String APPLICATION_RUNTIME_EXCEPTION = "/sample/applicationRuntimeException";
  private static final String INVALID_STATE_EXCEPTION = "/sample/invalidStateException";
  private static final String INVALID_ATTRIBUTE_EXCEPTION = "/sample/validationException";

  private static final String INVALID_PRODUCT_DIMENSION = "/sample/apiIncorrectDataException";
  private static final String EMPTY_IMAGE_FILE = "/sample/missingServletRequestPartException";
  private static final String HOST = "host";
  private static final String LOCALHOST = "localhost:";
  private static final String PRODUCT_LISTING_GENERIC_ERR_MESSAGE =
      "Mohon maaf, telah terjadi kesalahan pada sistem, namun kami sedang \"\n"
          + "      + \"memperbaikinya. Mohon kembali dalam beberapa saat dan coba lagi";

  @Value("${local.server.port}")
  private int port;

  @InjectMocks
  private SampleController sampleController;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    mockMvc = standaloneSetup(this.sampleController).build();
  }

  @Test
  public void clientExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(CLIENT_EXCEPTION_PATH)
            .header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }

  @Test
  public void imageNotFoundExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(IMAGE_NOT_FOUND_EXCEPTION_PATH)
            .header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }

  @Test
  public void imageValidationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(IMAGE_VALIDATION_EXCEPTION)
            .header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }

  @Test
  public void applicationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(APPLICATION_EXCEPTION).header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }


  @Test
  public void constraintViolationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(CONSTRAINT_VIOLATION_EXCEPTION)
            .header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }

  @Test
  public void writerExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(WRITER_EXCEPTION_PATH)
            .header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }

  @Test
  public void ProductListingGenericExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(PRODUCT_LISTING_GENERIC_PATH)
            .header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    } catch (NestedServletException e){
      Assertions.assertEquals(PRODUCT_LISTING_GENERIC_ERR_MESSAGE , e.getCause().getMessage());
    }
  }

  @Test
  public void applicationRuntimeException() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(APPLICATION_RUNTIME_EXCEPTION)
            .header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }

  @Test
  public void invalidStateExceptionException() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(INVALID_STATE_EXCEPTION)
            .header(HOST, LOCALHOST + port)
            .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }

  @Test
  public void invalidAttributeException() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      get(INVALID_ATTRIBUTE_EXCEPTION)
        .header(HOST, LOCALHOST + port)
        .sessionAttr(Constants.SESSION, getDefaultSession());
    Assertions.assertThrows(ServletException.class, () -> mockMvc.perform(requestBuilder));
  }

  @Test
  public void apiInCorrectDataException() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      get(INVALID_PRODUCT_DIMENSION)
        .header(HOST, LOCALHOST + port)
        .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    } catch (NestedServletException e){
      Assertions.assertEquals(ApiErrorCode.DIMENSION_EXCEEDED_THRESHOLD.getDesc(), e.getCause().getMessage());
    }
  }

  @Test
  public void missingServletRequestPartException() throws Exception {
    MockHttpServletRequestBuilder requestBuilder = get(EMPTY_IMAGE_FILE).header(HOST, LOCALHOST + port)
        .sessionAttr(Constants.SESSION, getDefaultSession());
    try {
      mockMvc.perform(requestBuilder);
    } catch (NestedServletException e) {
      Assertions.assertEquals(ErrorMessages.INVALID_REQUEST, e.getCause().getMessage());
    }
  }
}