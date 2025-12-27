package com.gdn.partners.product.analytics.web.controller;

import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;

import com.gdn.partners.product.analytics.web.TestApplication;
import com.gdn.partners.product.analytics.web.helper.TestHelper;

@SpringBootTest(classes = TestApplication.class, webEnvironment=SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureMockMvc
public class ErrorControllerTest extends TestHelper {

  private static final String CLIENT_EXCEPTION_PATH = "/sample/clientException";
  private static final String FILE_EXCEPTION_PATH = "/sample/fileException";
  private static final String VALIDATION_EXCEPTION_PATH = "/sample/validationException";
  private static final String APPLICATION_RUNTIME_EXCEPTION = "/sample/applicationRuntimeException";
  private static final String HOST = "host";
  private static final String LOCALHOST = "localhost:";


  @BeforeEach
  public void setUp() throws Exception {

  }

  @Test
  public void clientExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(CLIENT_EXCEPTION_PATH)
            .header(HOST, LOCALHOST + String.valueOf(8080));
    mockMvc.perform(requestBuilder).andExpect(status().isInternalServerError());
  }

  @Test
  public void fileExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(FILE_EXCEPTION_PATH)
            .header(HOST, LOCALHOST + String.valueOf(8080));
    mockMvc.perform(requestBuilder).andExpect(status().isInternalServerError());
  }

  @Test
  public void validationExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(VALIDATION_EXCEPTION_PATH).header(HOST, LOCALHOST + String.valueOf(8080));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
  }

  @Test
  public void applicationRuntimeExceptionTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(APPLICATION_RUNTIME_EXCEPTION)
            .header(HOST, LOCALHOST + String.valueOf(8080));
    mockMvc.perform(requestBuilder).andExpect(status().isInternalServerError());
  }
}
