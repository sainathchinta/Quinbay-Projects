package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.StoreCopyApiPath;
import com.gdn.partners.pcu.internal.service.StoreCopyService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;

@AutoConfigureMockMvc
public class StoreCopyControllerTest extends TestHelper {
  private static final String SELLER_CODE = "sellerCode";
  private static final String USER_NAME = "userName";
  private static final String PROCESS_TYPE = "processType";

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private StoreCopyService storeCopyService;

  @InjectMocks
  private StoreCopyController storeCopyController;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(storeCopyController).build();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(clientParameterHelper);
    verifyNoMoreInteractions(storeCopyService);
  }

  @Test
  public void bulkDownloadSellerProducts() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(StoreCopyApiPath.BASE_PATH + StoreCopyApiPath.DOWNLOAD_ALL_PRODUCTS, SELLER_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(storeCopyService).downloadAllProductsBySellerCode(Constants.USER_NAME, SELLER_CODE);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void downloadUploadTemplate() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(StoreCopyApiPath.BASE_PATH + StoreCopyApiPath.DOWNLOAD_UPLOAD_TEMPLATE, SELLER_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(storeCopyService).downloadUploadTemplate(SELLER_CODE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void checkPendingDownloadProcessTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(StoreCopyApiPath.BASE_PATH + StoreCopyApiPath.GET_PENDING_PROCESSES)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("sellerCode", String.valueOf(SELLER_CODE)).param("userName", String.valueOf(USER_NAME))
            .param("processType", String.valueOf(PROCESS_TYPE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(storeCopyService).getPendingProcesses(SELLER_CODE, USER_NAME, PROCESS_TYPE);
    verify(clientParameterHelper).getRequestId();
  }
}