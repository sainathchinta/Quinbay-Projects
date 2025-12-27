package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.RecatApiPath;
import com.gdn.partners.pcu.internal.service.RecatService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.internal.web.model.request.RecatProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProductSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductCountWebResponse;
import org.apache.commons.io.FileUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import java.util.ArrayList;
import java.io.File;
import java.io.IOException;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;

@AutoConfigureMockMvc
public class RecatControllerTest extends TestHelper {

  private static final String DEFAULT_RECAT_REQUEST_CODE = "RECAT_REQUEST_CODE";
  private static final String RECAT_REQUEST_CODE = "recat-request-code";
  private static final String PATH = "path/filestore/";
  private static final String FILE = "originalFilename.xlsx";
  private static final int PAGE = 0;
  private static final int SIZE = 25;

  private byte[] fileContent;
  private MockMultipartFile multipartFile;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private RecatService recatService;

  @InjectMocks
  private RecatController recatController;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(recatController).build();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    fileContent = new byte[]{-1, -40, -20, -10};
  }

  @AfterEach
  public void teardown() throws IOException {
    verifyNoMoreInteractions(recatService);
    verifyNoMoreInteractions(clientParameterHelper);
    FileUtils.deleteDirectory(new File(PATH));
  }

  @Test
  public void getFailedProductsMailTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(RecatApiPath.BASE_PATH + RecatApiPath.GET_FAILED_PRODUCTS_MAIL, DEFAULT_RECAT_REQUEST_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(recatService).getFailedProductsMail(DEFAULT_RECAT_REQUEST_CODE);
    verify(clientParameterHelper).getRequestId();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void uploadRecatExcelTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("recatExcel", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(RecatApiPath.BASE_PATH + RecatApiPath.UPLOAD_EXCEL)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(recatService).uploadRecatRequest(multipartFile, null);
    verify(clientParameterHelper).getRequestId();

  }

  @Test
  public void productCenterActionTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(recatService).cancelRecatRequest(RECAT_REQUEST_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        put(RecatApiPath.BASE_PATH + RecatApiPath.CANCEL_REQUEST, RECAT_REQUEST_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(recatService).cancelRecatRequest(RECAT_REQUEST_CODE);
    verify(clientParameterHelper).getRequestId();  }

  @Test
  public void getRecatProductStatusCountTest() throws Exception {
    Mockito.when(recatService.getRecatProductStatusCounts(RECAT_REQUEST_CODE))
        .thenReturn(new RecatProductCountWebResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(RecatApiPath.BASE_PATH + RecatApiPath.GET_REACT_PRODUCT_STATUS_COUNTS, RECAT_REQUEST_CODE)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(recatService).getRecatProductStatusCounts(RECAT_REQUEST_CODE);
    verify(clientParameterHelper).getRequestId();  }

  @Test
  public void getRecatProductSummaryTest() throws Exception {
    RecatProductSummaryWebRequest recatProductSummaryWebRequest = new RecatProductSummaryWebRequest();
    Mockito.when(recatService.getRecatProductSummary(RECAT_REQUEST_CODE, 0, 1, recatProductSummaryWebRequest))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    MockHttpServletRequestBuilder requestBuilder =
        post(RecatApiPath.BASE_PATH + RecatApiPath.GET_RECAT_PRODUCT_SUMMARY, RECAT_REQUEST_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(recatProductSummaryWebRequest))
            .accept(MediaType.APPLICATION_JSON).param("page", "0").param("size", "1");
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(recatService).getRecatProductSummary(RECAT_REQUEST_CODE, 0, 1, recatProductSummaryWebRequest);
    verify(clientParameterHelper).getRequestId();  }

  @Test
  public void getRecatProcessSummaryTest() throws Exception {
    RecatProcessSummaryWebRequest recatProcessSummaryWebRequest = new RecatProcessSummaryWebRequest();
    Mockito
        .when(recatService.getRecatSummaryByFilter(recatProcessSummaryWebRequest, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    MockHttpServletRequestBuilder requestBuilder =
        post(RecatApiPath.BASE_PATH + RecatApiPath.REQUEST_SUMMARY_FILTER)
            .contentType(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(recatProcessSummaryWebRequest))
            .accept(MediaType.APPLICATION_JSON).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE));
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(recatService).getRecatSummaryByFilter(recatProcessSummaryWebRequest, PAGE, SIZE);
    verify(clientParameterHelper).getRequestId();  }
}