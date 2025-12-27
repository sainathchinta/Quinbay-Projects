package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;
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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BulkInternalProcessControllerApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.BulkInternalProcessService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.BulkInternalProcessSummaryWebRequest;

@AutoConfigureMockMvc
public class BulkInternalProcessControllerTest extends TestHelper {

  public static final String FILE = "originalFileName.xlsx";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  public static final String PATH = BASE_DIRECTORY + "path/filestore/";
  public static final String BIP_REQUEST_CODE = "BIP-0000001";
  public static final String SELLER_CODE = "sellerCode";
  public static final String SELLER_NAME = "sellerName";
  public static final String PROCESS_TYPE = "STORE_COPY";
  public static final String MOCK_FILE_PATH =
      new StringBuilder().append(PATH).append(BIP_REQUEST_CODE).append(Constants.SLASH).append(FILE).toString();


  private byte[] fileContent;
  private MockMultipartFile multipartFile;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private BulkInternalProcessService bulkInternalProcessService;

  @InjectMocks
  private BulkInternalProcessController bulkInternalProcessController;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(bulkInternalProcessController).build();
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    fileContent = new byte[] {-1, -40, -20, -10};
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(clientParameterHelper);
    verifyNoMoreInteractions(bulkInternalProcessService);
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void bulkInternalProcessSummaryTest() throws Exception {
    BulkInternalProcessSummaryWebRequest bulkInternalProcessSummaryWebRequest =
        new BulkInternalProcessSummaryWebRequest();
    Mockito.when(bulkInternalProcessService.bulkInternalProcessSummary(bulkInternalProcessSummaryWebRequest, 0, 1))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    MockHttpServletRequestBuilder requestBuilder =
        post(BulkInternalProcessControllerApiPath.BASE_PATH + BulkInternalProcessControllerApiPath.SUMMARY)
            .contentType(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(bulkInternalProcessSummaryWebRequest))
            .accept(MediaType.APPLICATION_JSON).param("page", "0").param("size", "1");
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    verify(bulkInternalProcessService).bulkInternalProcessSummary(bulkInternalProcessSummaryWebRequest, 0, 1);
  }

  @Test
  public void uploadExcelForInternalProcessTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("internalProcess", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    this.mockMvc.perform(MockMvcRequestBuilders
        .multipart(BulkInternalProcessControllerApiPath.BASE_PATH + BulkInternalProcessControllerApiPath.UPLOAD)
        .file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(bulkInternalProcessService).uploadInternalProcess(multipartFile, null, null, PROCESS_TYPE);
  }

  @Test
  public void cancelInternalBulkProcessRequest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(BulkInternalProcessControllerApiPath.BASE_PATH + BulkInternalProcessControllerApiPath.CANCEL)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .param("internalProcessRequestCode", BIP_REQUEST_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(bulkInternalProcessService).cancelInternalBulkProcessRequest(BIP_REQUEST_CODE);
  }
}
