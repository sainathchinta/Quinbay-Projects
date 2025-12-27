package com.gdn.partners.pcu.internal.service.impl;

import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.XBulkOutboundService;
import com.gdn.partners.pcu.internal.web.model.request.RecatProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProductSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.RecatProcessSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductSummaryWebResponse;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
//import org.powermock.api.mockito.// PowerMockito;
//import org.powermock.core.classloader.annotations.PrepareForTest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.mock.web.MockMultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Date;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verifyNoMoreInteractions;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class RecatServiceImplTest  {

  private static final String PATH = "path/filestore/";
  private static final String FILE = "originalFilename.xlsx";
  private static final String INVALID_FILE = "originalFilename.xls";
  private static final String RECAT_REQUEST_CODE = "RE-0000001";
  private static final String SCHEDULE_DATE = "20/08/2021 00:00:00";
  private static final String INVALID_SCHEDULE_DATE = "20-08-2021 00:00:00";
  private static final String MOCK_FILE_PATH =
      new StringBuilder().append(PATH).append(RECAT_REQUEST_CODE).append(Constants.SLASH)
          .append(FILE).toString();

  private byte[] fileContent;
  private MockMultipartFile multipartFile;

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String NEW_CATEGORY_CODE = "newCategoryCode";
  private static final String NEW_CATEGORY_NAME = "newCategoryName";
  private static final int PAGE = 0;
  private static final int SIZE = 20;
  private static final String CREATED_BY = "createdBy";
  private static final String RECAT_STATUS = "recatStatus";

  private RecatProductCountResponse recatProductCountResponse;
  private RecatProductSummaryResponse recatProductSummaryResponse;
  private RecatProcessSummaryResponse recatProcessSummaryResponse;
  private MockedStatic<Credential> mockedStatic;

  @Mock
  private XBulkOutboundService xBulkOutboundService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @InjectMocks
  private RecatServiceImpl recatService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    fileContent = new byte[]{-1, -40, -20, -10};
    Mockito.when(this.systemParameterProperties.getRecatUploadFilePath()).thenReturn(PATH);
  }

  @AfterEach
  public void teardown() throws Exception {
    Mockito.verifyNoMoreInteractions(xBulkOutboundService);
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(systemParameterProperties);
    FileUtils.deleteDirectory(new File(PATH));
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void uploadRecatRequestTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_RECAT_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.doNothing().when(this.xBulkOutboundService)
        .uploadNewRecatRequest(eq(RECAT_REQUEST_CODE), eq(FILE), Mockito.anyString());
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(MOCK_FILE_PATH);
    Mockito.when(this.pbpFeign.findCounterByKey(Constants.RECAT_REQUEST_CODE_KEY)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true,
            new SequenceResponse(Constants.RECAT_REQUEST_CODE_KEY, Long.valueOf(1)),
            Constants.REQUEST_ID));
    recatService.uploadRecatRequest(multipartFile, StringUtils.EMPTY);
    Mockito.verify(this.pbpFeign).findCounterByKey(Constants.RECAT_REQUEST_CODE_KEY);
    Mockito.verify(xBulkOutboundService)
        .uploadNewRecatRequest(eq(RECAT_REQUEST_CODE), eq(FILE), Mockito.anyString());
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void uploadRecatRequest_withScheduleDateTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_RECAT_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.doNothing().when(this.xBulkOutboundService)
        .uploadNewRecatRequest(eq(RECAT_REQUEST_CODE), eq(FILE), Mockito.anyString());
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(MOCK_FILE_PATH);
    Mockito.when(this.pbpFeign.findCounterByKey(Constants.RECAT_REQUEST_CODE_KEY)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true,
            new SequenceResponse(Constants.RECAT_REQUEST_CODE_KEY, Long.valueOf(1)),
            Constants.REQUEST_ID));
    recatService.uploadRecatRequest(multipartFile, SCHEDULE_DATE);
    Mockito.verify(this.pbpFeign).findCounterByKey(Constants.RECAT_REQUEST_CODE_KEY);
    Mockito.verify(xBulkOutboundService)
        .uploadNewRecatRequest(RECAT_REQUEST_CODE, FILE, SCHEDULE_DATE);
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void getAssigneesByFilterRequestAndActivatedAndViewableFlagTest() {
    String[] accessibility = {Constants.INTERNAL_RECAT_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    recatService.getFailedProductsMail(RECAT_REQUEST_CODE);
    Mockito.verify(xBulkOutboundService).getFailedProductsMail(RECAT_REQUEST_CODE);
  }

  @Test
  public void uploadRecatRequest_withInvalidScheduleDateTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_RECAT_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.doNothing().when(this.xBulkOutboundService)
        .uploadNewRecatRequest(eq(RECAT_REQUEST_CODE), eq(FILE), Mockito.anyString());
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(MOCK_FILE_PATH);
    Mockito.when(this.pbpFeign.findCounterByKey(Constants.RECAT_REQUEST_CODE_KEY)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true,
            new SequenceResponse(Constants.RECAT_REQUEST_CODE_KEY, Long.valueOf(1)),
            Constants.REQUEST_ID));
    recatService.uploadRecatRequest(multipartFile, INVALID_SCHEDULE_DATE);
    Mockito.verify(this.pbpFeign).findCounterByKey(Constants.RECAT_REQUEST_CODE_KEY);
    Mockito.verify(xBulkOutboundService)
        .uploadNewRecatRequest(eq(RECAT_REQUEST_CODE), eq(FILE), Mockito.anyString());
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void uploadRecatRequest_invalidFileTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", INVALID_FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    try {
      recatService.uploadRecatRequest(multipartFile, StringUtils.EMPTY);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @BeforeEach
  public void setUp() {
    recatProductCountResponse = new RecatProductCountResponse(20, 10, 0, 10, "PARTIAL_SUCCESS");
    recatProductSummaryResponse =
        new RecatProductSummaryResponse(RECAT_REQUEST_CODE, PRODUCT_CODE, PRODUCT_NAME, CATEGORY_CODE, CATEGORY_NAME,
            NEW_CATEGORY_CODE, NEW_CATEGORY_NAME, "COMPLETED", new Date(), "Succeed");
    recatProcessSummaryResponse = RecatProcessSummaryResponse.builder().initiator(CREATED_BY)
        .recatRequestCode(RECAT_REQUEST_CODE).productCount(10).status(RECAT_STATUS).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(xBulkOutboundService);
    verifyNoMoreInteractions(xBulkOutboundService);
    if (mockedStatic != null) {
      mockedStatic.close();
    }
  }

  private void mockStaticMethods(String[] accessibility){
    mockedStatic = Mockito.mockStatic(Credential.class);
    mockedStatic.when(Credential::getAccessibilities).thenReturn(accessibility);
  }

  @Test
  public void getRecatProductStatusCountsTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_RECAT_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    Mockito.when(xBulkOutboundService.getRecatProductStatusCount(RECAT_REQUEST_CODE))
        .thenReturn(recatProductCountResponse);
    RecatProductCountWebResponse recatProductCountWebResponse =
        recatService.getRecatProductStatusCounts(RECAT_REQUEST_CODE);
    Mockito.verify(xBulkOutboundService).getRecatProductStatusCount(RECAT_REQUEST_CODE);
    Assertions.assertEquals(20, recatProductCountWebResponse.getTotalCount());
    Assertions.assertEquals(10, recatProductCountWebResponse.getSuccessCount());
    Assertions.assertEquals(0, recatProductCountWebResponse.getInProgressCount());
    Assertions.assertEquals(10, recatProductCountWebResponse.getFailedCount());
    Assertions.assertEquals("PARTIAL_SUCCESS", recatProductCountWebResponse.getStatus());
  }

  @Test
  public void getRecatProductStatusCountsInvalidAccessibilityTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    try {
      recatService.getRecatProductStatusCounts(RECAT_REQUEST_CODE);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void getRecatProductSummaryTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_RECAT_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    Mockito.when(xBulkOutboundService.getRecatProductSummaryByRecatRequestCode(RECAT_REQUEST_CODE, 0, 1, new RecatProductSummaryRequest()))
        .thenReturn(new PageImpl<>(Arrays.asList(recatProductSummaryResponse)));
    Page<RecatProductSummaryWebResponse> recatProductSummaryWebResponsePage =
        recatService.getRecatProductSummary(RECAT_REQUEST_CODE, 0, 1, new RecatProductSummaryWebRequest());
    Mockito.verify(xBulkOutboundService).getRecatProductSummaryByRecatRequestCode(RECAT_REQUEST_CODE, 0, 1, new RecatProductSummaryRequest());
    Assertions.assertEquals(RECAT_REQUEST_CODE, recatProductSummaryWebResponsePage.getContent().get(0).getRecatRequestCode());
    Assertions.assertEquals(PRODUCT_CODE, recatProductSummaryWebResponsePage.getContent().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME, recatProductSummaryWebResponsePage.getContent().get(0).getProductName());
    Assertions.assertEquals(CATEGORY_CODE, recatProductSummaryWebResponsePage.getContent().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, recatProductSummaryWebResponsePage.getContent().get(0).getCategoryName());
    Assertions.assertEquals(NEW_CATEGORY_CODE, recatProductSummaryWebResponsePage.getContent().get(0).getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME, recatProductSummaryWebResponsePage.getContent().get(0).getNewCategoryName());
    Assertions.assertEquals("COMPLETED", recatProductSummaryWebResponsePage.getContent().get(0).getStatus());
    Assertions.assertEquals("Succeed", recatProductSummaryWebResponsePage.getContent().get(0).getNotes());
    Assertions.assertNotNull(recatProductSummaryWebResponsePage.getContent().get(0).getUpdatedDate());
  }

  @Test
  public void getRecatProductSummaryTestInvalidAccessibilityTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_PRODUCT_CENTER_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    try {
      recatService.getRecatProductSummary(RECAT_REQUEST_CODE, 0, 1,
          new RecatProductSummaryWebRequest());
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  public void getRecatSummaryByFilterTest() throws Exception {
    String[] accessibility = {Constants.INTERNAL_RECAT_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    Mockito.when(xBulkOutboundService
        .recatProcessFilterSummary(Mockito.any(RecatProcessSummaryRequest.class), eq(PAGE),
            eq(SIZE))).thenReturn(
        new PageImpl<>(Arrays.asList(recatProcessSummaryResponse), PageRequest.of(PAGE, SIZE), 1));
    Page<RecatProcessSummaryWebResponse> response =
        recatService.getRecatSummaryByFilter(new RecatProcessSummaryWebRequest(), PAGE, SIZE);
    Mockito.verify(xBulkOutboundService)
        .recatProcessFilterSummary(Mockito.any(RecatProcessSummaryRequest.class), eq(PAGE),
            eq(SIZE));
    Assertions.assertEquals(RECAT_REQUEST_CODE, response.getContent().get(0).getRecatRequestCode());
    Assertions.assertEquals(CREATED_BY, response.getContent().get(0).getInitiator());
    Assertions.assertEquals(RECAT_STATUS, response.getContent().get(0).getStatus());
  }

  @Test
  public void cancelRecatRequestTest() {
    String[] accessibility = {Constants.INTERNAL_RECAT_ACCESSIBILITY};
    mockStaticMethods(accessibility);
    recatService.cancelRecatRequest(RECAT_REQUEST_CODE);
    Mockito.verify(xBulkOutboundService).cancelRecatRequest(RECAT_REQUEST_CODE);
  }
}