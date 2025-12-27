package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.ArgumentMatchers.eq;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.mock.web.MockMultipartFile;

import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryResponse;
import com.gdn.mta.bulk.dto.BulkInternalProcessUploadRequest;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.XBulkFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.FileHelper;
import com.gdn.partners.pcu.internal.web.model.request.BulkInternalProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BulkInternalProcessSummaryWebResponse;
import org.apache.commons.io.FileUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class BulkInternalProcessServiceImplTest {

  public static final int PAGE = 0;
  public static final int SIZE = 1;
  public static final String REQUEST_ID = "requestId";
  public static final String SELLER_CODE = "INTERNAL";
  public static final String SELLER_NAME = "sellerName";
  public static final String PROCESS_TYPE_SALES_CATEGORY = "SALES_CATEGORY_UPDATE";
  public static final String FILE = "originalFileName.xlsx";
  public static final String PATH = "target/test-classes/internalProcess/";
  public static final String BIP_REQUEST_CODE = "BIP-0000001";
  public static final String UPLOAD_DATE = "UPLOAD_DATE";
  public static final String MOCK_FILE_PATH =
      new StringBuilder().append(PATH).append(BIP_REQUEST_CODE).append(Constants.SLASH).append(FILE).toString();

  private byte[] fileContent;
  private MockMultipartFile multipartFile;
  private MockedStatic<Credential> mockedStatic;

  @InjectMocks
  private BulkInternalProcessServiceImpl bulkInternalProcessService;

  @Mock
  private XBulkFeign xBulkFeign;

  @Mock
  private PBPFeign pbpFeign;

  @Mock
  private FileHelper fileHelper;

  @Captor
  ArgumentCaptor<BulkInternalProcessUploadRequest> bulkInternalProcessUploadRequestArgumentCaptor;

  @Captor
  ArgumentCaptor<String> bipRequestCodeArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    fileContent = new byte[] {-1, -40, -20, -10};
  }

  @AfterEach
  public void teardown() throws Exception {
    if (mockedStatic != null) {
      mockedStatic.close();
    }
    Mockito.verifyNoMoreInteractions(pbpFeign);
    Mockito.verifyNoMoreInteractions(xBulkFeign);
    Mockito.verifyNoMoreInteractions(fileHelper);
    FileUtils.deleteDirectory(new File(PATH));
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  private void mockStaticMethods() {
    String[] accessibilities = {Constants.INTERNAL_UPDATE_SALES_CATEGORY_ACCESSIBILITY};
    mockedStatic = Mockito.mockStatic(Credential.class);
    mockedStatic.when(Credential::getAccessibilities).thenReturn(accessibilities);
  }

  @Test
  public void bulkInternalProcessSummaryTest() throws Exception {
    BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest = new BulkInternalProcessSummaryRequest();
    bulkInternalProcessSummaryRequest.setSortColumn(UPLOAD_DATE);
    Page<BulkInternalProcessSummaryResponse> bulkInternalProcessSummaryResponsePage = new PageImpl<>(new ArrayList<>());
    Mockito.when(this.xBulkFeign.bulkInternalProcessSummary(bulkInternalProcessSummaryRequest, PAGE, SIZE)).thenReturn(
        new GdnRestListResponse<>(null, null, true, bulkInternalProcessSummaryResponsePage.getContent(),
            new PageMetaData(SIZE, PAGE, bulkInternalProcessSummaryResponsePage.getTotalElements()), REQUEST_ID));
    BulkInternalProcessSummaryWebRequest bulkInternalProcessSummaryWebRequest = new BulkInternalProcessSummaryWebRequest();
    Page<BulkInternalProcessSummaryWebResponse> bulkInternalProcessSummaryWebResponsePage =
        bulkInternalProcessService.bulkInternalProcessSummary(bulkInternalProcessSummaryWebRequest, PAGE, SIZE);
    Mockito.verify(xBulkFeign).bulkInternalProcessSummary(bulkInternalProcessSummaryRequest, PAGE, SIZE);
  }

  @Test
  public void uploadInternalProcessTest() throws Exception {
    mockStaticMethods();
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.when(this.pbpFeign.findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true,
            new SequenceResponse(Constants.BULK_INTERNAL_PROCESS_CODE_KEY, Long.valueOf(1)), Constants.REQUEST_ID));
    Mockito.doNothing().when(fileHelper)
        .uploadFileBasedOnProcessType(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    bulkInternalProcessService
        .uploadInternalProcess(multipartFile, SELLER_CODE, SELLER_NAME, PROCESS_TYPE_SALES_CATEGORY);
    Mockito.verify(pbpFeign).findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY);
    Mockito.verify(xBulkFeign)
        .uploadNewBulkInternalProcessRequest(bulkInternalProcessUploadRequestArgumentCaptor.capture());
    Mockito.verify(fileHelper).uploadFileBasedOnProcessType(eq(multipartFile), eq(PROCESS_TYPE_SALES_CATEGORY),
        bipRequestCodeArgumentCaptor.capture());
    Assertions.assertTrue(bipRequestCodeArgumentCaptor.getValue().contains(Constants.BULK_INTERNAL_PROCESS_CODE_KEY));
    Assertions.assertEquals(SELLER_CODE, bulkInternalProcessUploadRequestArgumentCaptor.getValue().getSellerCode());
    Assertions.assertEquals(FILE, bulkInternalProcessUploadRequestArgumentCaptor.getValue().getFileName());
  }

  @Test
  public void cancelInternalBulkProcessRequestTest() {
    Mockito.when(xBulkFeign.bulkInternalProcessCancelRequest(REQUEST_ID))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    bulkInternalProcessService.cancelInternalBulkProcessRequest(REQUEST_ID);
    Mockito.verify(xBulkFeign).bulkInternalProcessCancelRequest(REQUEST_ID);
  }
}
