package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.XBulkOutboundService;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.StoreCopyDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.response.PendingDownloadProcessWebResponse;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.mockito.ArgumentMatchers.eq;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class StoreCopyServiceImplTest {

  public static final String SELLER_CODE = "sellerCode";
  public static final String SELLER_CODE_FILE = "sellerCodeFile";
  public static final String USER_NAME = "userName";
  public static final String PROCESS_TYPE = "processType";
  public static final long PENDING_COUNT = 53;

  @InjectMocks
  private StoreCopyServiceImpl storeCopyService;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @Mock
  private XBulkOutboundService xBulkOutboundService;

  @Captor
  private ArgumentCaptor<BulkDownloadRequest> bulkDownloadRequestArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(kafkaPublisher);
    Mockito.verifyNoMoreInteractions(xBulkOutboundService);
  }

  @Test
  public void testDownloadAllProductsBySellerCode() {
    storeCopyService.downloadAllProductsBySellerCode(Constants.USER_NAME, SELLER_CODE);
    Mockito.verify(this.kafkaPublisher)
        .send(eq(DomainEventName.BULK_DOWNLOAD_ALL_EVENT), eq(SELLER_CODE),bulkDownloadRequestArgumentCaptor.capture());
    StoreCopyDownloadRequest bulkDownloadRequest =
        (StoreCopyDownloadRequest) bulkDownloadRequestArgumentCaptor.getValue();
    Assertions.assertNotNull(bulkDownloadRequest);
    Assertions.assertEquals(SELLER_CODE, bulkDownloadRequest.getMerchantId());
    Assertions.assertEquals(Constants.USER_NAME, bulkDownloadRequest.getUsername());
    Assertions.assertEquals(BulkProcessEntity.STORE_COPY_PRODUCTS, bulkDownloadRequest.getBulkProcessEntity());
    Assertions.assertFalse(bulkDownloadRequest.getArchived());
  }

  @Test
  public void downloadUploadTemplate() {
    Mockito.when(xBulkOutboundService.downloadStoreCopyUploadTemplate(SELLER_CODE)).thenReturn(SELLER_CODE_FILE);
    String fileName = storeCopyService.downloadUploadTemplate(SELLER_CODE);
    Mockito.verify(xBulkOutboundService).downloadStoreCopyUploadTemplate(SELLER_CODE);
    Assertions.assertEquals(fileName, SELLER_CODE_FILE);
  }

  @Test
  public void checkPendingDownloadProcessTest() {
    BulkInternalPendingRequestResponse bulkInternalPendingRequestResponse = new BulkInternalPendingRequestResponse();
    bulkInternalPendingRequestResponse.setBulkInternalStatusFlag(true);
    bulkInternalPendingRequestResponse.setPendingRequestsCount(PENDING_COUNT);
    Mockito.when(xBulkOutboundService.getPendingProcesses(SELLER_CODE, USER_NAME, PROCESS_TYPE))
        .thenReturn(bulkInternalPendingRequestResponse);
    PendingDownloadProcessWebResponse pendingDownloadProcessWebResponse =
        storeCopyService.getPendingProcesses(SELLER_CODE, USER_NAME, PROCESS_TYPE);
    Mockito.verify(xBulkOutboundService).getPendingProcesses(SELLER_CODE, USER_NAME, PROCESS_TYPE);
    Assertions.assertEquals(true, pendingDownloadProcessWebResponse.isBulkInternalProcessStatusFlag());
    Assertions.assertEquals(PENDING_COUNT, pendingDownloadProcessWebResponse.getPendingCount());
  }
}
