package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.verifyNoMoreInteractions;

public class BulkDownloadServiceWrapperImplTest {

  private static final String DEFAULT_STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String BULK_VENDOR_DOWNLOAD_EMAIL_TO = "bulkVendorDownloadEmailTo";
  private static final String BULK_VENDOR_DOWNLOAD_EMAIL_CC = "bulkVendorDownloadEmailCc";
  private static final String EMAIL_TO_MUST_NOT_BE_BLANK = "Email To value must not be blank";

  private SystemParameterConfig systemParameterConfig = new SystemParameterConfig();
  private FilterSummaryRequest filterSummaryRequest = new FilterSummaryRequest();

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @InjectMocks
  private BulkDownloadServiceWrapperImpl bulkDownloadServiceWrapper;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    systemParameterConfig.setValue(USERNAME);
    systemParameterConfig.setVariable(USERNAME);
  }

  @AfterEach
  public void teardown() {
    verifyNoMoreInteractions(systemParameterConfigService);
    verifyNoMoreInteractions(bulkProcessDownloadService);
    verifyNoMoreInteractions(bulkDownloadServiceBeanUtil);
  }

  @Test
  public void downloadAndSendVendorProductMailTest() throws Exception {
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, BULK_VENDOR_DOWNLOAD_EMAIL_CC))
        .thenReturn(systemParameterConfig);
    Mockito.when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, BULK_VENDOR_DOWNLOAD_EMAIL_TO))
        .thenReturn(systemParameterConfig);
    Mockito.doNothing().when(bulkProcessDownloadService)
        .downloadAll(Mockito.any(BulkDownloadRequest.class));
    Mockito.when(this.bulkDownloadServiceBeanUtil
        .getVendorBulkDownloadRequest(USERNAME, USERNAME, filterSummaryRequest, REQUEST_ID,
            USERNAME)).thenReturn(new BulkDownloadRequest());
    bulkDownloadServiceWrapper
        .downloadAndSendVendorProductMail(DEFAULT_STORE_ID, filterSummaryRequest, REQUEST_ID,
            USERNAME);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, BULK_VENDOR_DOWNLOAD_EMAIL_CC);
    Mockito.verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(DEFAULT_STORE_ID, BULK_VENDOR_DOWNLOAD_EMAIL_TO);
    Mockito.verify(bulkProcessDownloadService).downloadAll(Mockito.any(BulkDownloadRequest.class));
    Mockito.verify(this.bulkDownloadServiceBeanUtil)
        .getVendorBulkDownloadRequest(USERNAME, USERNAME, filterSummaryRequest, REQUEST_ID,
            USERNAME);
  }
}
