package com.gdn.mta.bulk.service;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.partners.bulk.util.Constant;

public class StoreCopyServiceWrapperImplTest {

  public static final String SELLER_CODE = "sellerCode";

  @InjectMocks
  private StoreCopyServiceWrapperImpl storeCopyServiceWrapper;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Mock
  private BulkProcessDownloadService bulkProcessDownloadService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkDownloadServiceBeanUtil);
    Mockito.verifyNoMoreInteractions(bulkProcessDownloadService);
  }

  @Test
  public void downloadTargetSellerTemplate() throws Exception {
    Mockito.when(bulkDownloadServiceBeanUtil.getTargetSellerTemplateForCopyStore(SELLER_CODE, Constant.DEFAULT_USERNAME,
        Constant.REQUEST_ID)).thenReturn(new BulkDownloadRequest());
    Mockito.when(bulkProcessDownloadService.downloadAndOverwriteExcelFile(Mockito.any(BulkDownloadRequest.class)))
        .thenReturn(SELLER_CODE);
    String filePath =
        storeCopyServiceWrapper.downloadTargetSellerTemplate(Constant.STORE_ID, SELLER_CODE, Constant.DEFAULT_USERNAME,
            Constant.REQUEST_ID);
    Mockito.verify(bulkDownloadServiceBeanUtil).getTargetSellerTemplateForCopyStore(SELLER_CODE, Constant.DEFAULT_USERNAME,
        Constant.REQUEST_ID);
    Mockito.verify(bulkProcessDownloadService).downloadAndOverwriteExcelFile(Mockito.any(BulkDownloadRequest.class));
    Assertions.assertEquals(filePath, SELLER_CODE);

  }
}