package com.gdn.mta.bulk.service.download;


import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterSkuReviewDownloadItemsRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemsDownloadResponse;
import com.gdn.mta.bulk.repository.MasterSkuItemsRepository;

@ExtendWith(MockitoExtension.class)
class BulkDownloadMasterSkuReviewItemsServiceBeanTest {

  @Mock
  private MasterSkuItemsRepository masterSkuItemsRepository;

  @InjectMocks
  private BulkDownloadMasterSkuReviewItemsServiceBean bulkDownloadMasterSkuReviewItemsServiceBean;

  private MasterSkuReviewDownloadItemsRequest masterSkuReviewDownloadItemsRequest;
  private ItemsDownloadResponse itemsDownloadResponse;

  @BeforeEach
  public void setup() {
    masterSkuReviewDownloadItemsRequest = new MasterSkuReviewDownloadItemsRequest();
    masterSkuReviewDownloadItemsRequest.setBulkProcessEntity(BulkProcessEntity.MASTER_SKU_ALL_ITEMS_DOWNLOAD);
    itemsDownloadResponse = new ItemsDownloadResponse();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(masterSkuItemsRepository);
  }

  @Test
  public void getDataTest() throws Exception {
    Mockito.when(masterSkuItemsRepository.getItemsForMasterSkuReview(Mockito.any())).thenReturn(itemsDownloadResponse);
    BulkDataResponse response =
        bulkDownloadMasterSkuReviewItemsServiceBean.getData((BulkDownloadRequest) masterSkuReviewDownloadItemsRequest);
    Mockito.verify(masterSkuItemsRepository).getItemsForMasterSkuReview(Mockito.any());
    Assertions.assertEquals(masterSkuReviewDownloadItemsRequest.getBulkProcessEntity(),
        response.getBulkProcessEntity());
  }

}
