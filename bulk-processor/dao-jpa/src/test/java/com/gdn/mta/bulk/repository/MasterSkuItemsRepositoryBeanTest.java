package com.gdn.mta.bulk.repository;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.MasterSkuReviewFeign;
import com.gdn.mta.bulk.models.download.ClusterReviewFeedbackRequest;
import com.gdn.mta.bulk.models.download.DownloadItemsRequest;
import com.gdn.mta.bulk.models.download.responsedata.ClusterActionResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemSkuAndMasterSkuResponse;
import com.gdn.mta.bulk.models.download.responsedata.ItemsDownloadResponse;

//@ExtendWith(MockitoExtension.class)
public class MasterSkuItemsRepositoryBeanTest {

  private static final String ITEM_SKU = "item_sku";
  private static final String MASTER_SKU = "master_sku";
  private static final String REQUEST_ID = "x-bulk";
  private static final String STORE_ID = "10001";
  private static final String USER_NAME = "username";

  @Mock
  private MasterSkuReviewFeign masterSkuReviewFeign;

  @InjectMocks
  private MasterSkuItemsRepositoryBean masterSkuItemsRepositoryBean;

  private DownloadItemsRequest downloadItemsRequest;
  private ClusterReviewFeedbackRequest clusterReviewFeedbackRequest;
  private ItemsDownloadResponse itemsDownloadResponse;
  private ClusterActionResponse clusterActionResponse;
  private ItemSkuAndMasterSkuResponse itemSkuAndMasterSkuResponse;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(masterSkuItemsRepositoryBean, "masterSkuReviewItemsFetchBatchSize", 1);
    downloadItemsRequest = new DownloadItemsRequest();
    downloadItemsRequest.setPage(0);
    downloadItemsRequest.setLimit(5);
    itemSkuAndMasterSkuResponse = new ItemSkuAndMasterSkuResponse();
    itemSkuAndMasterSkuResponse.setMasterItemSku(MASTER_SKU);
    itemSkuAndMasterSkuResponse.setItemSku(ITEM_SKU);
    itemsDownloadResponse = new ItemsDownloadResponse();
    itemsDownloadResponse.setItemSkuAndMasterSkuResponseList(List.of(itemSkuAndMasterSkuResponse));
    clusterActionResponse = new ClusterActionResponse();
    clusterReviewFeedbackRequest = new ClusterReviewFeedbackRequest();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(masterSkuReviewFeign);
  }

  @Test
  public void getItemsForMasterSkuReviewTest() throws Exception {
    GdnRestSingleResponse<ItemsDownloadResponse> response =
        new GdnRestSingleResponse<>(itemsDownloadResponse, REQUEST_ID);
    Mockito.when(masterSkuReviewFeign.downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(response);
    ItemsDownloadResponse result = masterSkuItemsRepositoryBean.getItemsForMasterSkuReview(downloadItemsRequest);
    Mockito.verify(masterSkuReviewFeign, Mockito.times(5))
        .downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(), Mockito.any());
    Assertions.assertEquals(5, result.getItemSkuAndMasterSkuResponseList().size());
    Assertions.assertEquals(MASTER_SKU, result.getItemSkuAndMasterSkuResponseList().get(0).getMasterItemSku());
  }

  @Test
  public void getItemsForMasterSkuReviewEmptyListTest() throws Exception {
    GdnRestSingleResponse<ItemsDownloadResponse> response =
        new GdnRestSingleResponse<>(itemsDownloadResponse, REQUEST_ID);
    downloadItemsRequest.setItemSkuList(List.of(ITEM_SKU));
    Mockito.when(masterSkuReviewFeign.downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(response);
    ItemsDownloadResponse result = masterSkuItemsRepositoryBean.getItemsForMasterSkuReview(downloadItemsRequest);
    Mockito.verify(masterSkuReviewFeign)
        .downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(), Mockito.any());
    Assertions.assertEquals(1, result.getItemSkuAndMasterSkuResponseList().size());
    Assertions.assertEquals(MASTER_SKU, result.getItemSkuAndMasterSkuResponseList().get(0).getMasterItemSku());
  }

  @Test
  public void getItemsForMasterSkuReviewSelectedDownloadTest() throws Exception {
    GdnRestSingleResponse<ItemsDownloadResponse> response =
        new GdnRestSingleResponse<>(new ItemsDownloadResponse(), REQUEST_ID);
    Mockito.when(masterSkuReviewFeign.downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(response);
    ItemsDownloadResponse result = masterSkuItemsRepositoryBean.getItemsForMasterSkuReview(downloadItemsRequest);
    Mockito.verify(masterSkuReviewFeign, Mockito.times(1))
        .downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(), Mockito.any());
    Assertions.assertEquals(0, result.getItemSkuAndMasterSkuResponseList().size());
  }

  @Test
  public void getItemsForMasterSkuReviewNullResponseTest() throws Exception {
    GdnRestSingleResponse<ItemsDownloadResponse> response =
        new GdnRestSingleResponse<>(itemsDownloadResponse, REQUEST_ID);
    Mockito.when(masterSkuReviewFeign.downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        masterSkuItemsRepositoryBean.getItemsForMasterSkuReview(downloadItemsRequest);
      });
    } finally {
      Mockito.verify(masterSkuReviewFeign)
          .downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void getItemsForMasterSkuReviewSuccessFalseTest() throws Exception {
    GdnRestSingleResponse<ItemsDownloadResponse> response =
        new GdnRestSingleResponse<>(itemsDownloadResponse, REQUEST_ID);
    response.setSuccess(false);
    Mockito.when(masterSkuReviewFeign.downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        masterSkuItemsRepositoryBean.getItemsForMasterSkuReview(downloadItemsRequest);
      });
    } finally {
      Mockito.verify(masterSkuReviewFeign).downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(),
          Mockito.any());
    }
  }

  @Test
  public void getItemsForMasterSkuReviewSuccessFalseValidationTest() throws Exception {
    GdnRestSingleResponse<ItemsDownloadResponse> response =
        new GdnRestSingleResponse<>(itemsDownloadResponse, REQUEST_ID);
    response.setSuccess(false);
    response.setErrorCode(ErrorCategory.VALIDATION.getCode());
    Mockito.when(masterSkuReviewFeign.downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        masterSkuItemsRepositoryBean.getItemsForMasterSkuReview(downloadItemsRequest);
      });
    } finally {
      Mockito.verify(masterSkuReviewFeign)
          .downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void getItemsForMasterSkuReviewNullValueTest() throws Exception {
    GdnRestSingleResponse<ItemsDownloadResponse> response =
        new GdnRestSingleResponse<>(itemsDownloadResponse, REQUEST_ID);
    response.setValue(null);
    Mockito.when(masterSkuReviewFeign.downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(),
        Mockito.any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        masterSkuItemsRepositoryBean.getItemsForMasterSkuReview(downloadItemsRequest);
      });
    } finally {
      Mockito.verify(masterSkuReviewFeign)
          .downloadItemsForMasterSkuReview(Mockito.any(), Mockito.any(), Mockito.any());
    }
  }

  @Test
  public void performClusterActionTest() throws Exception {
    GdnRestSingleResponse<ClusterActionResponse> response =
        new GdnRestSingleResponse<>(clusterActionResponse, REQUEST_ID);
    Mockito.when(
        masterSkuReviewFeign.performClusterReviewAction(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.any(), Mockito.eq(MASTER_SKU), Mockito.eq(clusterReviewFeedbackRequest))).thenReturn(response);
    masterSkuItemsRepositoryBean.performClusterAction(MASTER_SKU, clusterReviewFeedbackRequest, USER_NAME);
    Mockito.verify(masterSkuReviewFeign)
        .performClusterReviewAction(Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(), Mockito.any(),
            Mockito.eq(MASTER_SKU), Mockito.eq(clusterReviewFeedbackRequest));
  }

}
