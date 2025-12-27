package com.gdn.mta.bulk.service.download;

import com.gdn.mta.bulk.models.AutoApprovedSelectedDownloadRequest;
import com.gdn.mta.bulk.models.download.AutoApprovedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.AutoApprovedWebRequest;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedListWebResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkAutoApprovedProductsDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.SellerResponse;
import com.gdn.mta.bulk.service.ProductAnalyticsOutboundService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;

class BulkDownloadAutoApprovedProductsServiceBeanTest {

  @Mock
  private ProductAnalyticsOutboundService productAnalyticsOutboundService;

  @InjectMocks
  private BulkDownloadAutoApprovedProductsServiceBean bulkDownloadAutoApprovedProductsServiceBean;

  private AutoApprovedProductsDownloadRequest request;

  private static final int PAGE = 0;
  private static final int SIZE = 100;
  private static final String PRODUCT_CODE = "productCode";
  private static final String REQUEST_ID = "requestId";

  @BeforeEach
  void setUp() {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(bulkDownloadAutoApprovedProductsServiceBean,
        "autoApprovedProductsFetchBatchSize", 50);
    request = new AutoApprovedProductsDownloadRequest();
  }

  @Test
  void getData_EmptyProductCodeList() throws Exception {
    request.setRequestId(REQUEST_ID);
    request.setProductCodeList(Collections.emptyList());

    List<AutoApprovedListWebResponse> mockResponseList = new ArrayList<>();
    SellerResponse sellerResponse = new SellerResponse();
    AutoApprovedListWebResponse autoApprovedListWebResponse = new AutoApprovedListWebResponse();
    autoApprovedListWebResponse.setSeller(sellerResponse);
    mockResponseList.add(autoApprovedListWebResponse);
    mockResponseList.add(new AutoApprovedListWebResponse());

    Mockito.when(productAnalyticsOutboundService.fetchAutoApprovedProductsDownloadList(
            eq(REQUEST_ID), eq(0), eq(bulkDownloadAutoApprovedProductsServiceBean.autoApprovedProductsFetchBatchSize),
            Mockito.any()))
        .thenReturn(mockResponseList);

    BulkDataResponse result = bulkDownloadAutoApprovedProductsServiceBean.getData(request);

    Mockito.verify(productAnalyticsOutboundService, Mockito.times(2))
        .fetchAutoApprovedProductsDownloadList(any(), anyInt(), anyInt(), any(AutoApprovedWebRequest.class));

    Assertions.assertEquals(BulkAutoApprovedProductsDownloadResponse.class, result.getClass());
    Assertions.assertEquals(2,
        ((BulkAutoApprovedProductsDownloadResponse) result).getAutoApprovedProductsResponses().size());
  }

  @Test
  void getData_NonEmptyProductCodeList() throws Exception {
    request.setRequestId(REQUEST_ID);
    request.setProductCodeList(Collections.singletonList(PRODUCT_CODE));

    List<AutoApprovedListWebResponse> mockResponseList = new ArrayList<>();
    mockResponseList.add(new AutoApprovedListWebResponse());
    mockResponseList.add(new AutoApprovedListWebResponse());

    Mockito.when(productAnalyticsOutboundService.fetchAutoApprovedProductsSelectedDownloadList(
            REQUEST_ID, new AutoApprovedSelectedDownloadRequest(Collections.singletonList(PRODUCT_CODE))))
        .thenReturn(mockResponseList);

    BulkDataResponse result = bulkDownloadAutoApprovedProductsServiceBean.getData(request);
    Mockito.verify(productAnalyticsOutboundService)
        .fetchAutoApprovedProductsSelectedDownloadList(REQUEST_ID,
            new AutoApprovedSelectedDownloadRequest(Collections.singletonList(PRODUCT_CODE)));

    Assertions.assertEquals(BulkAutoApprovedProductsDownloadResponse.class, result.getClass());
    Assertions.assertEquals(2,
        ((BulkAutoApprovedProductsDownloadResponse) result).getAutoApprovedProductsResponses().size());
  }

  @Test
  void getData_EmptyProductCodeEqualResponseList() throws Exception {
    request.setRequestId(REQUEST_ID);
    request.setProductCodeList(Collections.emptyList());
    bulkDownloadAutoApprovedProductsServiceBean.autoApprovedProductsFetchBatchSize = 1;

    List<AutoApprovedListWebResponse> mockResponseList = new ArrayList<>();
    SellerResponse sellerResponse = new SellerResponse();
    AutoApprovedListWebResponse autoApprovedListWebResponse = new AutoApprovedListWebResponse();
    autoApprovedListWebResponse.setSeller(sellerResponse);
    mockResponseList.add(autoApprovedListWebResponse);
    mockResponseList.add(new AutoApprovedListWebResponse());

    Mockito.when(productAnalyticsOutboundService.fetchAutoApprovedProductsDownloadList(
            eq(REQUEST_ID), eq(0), eq(bulkDownloadAutoApprovedProductsServiceBean.autoApprovedProductsFetchBatchSize),
            Mockito.any()))
        .thenReturn(mockResponseList);

    BulkDataResponse result = bulkDownloadAutoApprovedProductsServiceBean.getData(request);

    Mockito.verify(productAnalyticsOutboundService)
        .fetchAutoApprovedProductsDownloadList(any(), anyInt(), anyInt(), any(AutoApprovedWebRequest.class));

    Assertions.assertEquals(BulkAutoApprovedProductsDownloadResponse.class, result.getClass());
    Assertions.assertEquals(2,
        ((BulkAutoApprovedProductsDownloadResponse) result).getAutoApprovedProductsResponses().size());
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productAnalyticsOutboundService);
  }
}
