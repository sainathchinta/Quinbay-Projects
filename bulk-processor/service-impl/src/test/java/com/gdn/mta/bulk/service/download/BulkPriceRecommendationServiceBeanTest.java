package com.gdn.mta.bulk.service.download;

import java.util.Collections;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;

import com.gdn.mta.bulk.models.download.BulkPriceRecommendationDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import com.gdn.mta.bulk.response.BulkPriceAnalyticsResponse;
import com.gdn.mta.bulk.service.PriceAnalyticsOutboundService;

import static org.junit.jupiter.api.Assertions.assertNotNull;


public class BulkPriceRecommendationServiceBeanTest {

  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String BRAND_NAME = "brand_name";
  private static final String BUSINESS_PARTNER_CODE = "TEB-24219";
  private static final String CATEGORY_CODE = "KA-1000039";
  private static final String PRODUCT_TYPE = "product_type";

  @Mock
  private PriceAnalyticsOutboundService priceAnalyticsOutboundService;

  @InjectMocks
  private BulkPriceRecommendationServiceBean bulkPriceRecommendationServiceBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getDataTest() throws Exception {
    BulkPriceRecommendationDownloadRequest bulkPriceRecommendationDownloadRequest =
        BulkPriceRecommendationDownloadRequest.builder()
            .brandNames(Collections.singleton(BRAND_NAME))
            .businessPartnerCodes(Collections.singleton(BUSINESS_PARTNER_CODE))
            .categoryCodes(Collections.singleton(CATEGORY_CODE))
            .productTypes(Collections.singleton(PRODUCT_TYPE))
            .build();
    bulkPriceRecommendationDownloadRequest.setUsername("testUsername");
    List<DownloadSkuResponse> mockResponse = Collections.singletonList(new DownloadSkuResponse());
    Mockito.when(priceAnalyticsOutboundService.getDownloadSkus(bulkPriceRecommendationDownloadRequest.getUsername(),
        bulkPriceRecommendationDownloadRequest)).thenReturn(mockResponse);
    BulkDataResponse response = bulkPriceRecommendationServiceBean.getData(bulkPriceRecommendationDownloadRequest);
    Mockito.verify(priceAnalyticsOutboundService)
        .getDownloadSkus(bulkPriceRecommendationDownloadRequest.getUsername(), bulkPriceRecommendationDownloadRequest);
    assertNotNull(response);
    BulkPriceAnalyticsResponse bulkResponse = (BulkPriceAnalyticsResponse) response;
    Assertions.assertEquals(mockResponse, bulkResponse.getDownloadSkuResponseList());
    Assertions.assertEquals(bulkPriceRecommendationDownloadRequest.getBulkProcessEntity(), bulkResponse.getBulkProcessEntity());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(priceAnalyticsOutboundService);
  }
}
