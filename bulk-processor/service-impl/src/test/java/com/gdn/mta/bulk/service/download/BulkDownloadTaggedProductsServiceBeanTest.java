package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.dto.product.TaggedProductFilterDTO;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.response.BulkDownloadTaggedProductsResponse;
import com.gdn.mta.bulk.service.PriceAnalyticsOutboundService;

public class BulkDownloadTaggedProductsServiceBeanTest {

  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";

  @Mock
  private PriceAnalyticsOutboundService priceAnalyticsOutboundService;

  @InjectMocks
  private BulkDownloadTaggedProductsServiceBean bulkDownloadTaggedProductsServiceBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void getDataTest() throws Exception {
    TaggedProductFilterDTO taggedProductFilterDTO = new TaggedProductFilterDTO();
    taggedProductFilterDTO.setEmailAddress("bulk@gmail.com");
    TaggedProductFilterRequest taggedProductFilterRequest = new TaggedProductFilterRequest();
    taggedProductFilterRequest.setEmailAddress("bulk@gmail.com");
    Mockito.when(priceAnalyticsOutboundService.getDownloadTaggedProducts(taggedProductFilterRequest))
        .thenReturn(new ArrayList<>());
    BulkDataResponse response = bulkDownloadTaggedProductsServiceBean.getData(taggedProductFilterDTO);
    BulkDownloadTaggedProductsResponse bulkDownloadTaggedProductsResponse =
        (BulkDownloadTaggedProductsResponse) response;
    Mockito.verify(priceAnalyticsOutboundService).getDownloadTaggedProducts(taggedProductFilterRequest);
    Assertions.assertEquals("bulk@gmail.com", bulkDownloadTaggedProductsResponse.getEmailAddress());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(priceAnalyticsOutboundService);
  }
}
