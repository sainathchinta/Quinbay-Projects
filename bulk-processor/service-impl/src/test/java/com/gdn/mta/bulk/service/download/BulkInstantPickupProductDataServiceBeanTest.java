package com.gdn.mta.bulk.service.download;


import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.InstantPickupProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkInstantPickupProductResponse;
import com.gdn.mta.bulk.repository.ProductInstantPickupRepository;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupResponse;

public class BulkInstantPickupProductDataServiceBeanTest {

  private static final String REQUEST_ID = "requestId";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String ITEM_SKU = "itemSku";

  private static final int PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 100;

  @InjectMocks
  private BulkInstantPickupProductDataServiceBean bulkInstantPickupProductDataServiceBean;

  @Mock
  private ProductInstantPickupRepository productInstantPickupRepository;

  @Test
  public void testGetData_withEmptyResponse() throws Exception {
    BulkDownloadRequest request = createInstantPickupProductDownloadRequest();

    doReturn(new ArrayList<>()).when(productInstantPickupRepository)
        .findSummaryInstantPickupBulkDownload((InstantPickupProductDownloadRequest) request,
            PAGE_NUMBER, PAGE_SIZE);

    BulkDataResponse actual = bulkInstantPickupProductDataServiceBean.getData(request);

    verify(productInstantPickupRepository).findSummaryInstantPickupBulkDownload(
        (InstantPickupProductDownloadRequest) request, PAGE_NUMBER, PAGE_SIZE);

    BulkInstantPickupProductResponse response = (BulkInstantPickupProductResponse) actual;

    Assertions.assertEquals(0, response.getOfflineItemInstantPickupResponses().size());
  }

  @Test
  public void testGetData_returnResponse() throws Exception {
    BulkDownloadRequest request = createInstantPickupProductDownloadRequest();

    List<OfflineItemInstantPickupResponse> offlineItemInstantPickupResponses = new ArrayList<>();
    OfflineItemInstantPickupResponse offlineItemInstantPickupResponse =
        new OfflineItemInstantPickupResponse();
    offlineItemInstantPickupResponse.setItemSku(ITEM_SKU);
    offlineItemInstantPickupResponses.add(offlineItemInstantPickupResponse);

    doReturn(offlineItemInstantPickupResponses).when(productInstantPickupRepository)
        .findSummaryInstantPickupBulkDownload((InstantPickupProductDownloadRequest) request,
            PAGE_NUMBER, PAGE_SIZE);

    BulkDataResponse actual = bulkInstantPickupProductDataServiceBean.getData(request);

    verify(productInstantPickupRepository).findSummaryInstantPickupBulkDownload(
        (InstantPickupProductDownloadRequest) request, PAGE_NUMBER, PAGE_SIZE);
    verify(productInstantPickupRepository).findSummaryInstantPickupBulkDownload(
        (InstantPickupProductDownloadRequest) request, 1, PAGE_SIZE);

    BulkInstantPickupProductResponse response = (BulkInstantPickupProductResponse) actual;

    Assertions.assertEquals(1, response.getOfflineItemInstantPickupResponses().size());
  }

  private BulkDownloadRequest createInstantPickupProductDownloadRequest() {
    InstantPickupProductDownloadRequest instantPickupProductDownloadRequest =
        InstantPickupProductDownloadRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
            .pickupPointCode(PICKUP_POINT_CODE).build();
    instantPickupProductDownloadRequest.setRequestId(REQUEST_ID);
    return instantPickupProductDownloadRequest;
  }

  @BeforeEach
  public void setUp() {
    initMocks(this);
    ReflectionTestUtils.setField(bulkInstantPickupProductDataServiceBean,
        "getSummaryInstantPickupBatchSize", 100);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productInstantPickupRepository);
  }

}
