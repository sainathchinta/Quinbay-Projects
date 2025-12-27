package com.gdn.mta.bulk.repository;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Collections;
import java.util.List;

import com.gdn.mta.bulk.feignConfig.PBPFeign;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.models.download.InstantPickupProductDownloadRequest;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupRequest;

public class ProductInstantPickupRepositoryBeanTest {

  private final static String REQUEST_ID = "requestId";
  private final static String USERNAME = "com.gdn.mta";

  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  @InjectMocks
  private ProductInstantPickupRepositoryBean productInstantPickupRepositoryBean;

  @Mock
  private PBPFeign pbpFeign;

  @Test
  public void testFindSummaryInstantPickupBulkDownload() throws Exception {
    InstantPickupProductDownloadRequest instantPickupProductDownloadRequest =
        InstantPickupProductDownloadRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build();

    OfflineItemInstantPickupRequest offlineItemInstantPickupRequest =
        new OfflineItemInstantPickupRequest();
    offlineItemInstantPickupRequest.setMerchantCode(BUSINESS_PARTNER_CODE);

    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);


    GdnRestListResponse<OfflineItemInstantPickupBulkDownloadResponse> response =
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(),
            new PageMetaData(0, 0, 0), REQUEST_ID);

    Mockito.when(
            pbpFeign.filterSummaryInstantPickupBulkDownload(Constant.STORE_ID, Constant.CHANNEL_ID,
                Constant.CLIENT_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, null, 0, 100))
        .thenReturn(response);

    List<OfflineItemInstantPickupBulkDownloadResponse> responses =
        this.productInstantPickupRepositoryBean
            .findSummaryInstantPickupBulkDownload(instantPickupProductDownloadRequest, 0, 100);

    Mockito.verify(pbpFeign).filterSummaryInstantPickupBulkDownload(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, REQUEST_ID, USERNAME,
      BUSINESS_PARTNER_CODE, null, 0, 100);

    Assertions.assertNotNull(responses);
  }

  @Test
  public void testFindSummaryInstantPickupBulkDownload_blankMDCRequestIdAndUsername()
      throws Exception {
    InstantPickupProductDownloadRequest instantPickupProductDownloadRequest =
        InstantPickupProductDownloadRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
            .pickupPointCode(PICKUP_POINT_CODE).build();

    OfflineItemInstantPickupRequest offlineItemInstantPickupRequest =
        new OfflineItemInstantPickupRequest();
    offlineItemInstantPickupRequest.setMerchantCode(BUSINESS_PARTNER_CODE);
    offlineItemInstantPickupRequest.setPickupPointCode(PICKUP_POINT_CODE);

    GdnRestListResponse<OfflineItemInstantPickupBulkDownloadResponse> response =
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(),
            new PageMetaData(0, 0, 0), REQUEST_ID);

    Mockito.when(pbpFeign.filterSummaryInstantPickupBulkDownload(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), anyString(), eq(Constant.USER_NAME),
        eq(BUSINESS_PARTNER_CODE), eq(PICKUP_POINT_CODE), eq(0), eq(100))).thenReturn(response);

    List<OfflineItemInstantPickupBulkDownloadResponse> responses =
        this.productInstantPickupRepositoryBean
            .findSummaryInstantPickupBulkDownload(instantPickupProductDownloadRequest, 0, 100);

    Mockito.verify(pbpFeign).filterSummaryInstantPickupBulkDownload(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), anyString(),
        eq(Constant.USER_NAME), eq(BUSINESS_PARTNER_CODE), eq(PICKUP_POINT_CODE), eq(0), eq(100));

    Assertions.assertNotNull(responses);
  }

  @Test
  public void testFindSummaryInstantPickupBulkDownload_failed() throws Exception {
    InstantPickupProductDownloadRequest instantPickupProductDownloadRequest =
        InstantPickupProductDownloadRequest.builder().businessPartnerCode(BUSINESS_PARTNER_CODE).build();

    OfflineItemInstantPickupRequest offlineItemInstantPickupRequest =
        new OfflineItemInstantPickupRequest();
    offlineItemInstantPickupRequest.setMerchantCode(BUSINESS_PARTNER_CODE);

    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);

    GdnRestListResponse<OfflineItemInstantPickupBulkDownloadResponse> response =
        new GdnRestListResponse<>("communication failure", "COMMUNICATION_FAILURE", false,
            Collections.emptyList(), new PageMetaData(0, 0, 0), REQUEST_ID);

    Mockito.when(
            pbpFeign.filterSummaryInstantPickupBulkDownload(Constant.STORE_ID, Constant.CHANNEL_ID,
                Constant.CLIENT_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, null, 0, 100))
        .thenReturn(response);

    try {
      List<OfflineItemInstantPickupBulkDownloadResponse> responses =
          this.productInstantPickupRepositoryBean
              .findSummaryInstantPickupBulkDownload(instantPickupProductDownloadRequest, 0, 100);
      Assertions.assertNotNull(responses);
    } catch (Exception e) {
      Mockito.verify(pbpFeign)
          .filterSummaryInstantPickupBulkDownload(Constant.STORE_ID, Constant.CHANNEL_ID,
              Constant.CLIENT_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, null, 0, 100);
    }
  }

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    MDC.clear();

    verifyNoMoreInteractions(this.pbpFeign);
  }

}
