package com.gdn.mta.product.repository;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pbp.outbound.xProduct.feign.XProductFeign;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class OfflineItemRepositoryBeanTest {

  private static final String DEFAULT_GDN_SKU = "BLT-00001-00001-00001";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String BUSINESS_PARTNER_CODE = "MTA-0001";
  private static final String ITEM_SKU = "itemSku";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String PICKUP_POINT_CODE = "PPC-00001";
  private static final Double OFFER_PRICE = Double.valueOf(40000);

  @Mock
  private XProductFeign xProductFeign;

  @InjectMocks
  private OfflineItemRepositoryBean offlineItemRepositoryBean;

  private List<DeleteOfflineItemRequest> deleteOfflineItemRequests;
  private DeleteOfflineItemRequest deleteOfflineItemRequest;
  private UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest;

  private String requestId;
  private List<OfflineItemPriceResponse> offlineItemPriceResponses;
  private GdnBaseRestResponse baseRestResponse;

  private OfflineItemResponse offlineItemResponse;
  private List<OfflineItemResponseDetail> offlineItemResponseDetails;

  @Test
  public void findOfflineItemByBusinessPartnerCodeAndItemSkuTest() throws Exception {
    GdnRestListResponse<OfflineItemPriceResponse> responses =
        new GdnRestListResponse<>(this.offlineItemPriceResponses,
            new PageMetaData(this.offlineItemPriceResponses.size(), 0,
                this.offlineItemPriceResponses.size()), DEFAULT_REQUEST_ID);

    when(getProductClient().findOfflinePriceByMerchantCodeAndItemSku(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE), eq(DEFAULT_GDN_SKU))).thenReturn(responses);

    List<OfflineItemPriceResponse> result = offlineItemRepositoryBean
        .findOfflineItemByBusinessPartnerCodeAndItemSku(BUSINESS_PARTNER_CODE,
            DEFAULT_GDN_SKU);

    verify(getProductClient()).findOfflinePriceByMerchantCodeAndItemSku(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE), eq(DEFAULT_GDN_SKU));

    assertEquals(responses.getContent(), result);
  }

  @Test
  public void findOfflineItemByBusinessPartnerCodeAndItemSkuWithExceptionTest() throws Exception {
    GdnRestListResponse<OfflineItemPriceResponse> responses =
        new GdnRestListResponse<>(null, null, false, DEFAULT_REQUEST_ID);

    when(getProductClient().findOfflinePriceByMerchantCodeAndItemSku(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE), eq(DEFAULT_GDN_SKU))).thenReturn(responses);

    try {
      List<OfflineItemPriceResponse> result = getOfflineItemRepositoryBean()
          .findOfflineItemByBusinessPartnerCodeAndItemSku(BUSINESS_PARTNER_CODE,
              DEFAULT_GDN_SKU);
      assert false;
    } catch (Exception e) {
      verify(getProductClient()).findOfflinePriceByMerchantCodeAndItemSku(anyString(), anyString(),anyString(),anyString(), anyString(),
          eq(BUSINESS_PARTNER_CODE), eq(DEFAULT_GDN_SKU));
    }
  }

  @Test
  public void findOfflineItemByBusinessPartnerCodeAndMerchantSkusTest() throws Exception {
    GdnRestSingleResponse<OfflineItemResponse> response =
        new GdnRestSingleResponse<>(this.offlineItemResponse, DEFAULT_REQUEST_ID);

    List<String> merchantSkusListRequest = new ArrayList<>();
    when(getProductClient().getOfflineItemsByMerchantCodeAndMerchantSkus(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE), any())).thenReturn(response);

    OfflineItemResponse result = getOfflineItemRepositoryBean()
        .findOfflineItemByBusinessPartnerCodeAndMerchantSkus(BUSINESS_PARTNER_CODE,
            merchantSkusListRequest);

    verify(getProductClient()).getOfflineItemsByMerchantCodeAndMerchantSkus(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE), any());

    assertEquals(response.getValue(), result);
  }

  @Test
  public void findOfflineItemByBusinessPartnerCodeAndMerchantSkusWithExceptionTest() throws Exception {
    GdnRestSingleResponse<OfflineItemResponse> response =
        new GdnRestSingleResponse<>("error", "error", false, null, DEFAULT_REQUEST_ID);

    List<String> merchantSkusListRequest = new ArrayList<>();
    when(getProductClient().getOfflineItemsByMerchantCodeAndMerchantSkus(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE),any())).thenReturn(response);

    Exception exception = new Exception();
    try {
      OfflineItemResponse result = getOfflineItemRepositoryBean()
          .findOfflineItemByBusinessPartnerCodeAndMerchantSkus(BUSINESS_PARTNER_CODE,
              merchantSkusListRequest);
      assert false;
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertEquals(exception.getMessage(), "Unspecified error :[error] error");
      verify(getProductClient()).getOfflineItemsByMerchantCodeAndMerchantSkus(anyString(), anyString(),anyString(),anyString(), anyString(),
          eq(BUSINESS_PARTNER_CODE), any());
    }
  }

  @Test
  public void updateOfflineItemPriceByItemSkuTest() throws Exception {
    this.baseRestResponse = new GdnBaseRestResponse(this.requestId);

    when(getProductClient().updateOfflineItemPriceByItemSku(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE), eq(this.updateOfflineItemPriceRequest))).thenReturn(this.baseRestResponse);

    GdnBaseRestResponse response = getOfflineItemRepositoryBean().updateOfflineItemPriceByItemSku(
        BUSINESS_PARTNER_CODE, this.updateOfflineItemPriceRequest);

    verify(getProductClient()).updateOfflineItemPriceByItemSku(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE), eq(this.updateOfflineItemPriceRequest));

    assertTrue(response.isSuccess());
  }

  @Test
  public void updateOfflineItemPriceByItemSkuTest_exception() throws Exception {
    this.baseRestResponse = new GdnBaseRestResponse();

    when(getProductClient().updateOfflineItemPriceByItemSku(anyString(), anyString(),anyString(),anyString(), anyString(),
        eq(BUSINESS_PARTNER_CODE), eq(this.updateOfflineItemPriceRequest))).thenReturn(this.baseRestResponse);

    try {
      this.getOfflineItemRepositoryBean().updateOfflineItemPriceByItemSku(
          BUSINESS_PARTNER_CODE, this.updateOfflineItemPriceRequest);
    } catch (ApplicationException ae) {
      assertEquals(ErrorCategory.UNSPECIFIED, ae.getErrorCodes());
      verify(getProductClient()).updateOfflineItemPriceByItemSku(anyString(), anyString(),anyString(),anyString(), anyString(),
          eq(BUSINESS_PARTNER_CODE), eq(this.updateOfflineItemPriceRequest));
    }
  }

  public OfflineItemRepositoryBean getOfflineItemRepositoryBean() {
    return offlineItemRepositoryBean;
  }

  public XProductFeign getProductClient() {
    return xProductFeign;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    setRequestId(UUID.randomUUID().toString());

    this.offlineItemPriceResponses = new ArrayList<>();

    this.deleteOfflineItemRequests = new ArrayList<>();
    this.deleteOfflineItemRequest = new DeleteOfflineItemRequest();
    this.deleteOfflineItemRequest.setItemSku(ITEM_SKU);
    this.deleteOfflineItemRequest.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    this.deleteOfflineItemRequests.add(this.deleteOfflineItemRequest);

    this.updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    this.updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    this.updateOfflineItemPriceRequest.setOfferPrice(OFFER_PRICE);

    this.offlineItemResponse = new OfflineItemResponse();
    this.offlineItemResponseDetails = new ArrayList<>();
    this.offlineItemResponse.setOfflineProducts(this.offlineItemResponseDetails);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.getProductClient());
  }
}
