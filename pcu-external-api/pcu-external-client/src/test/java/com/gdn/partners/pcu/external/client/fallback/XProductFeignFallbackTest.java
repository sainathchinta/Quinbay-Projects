package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.model.BusinessPartnerPickupPointOutboundResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.ReelProductListingWebRequest;
import com.gdn.partners.pcu.external.web.model.response.ItemL5ListingResponse;
import com.gdn.partners.pcu.external.web.model.response.L2StockDetailResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductBasicWebResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpcStatusRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreRuleResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleLongResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class XProductFeignFallbackTest {

  private XProductFeignFallback xProductFeignFallback = new XProductFeignFallback();
  private XInventoryFeignFallback xInventoryFeignFallback = new XInventoryFeignFallback();
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 100;
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String STORE_ID = "STORE_ID";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String MERCHANT_CODE = "MERCHANT_CODE";
  private static final String KEY = "KEY";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String ACTIVE = "ACTIVE";
  private static final String SIZE_CHART_CODE = "SIZE_CHART_CODE";
  private List<String> categoryCodes;
  private ActiveProductRequest activeProductRequest = new ActiveProductRequest();
  private ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
  private ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();

  @Test
  public void getActiveProductListByMerchantAndCategoryCodeTest() {
    activeProductRequest.setMerchantCode(MERCHANT_CODE);
    activeProductRequest.setCategoryCodes(categoryCodes);
    activeProductRequest.setBuyable(true);
    activeProductRequest.setDiscoverable(true);
    activeProductRequest.setSearchKey(KEY);
    GdnRestListResponse<ActiveProductResponse> response =
        xProductFeignFallback.getActiveProductListByMerchantAndCategoryCode(PAGE, SIZE,
            activeProductRequest);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getActiveProductNamesByMerchantCodeTest() {
    activeProductRequest.setMerchantCode(MERCHANT_CODE);
    activeProductRequest.setCategoryCodes(categoryCodes);
    activeProductRequest.setBuyable(true);
    activeProductRequest.setDiscoverable(true);
    activeProductRequest.setSearchKey(KEY);
    GdnRestListResponse<ItemSummaryResponse> response =
        xProductFeignFallback.getActiveProductNamesByMerchantCode(PAGE, SIZE, itemSummaryRequest);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getSuspendedItemListTest() {
    activeProductRequest.setMerchantCode(MERCHANT_CODE);
    activeProductRequest.setCategoryCodes(categoryCodes);
    activeProductRequest.setBuyable(true);
    activeProductRequest.setDiscoverable(true);
    activeProductRequest.setSearchKey(KEY);
    GdnRestListResponse<ItemSummaryResponse> response =
        xProductFeignFallback.getSuspendedItemList(PAGE, SIZE, activeProductRequest);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @BeforeEach
  public void setUp() throws Exception {
    categoryCodes = Arrays.asList(CATEGORY_CODE);
  }

  @Test
  public void getProductScoreRuleTest() {
    GdnRestSingleResponse<ProductScoreRuleResponse> response = xProductFeignFallback.getProductScoreRule(CATEGORY_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductAndItemsTest() {
    GdnRestSingleResponse<ProductAndItemsResponse> response =
        xProductFeignFallback.getProductAndItems(STORE_ID, REQUEST_ID, PRODUCT_SKU, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getL3CountByProductCode() {
    GdnRestSingleResponse<SimpleLongResponse> response =
        xProductFeignFallback.getL3CountByProductCode(STORE_ID, REQUEST_ID, PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getPickupPointCodesByProductSkuTest() {
    GdnRestSingleResponse<ProductPickupPointListResponse> response =
        xProductFeignFallback.getPickupPointCodesByProductSku(PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductCountByTypeTest() {
    GdnRestSingleResponse<ProductCountResponse> response =
        xProductFeignFallback.getProductCountByType(ACTIVE, MERCHANT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getSecondaryProductCountByType() {
    GdnRestSingleResponse<ProductCountResponse> response =
        xProductFeignFallback.getSecondaryProductCountByType(ACTIVE, MERCHANT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getFilterSummaryL3Test() {
    GdnRestListResponse<ProductL3SummaryResponse> response =
        xProductFeignFallback.getFilterSummaryL3(PAGE, SIZE, false, productSummaryRequest);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getProductNamesByFilterTest() {
    GdnRestListResponse<ProductNameSuggestionResponse> response =
        xProductFeignFallback.getProductNamesByFilter(PAGE, SIZE, productSummaryRequest);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getItemNameByItemSkusFallbackTest() {
    GdnRestSingleResponse<SimpleMapStringResponse> response =
      xProductFeignFallback.getItemNameByItemSkus(new SimpleListStringRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getBusinessPartnerPickupPointSummaryTest() {
    GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> response =
        xProductFeignFallback.getBusinessPartnerPickupPointSummary(0, 1, new PickupPointSummaryRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getPickupPointDetailByCodesTest() {
    GdnRestListResponse<PickupPointDetailResponse> response =
      xProductFeignFallback.getPickupPointDetailByCodes(new SimpleListStringRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getPickupDetailByCodes() {
    GdnRestListResponse<BusinessPartnerPickupPointOutboundResponse> response =
      xProductFeignFallback.getPickupDetailByCodes(new SimpleListStringRequest(), "merchantCode");
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getL4ItemListByProductSkuFallbackTest() {
    GdnRestListResponse<ItemLevel4ListingResponse> response =
      xProductFeignFallback.getL4ItemListByProductSku(STORE_ID, REQUEST_ID, PAGE, SIZE,
        new ItemLevel4ListingWebRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getItemPickupPointCodeByItemSkusTest() {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> response =
        xProductFeignFallback.getItemPickupPointCodeByItemSkus(new SimpleListStringRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getItemBasicDetailsTest() {
    GdnRestListResponse<ItemBasicDetailV2Response> response =
        xProductFeignFallback.getItemBasicDetails(PRODUCT_SKU);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getItemL5DetailsTest() {
    GdnRestListResponse<ItemL5ListingResponse> response =
        xProductFeignFallback.getItemL5Details(PAGE, SIZE, PRODUCT_SKU, false, false, new SimpleListStringRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getItemBasicDetails() {
    GdnRestListResponse<ItemBasicDetailV2Response> response =
        xProductFeignFallback.getItemBasicDetails(false, new SimpleListStringRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void sharedProductByProductCodeTest() {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
        xProductFeignFallback.sharedProductByProductCode(PRODUCT_CODE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void fetchBasicItemDetailsByItemCodesTest() {
    GdnRestListResponse<ItemCodeBasicDetailResponse> response =
      xProductFeignFallback.fetchBasicItemDetailsByItemCodes(STORE_ID, REQUEST_ID, null, PAGE,
        SIZE, "itemSku", "asc", "itermCode", StringUtils.EMPTY);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void fetchStockDetailsByWarehouseItemSku() {
    GdnRestSingleResponse<L2StockDetailResponse> response =
      xInventoryFeignFallback.findStockAvailabilityByWarehouseItemSku("MTA-51366510-00003");
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void updateProductSizeChartTest() {
    GdnBaseRestResponse response = xProductFeignFallback.updateProductSizeChart(SIZE_CHART_CODE, new ProductSizeChartUpdateRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getUpcStatus() {
    GdnBaseRestResponse response = xProductFeignFallback.getUpcStatus(new UpcStatusRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void republishProductsToAgp() {
    GdnBaseRestResponse response = xProductFeignFallback.republishProductsToAgp(new ArrayList<>());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void republishItemsToAgp() {
    GdnBaseRestResponse response = xProductFeignFallback.republishItemsToAgp(new ArrayList<>());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void republishItemPickupPointToAgp() {
    GdnBaseRestResponse response = xProductFeignFallback.republishItemPickupPointToAgp(false, new ArrayList<>());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getReelProductListTest() {
    GdnBaseRestResponse response =
        xProductFeignFallback.getReelProductList(0, 10, new ReelProductListingWebRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getProductBasicDetailsTest() {
    GdnRestListResponse<ProductBasicWebResponse> response =
        xProductFeignFallback.getProductBasicDetails(new SimpleListStringRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }


}
