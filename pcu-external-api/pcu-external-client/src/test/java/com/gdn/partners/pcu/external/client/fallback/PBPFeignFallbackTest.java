package com.gdn.partners.pcu.external.client.fallback;

import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.LogAuditTrailUpdatedProductResponse;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.OmniChannelExistsRequest;
import com.gda.mta.product.dto.PickupPointUpdateRequest;
import com.gda.mta.product.dto.PickupPointUpdateResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductCopyRequest;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gda.mta.product.dto.ProductImageEditRequest;
import com.gdn.partners.pcu.external.client.model.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3OrderResponse;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3StockInfoWebSiteResponse;
import com.gda.mta.product.dto.ProductLevel3StockRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryCountResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateSummaryRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gda.mta.product.dto.ProductSkuAndPickupPointCodeRequest;
import com.gda.mta.product.dto.ProductSkuListRequest;
import com.gda.mta.product.dto.ProductSystemParameterResponse;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.UpdateImageRequest;
import com.gda.mta.product.dto.UpdateItemsPriceStockImagesRequest;
import com.gda.mta.product.dto.UpdateProductLevel3InfoRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gda.mta.product.dto.response.ItemBulkArchiveResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductCountResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.ProductSkuResponseList;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.ProductSystemParameterSwitchResponse;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.PickupPointCodeResponse;
import com.gdn.mta.product.entity.UniquePickupPointCodeResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pbp.dto.productcategory.CategoryHierarchyProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel1.ProductSearchRequest;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3InactiveResponse;
import com.gdn.partners.pbp.dto.productlevel3.CountProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.EstimateItemPriceResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipDetailResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoUpdateRequest;
import com.gdn.partners.pcu.external.web.model.response.AppealProductConfigResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3DetailsResponse;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class PBPFeignFallbackTest {

  private PBPFeignFallback pbpFeignFallback = new PBPFeignFallback();
  private static final String PRODUCT_NAME = "product_name";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 100;
  private List<String> categoryCodes;
  private static final String CATEGORY_ID = "category_id";
  private static final String ITEM_CODE = "itemCode";
  private static final double LOWEST_PRICE = 1.5;
  private static final String KEYWORD = "keyword";
  private static final String VARIABLE = "variable";
  private static final String GDN_PRODUCT_SKU = "gdn-product-sku";
  private static final String BUSINESS_PARTNER_CODE = "business_partner_code";
  private static final String BUSINESS_PARTNER_ID = "business_partner_id";
  private static final String FILEPATH = "filepath";
  private static final String ORDER_BY = "orderBy";
  private static final String SORT_BY = "sortBy";
  private static final String ITEM_SKU = "item-sku";
  private static final boolean DO_ARCHIVE = true;
  public static final String DEFAULT_CLIENT_HOST = "MTA_WEB";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String USERNAME = "USERNAME";
  private static final String MATERIAL_CODE = "MATERIAL_CODE";
  private static final String STORE_ID = "store-id";
  private static final String PRODUCT_CODE = "product-code";

  @Test
  public void getCategoryHierarchyByKeywordWithProductCountTest() {
    GdnRestListResponse<CategoryHierarchyProductCountResponse> response =
        pbpFeignFallback.getCategoryHierarchyByKeywordWithProductCount(PAGE, SIZE, KEYWORD, null);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @BeforeEach
  public void setUp() throws Exception {
    categoryCodes = Arrays.asList(CATEGORY_CODE);
  }


  @Test
  public void generateProductCode_Test() {
    GdnRestSimpleResponse<String> response = pbpFeignFallback.generateProductCode();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getEstimatedPrice_Test() {
    GdnRestSimpleResponse<EstimateItemPriceResponse> response =
        pbpFeignFallback.getEstimatedPrice(ITEM_CODE, LOWEST_PRICE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void generateBarCode_Test() {
    GdnRestSimpleResponse<String> response = pbpFeignFallback.generateBarCode();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductItemsByNameAndCategoryCodesTest() {
    GdnRestListResponse<ProductItemDetailResponse> response = pbpFeignFallback
        .getProductItemsByNameAndCategoryCodes(PAGE, SIZE, new ProductSearchRequest(PRODUCT_NAME, categoryCodes),
            Boolean.TRUE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductItemSuggestionsTest() {
    GdnRestListResponse<ProductItemResponse> response =
        pbpFeignFallback.getProductItemSuggestions(PAGE, SIZE, PRODUCT_NAME, CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void createTest() {
    GdnBaseRestResponse response = pbpFeignFallback.create(new ProductBusinessPartnerRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void createProductTest() {
    GdnBaseRestResponse response = pbpFeignFallback.createProduct(new ProductCreationRequest(), null);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void createNewProductTest() {
    GdnBaseRestResponse response = pbpFeignFallback.createNewProduct(new ProductCreationRequest(), null);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductsCountByViewableTest() {
    GdnRestSimpleResponse<Integer> response = pbpFeignFallback.getProductsCountByViewable(true);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductDetailByProductSkuTest() {
    GdnRestSingleResponse<ProductLevel3WipDetailResponse> response =
        pbpFeignFallback.getProductDetailByProductSku(GDN_PRODUCT_SKU, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void copyTest(){
    GdnBaseRestResponse response = pbpFeignFallback.copy(false, new ProductCopyRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void copyAllTest() {
    GdnBaseRestResponse response = pbpFeignFallback.copyAll(new ProductCopyRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void generateQRCodeNotificationTest() {
    GdnBaseRestResponse response = pbpFeignFallback.generateQRCodeNotification(BUSINESS_PARTNER_CODE, FILEPATH);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getSuspendedItemTest() {
    GdnRestListResponse<SuspensionItemResponse> response =
        pbpFeignFallback.getSuspendedItem(PAGE, SIZE, new SummaryFilterRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getActiveProductStockCountTest() {
    GdnRestSingleResponse<ProductLevel3SummaryCountResponse> response =
        pbpFeignFallback.getActiveProductStockCount(BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getFilterSummaryTest() {
    GdnRestListResponse<ProductLevel3SummaryResponse> response = pbpFeignFallback
        .filterSummary(BUSINESS_PARTNER_CODE, PAGE, SIZE, ORDER_BY, SORT_BY, new ProductLevel3SummaryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterSummaryWithStateTest() {
    GdnRestListResponse<ProductLevel3WipResponse> response =
        pbpFeignFallback.filterSummaryWithState(PAGE, SIZE, new ProductLevel3WipSummaryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getInProgressProductCountTest() {
    GdnRestSingleResponse<CountProductLevel3WipResponse> response =
        pbpFeignFallback.getInProgressProductCount(BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getNonActiveProductCount() {
    GdnRestSingleResponse<ProductLevel3CountResponse> response =
        pbpFeignFallback.getNonActiveProductCount(BUSINESS_PARTNER_CODE, Constants.ACTIVE_STATUS);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getInActiveProductCountTest() {
      GdnRestSingleResponse<CountProductLevel3InactiveResponse> response =
          pbpFeignFallback.getInActiveProductCount(BUSINESS_PARTNER_CODE);
      assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
      assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
      assertFalse(response.isSuccess());
    }

    @Test
    public void getFilterProductBusinessPartnerSummaryByBusinessPartnerIdTest() {
      GdnRestListResponse<RejectedSkuProductResponse> response = pbpFeignFallback
          .filterProductBusinessPartnerSummaryByBusinessPartnerId(PAGE, SIZE, BUSINESS_PARTNER_CODE, StringUtils.EMPTY, ORDER_BY, SORT_BY);
      assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
      assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
      assertFalse(response.isSuccess());
    }

  @Test
  public void toggleArchiveItemTest() {
    GdnBaseRestResponse response = pbpFeignFallback.toggleArchiveItem(DEFAULT_CLIENT_HOST, ITEM_SKU, DO_ARCHIVE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductUpdateLogsTest() {
    GdnRestListResponse<LogAuditTrailUpdatedProductResponse> response =
        pbpFeignFallback.getProductUpdateLogs(PAGE, SIZE, ITEM_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getCogsValueTest() {
    GdnRestSimpleResponse<CogsValueResponse> response = pbpFeignFallback.getCogsValue(MATERIAL_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterDetailOrderByGdnSkuTest() {
    GdnRestSingleResponse<ProductLevel3OrderResponse> response =
        pbpFeignFallback.filterDetailOrderByGdnSku(ITEM_SKU, BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateSummaryTest() {
    GdnRestSingleResponse<ProductLevel3SummaryResponse> response = pbpFeignFallback
        .updateSummary(BUSINESS_PARTNER_CODE, ITEM_SKU, DEFAULT_CLIENT_HOST, new ProductLevel3UpdateSummaryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void bulkDeleteProductWipTest() {
    GdnRestSingleResponse response =
        pbpFeignFallback.bulkDeleteProductWip(BUSINESS_PARTNER_CODE, new BulkDeleteProductWipRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void retryCreateTest() {
    GdnBaseRestResponse response = pbpFeignFallback.retryCreate(BUSINESS_PARTNER_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void notifyMailVisibilityOptionForProductWipTest() {
    GdnRestSimpleResponse<Boolean> response =
        pbpFeignFallback.notifyMailVisibilityOptionForProductWip(BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterSummaryByGdnSkuTest() {
    GdnRestSingleResponse<ProductLevel3SummaryResponse> response =
        pbpFeignFallback.filterSummaryByGdnSku(BUSINESS_PARTNER_CODE, GDN_PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void activeBrandByCategoryIdTest() {
    GdnRestListResponse<PredefinedAllowedAttributeValueResponse> response =
        pbpFeignFallback.activeBrandByCategoryId(CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void sendEmailForExceededActivationTest() {
    GdnBaseRestResponse response = pbpFeignFallback.sendEmailForExceededActivation(BUSINESS_PARTNER_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getStockInfoWebSiteTest() {
    GdnRestSingleResponse<ProductLevel3StockInfoWebSiteResponse> response =
        pbpFeignFallback.getStockInfoWebSite(BUSINESS_PARTNER_CODE, GDN_PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateAndReturnTest() {
    GdnRestSingleResponse<ProductLevel3Response> response = pbpFeignFallback
        .updateAndReturn(DEFAULT_CLIENT_HOST, Boolean.valueOf(Constants.IS_EXTERNAL_ONLY), true, true, new ProductLevel3Request());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  public void findDetailByGdnSkuTest() {
    GdnRestSingleResponse<ProductLevel3Response> response =
        pbpFeignFallback.findDetailByGdnSku(BUSINESS_PARTNER_CODE, ITEM_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());

  }

  @Test
  public void unsynchronizeProductTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.unsynchronizeProduct(DEFAULT_CLIENT_HOST, GDN_PRODUCT_SKU, ITEM_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void synchronizeProductTest() {
    GdnBaseRestResponse response = pbpFeignFallback.synchronizeProduct(DEFAULT_CLIENT_HOST, GDN_PRODUCT_SKU, ITEM_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void isPristineCategoryTest() {
    GdnRestSimpleResponse<Boolean> response = pbpFeignFallback.isPristineCategory(CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateProductImageTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateProductImage(Boolean.TRUE, new UpdateImageRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void isProductMappedTest() {
    GdnRestSimpleResponse<Boolean> response = pbpFeignFallback.isProductMappedToMerchant(BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateItemStockTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateItemStock(DEFAULT_CLIENT_HOST, BUSINESS_PARTNER_CODE, new ProductLevel3StockRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateItemPriceTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateItemPrice(DEFAULT_CLIENT_HOST, StringUtils.EMPTY, new ProductPriceAndWholesaleRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getMinimumPriceTest() {
    GdnRestSimpleResponse<Integer> response = pbpFeignFallback.getMinimumPrice();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void findSystemParameter() {
    GdnRestSingleResponse<ProductSystemParameterResponse> response = pbpFeignFallback.findSystemParameter(VARIABLE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getSystemParameterSwitchTest() {
    GdnRestSingleResponse<ProductSystemParameterSwitchResponse> response = pbpFeignFallback.getSystemParameterSwitch();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void editProductInfoTest() {
    GdnRestSingleResponse<EditProductV2Response> response = pbpFeignFallback.editProductInfo(new ProductLevel3Request(), "ABC", false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getPickupPointCodesTest() {
    GdnRestListResponse<PickupPointCodeResponse> response =
        pbpFeignFallback.getPickupPointCodes(0, 1, GDN_PRODUCT_SKU, true, BUSINESS_PARTNER_CODE, true);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getUniquePickupPointCodesTest() {
    GdnRestSingleResponse<UniquePickupPointCodeResponse> response =
        pbpFeignFallback.getUniquePickupPointCodes(GDN_PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateItemsPriceStockImagesTest() {
    GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> response =
        pbpFeignFallback.updateItemsPriceStockImages(BUSINESS_PARTNER_CODE, new UpdateItemsPriceStockImagesRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getVariantHistorySummaryTest() {
    GdnRestListResponse<HistoryResponse> response =
        pbpFeignFallback.getProductHistorySummary(0, 1, new HistoryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateLogisticsTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateLogistics(Boolean.valueOf(Constants.IS_EXTERNAL_ONLY), new ProductLevel3UpdateRequest(),
            DO_ARCHIVE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
  }

  @Test
  public void getProductSkusByProductCode() {
    GdnRestSimpleResponse<ProductSkuResponseList> response = pbpFeignFallback.getProductSkusByProductCode(PRODUCT_NAME);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updatePickupPointCodesTest() {
    GdnRestSingleResponse<PickupPointUpdateResponse> response =
        pbpFeignFallback.updatePickupPointCodes(new PickupPointUpdateRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getL3DetailByProductSkuTest() {
    GdnRestSingleResponse<ProductLevel3DetailResponse> response =
        pbpFeignFallback.getL3DetailByProductSku(GDN_PRODUCT_SKU, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void itemListingUpdateTest() {
    GdnRestSingleResponse<ItemBulkArchiveResponse> response =
        pbpFeignFallback.archiveProducts(true, new SimpleListStringRequest());
      assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
      assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
      assertFalse(response.isSuccess());
  }

  @Test
  public void archiveProductsTest() {
    GdnRestSingleResponse<ItemBulkArchiveResponse> response =
        pbpFeignFallback.archiveProducts(true, new SimpleListStringRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchProductSuspensionHistory() {
    GdnRestListResponse<ProductSuspensionHistoryResponse> response =
        pbpFeignFallback.fetchProductSuspensionHistory(new ProductSkuListRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getVendorNotes() {
    GdnRestSimpleResponse<VendorNotesResponse> response =
        pbpFeignFallback.getVendorNotes(PRODUCT_NAME);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateVendorNotes() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateVendorNotes(PRODUCT_NAME, new VendorNotesRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void submitNeedRevisionProductTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.submitNeedRevisionProduct(new NeedRevisionSubmitRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateAppealInProgressProductTest() {
    GdnRestSingleResponse<AppealProductResponse> response =
        pbpFeignFallback.updateAppealInProgressProduct(new AppealProductRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateProductInfoTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateProductInfo(new UpdateProductLevel3InfoRequest(), GDN_PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateImagesTest() {
    GdnBaseRestResponse response = pbpFeignFallback.updateImages(new ProductImageEditRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getL3ProductDetailsByProductSkuTest() {
    GdnRestSingleResponse<ProductL3DetailsResponse> response =
      pbpFeignFallback.getL3ProductDetailsByProductSku(GDN_PRODUCT_SKU, false, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductUpdateHistoryTest() {
    GdnRestListResponse<HistoryUpdateResponse> response =
      pbpFeignFallback.getProductUpdateHistory(PAGE, SIZE, new HistoryUpdateRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void itemListingUpdateV2Test() {
    GdnBaseRestResponse response = pbpFeignFallback.itemListingUpdateV2(GDN_PRODUCT_SKU,
      ProductLevel3QuickEditV2Request.builder().build(),false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getItemPickupPointL3ListingTest() {
    GdnRestListResponse<ItemPickupPointListingL3Response> response =
        pbpFeignFallback.getItemPickupPointL3Listing(PAGE, SIZE, false, DO_ARCHIVE, new ItemPickupPointListingL3Request());
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getL5SummaryByProductSkuListTest() {
    GdnRestListResponse<ProductLevel3SummaryResponse> response =
      pbpFeignFallback.getL5SummaryByProductSkuList(PAGE, SIZE, BUSINESS_PARTNER_CODE,
        new ProductSkuAndPickupPointCodeRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void bulkDownloadSummaryFromDbTest() {
    GdnRestSingleResponse<BulkDownloadProductLevel3Response> response =
        pbpFeignFallback.bulkDownloadSummaryFromDb(BUSINESS_PARTNER_CODE, new ProductLevel3SummaryRequest(), true);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void editProductV2InfoTest() {
    GdnRestSingleResponse<EditProductV2Response> response =
      pbpFeignFallback.editProductV2Info(new ProductL3UpdateRequest(), "ABC", false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchL3V2ProductDetailsByProductSkuTest() {
    GdnRestSingleResponse<ProductLevel3DetailsV2Response> response =
      pbpFeignFallback.fetchL3V2ProductDetailsByProductSku(GDN_PRODUCT_SKU, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void editL5PriceStockInfoTest() {
    GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> response = pbpFeignFallback.editL5PriceStockInfo(null);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getL4ProductDetailsByProductSkuTest() {
    GdnRestListResponse<ItemSummaryL4Response> response =
        pbpFeignFallback.getL4ProductDetailsByProductSku(PRODUCT_NAME, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductCountForProductLimitTest(){
    GdnRestSingleResponse<ProductCountResponse> response =
        pbpFeignFallback.getProductCountForProductLimit(BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getAppealProductConfigTest() {
    GdnRestSingleResponse<AppealProductConfigResponse> response =
      pbpFeignFallback.getAppealProductConfig(STORE_ID, REQUEST_ID, BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateDistributionInfoTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateDistributionInfo(PRODUCT_CODE, new DistributionInfoUpdateRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void checkOmniChannelSkuExistsInSellerTest() {
    OmniChannelExistsRequest omniChannelSkuRequest = new OmniChannelExistsRequest();
    GdnRestSingleResponse<OmniChannelMapAndSkuResponse> response =
      pbpFeignFallback.checkOmniChannelSkuExistsInSeller(omniChannelSkuRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
