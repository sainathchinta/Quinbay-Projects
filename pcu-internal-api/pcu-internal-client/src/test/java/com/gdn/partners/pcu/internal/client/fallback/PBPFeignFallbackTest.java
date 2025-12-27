package com.gdn.partners.pcu.internal.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.Arrays;

import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.partners.pcu.internal.client.model.request.SystemParameterRequest;
import com.gdn.partners.pcu.internal.client.model.response.ProductL3BasicResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductSystemParameterResponse;
import com.gdn.x.product.enums.Constants;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.GenericStringListRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.ProductHistoryResponse;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingResponse;
import com.gda.mta.product.dto.ProductImagePredictionRequest;
import com.gda.mta.product.dto.ProductImagePredictionResponse;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gda.mta.product.dto.response.SequenceResponse;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.enums.TimeFilterType;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductCollectionCountRestResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

/**
 * Created by govind on 14/01/2019 AD.
 */
public class PBPFeignFallbackTest {

  private static final String MASTER_PRODUCT_ID = "master-product-id";
  private static final String DUPLICATE_PRODUCT_ID = "duplicate-product-id";
  private static final String PRODUCT_ID = "product-id";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String PRODUCT_CODE = "MTA-0000001";
  private static final String STATE = "state";
  private static final String KEYWORD = "keyword";
  private static final String BULK_ACTION = "bulkAction";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY_ID = "productName";
  private static final String PRODUCT_SKU = "productSku";
  private static final String UPC_CODE = "upc_code";
  private static final String BRAND_NAME = "brandName";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String KEY = "key";

  private PBPFeignFallback pbpFeignFallback = new PBPFeignFallback();

  @Test
  public void mergeProductsTest() {
    GdnBaseRestResponse response = pbpFeignFallback.mergeProducts(MASTER_PRODUCT_ID, DUPLICATE_PRODUCT_ID, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void testUpdateAndPublishProductToPDT() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateAndPublishProductToPDT(new ProductRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateProductTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateProduct(new ProductRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void generateBarcodeTest() {
    GdnRestSimpleResponse<String> response = pbpFeignFallback.generateBarcode();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductTest(){
    GdnRestSingleResponse<ProductDetailResponse> response = pbpFeignFallback.getProduct(PRODUCT_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getFilterCountsTest(){
    GdnRestSingleResponse<FilterCountResponse> response =
        pbpFeignFallback.getFilterCounts(Boolean.FALSE, Boolean.FALSE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void productAssignmentTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.productAssignment(PRODUCT_CODE, ASSIGNED_TO, ASSIGNED_BY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductDetailTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.getProductDetail(PRODUCT_CODE, Boolean.FALSE, BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getReviewProductsTest() {
    GdnRestListResponse<ReviewProductResponse> response =
        pbpFeignFallback.getReviewProducts(new SummaryFilterRequest(), Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getBusinessPartnersByTimeAndStatusFilterTest() {
    GdnRestListResponse<ProductBusinessPartnerMapperResponse> response = pbpFeignFallback
        .getBusinessPartnersByTimeAndStatusFilter(new SummaryFilterRequest(), Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateImagePredictionTest() {
    GdnBaseRestResponse response = pbpFeignFallback.updateImagePrediction(new ProductImagePredictionRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateImagePredictionAndCategoryMappingTest() {
    GdnBaseRestResponse response =
        pbpFeignFallback.updateImagePredictionAndCategoryMapping(new ProductImagePredictionAndCategoryMappingRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void approveDraftTest() {
    GdnBaseRestResponse response = pbpFeignFallback.approveDraft(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductHistoryTest() {
    GdnBaseRestResponse response = pbpFeignFallback.getProductHistory(PRODUCT_ID, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getAssigneesByFilterRequestAndActivatedAndViewableFlagTest() {
    GdnRestListResponse<AssigneeResponse> response = pbpFeignFallback
        .getAssigneesByFilterRequestAndActivatedAndViewableFlag(new SummaryFilterRequest(), Boolean.FALSE, Boolean.FALSE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void doScreeningProductsBulkActionsTest() {
    GdnBaseRestResponse response = pbpFeignFallback
        .doScreeningProductsBulkActions(BULK_ACTION, new ScreeningProductBulkActionsRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void doSuspensionActionTest() {
    GdnBaseRestResponse response = pbpFeignFallback.doSuspensionAction(new SuspensionProductRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void findByNameOrUpcCodeTest() {
    GdnBaseRestResponse response = pbpFeignFallback.findByNameOrUpcCode(PRODUCT_NAME, UPC_CODE, CATEGORY_ID,
        Arrays.asList(new AttributeReqModel(PRODUCT_NAME, PRODUCT_CODE)), PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterProductsBySearchKeywordTest() {
    GdnRestListResponse<ProductCodeResponse> response =
        pbpFeignFallback.filterProductsBySearchKeyword(KEYWORD, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductRevisionHistoryTest() {
    GdnRestListResponse<ProductRevisionInfoResponse> response =
        pbpFeignFallback.getProductRevisionHistory(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void submitHistoryTest() {
    GdnBaseRestResponse response = pbpFeignFallback.submitHistory(new ProductHistoryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductsCountByBrandNameTest() {
    GdnRestSimpleResponse<Long> response = pbpFeignFallback.getProductsCountByBrandName(BRAND_NAME);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductScreeningNotesTest() {
    GdnRestSingleResponse<SingleValueResponse> response = pbpFeignFallback.getProductScreeningNotes(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getAllProductsTest() {
    GdnRestListResponse<SuspensionProductResponse> response =
        pbpFeignFallback.getAllProducts(new SummaryFilterRequest(), PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getSuspensionHistoryTest() {
    GdnRestListResponse<ProductSuspensionHistoryResponse> response = pbpFeignFallback.getSuspensionHistory(PRODUCT_SKU,PAGE,SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateProductCategoryTest() {
    GdnBaseRestResponse response = pbpFeignFallback.updateProductCategory(PRODUCT_CODE, CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterProductCollectionSummaryByKeywordTest() {
    GdnRestListResponse<ProductCollectionResponse> response = pbpFeignFallback
        .filterProductCollectionSummaryByKeyword(BUSINESS_PARTNER_CODE, CATEGORY_CODE, null, null, true, true,
            StringUtils.EMPTY, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void countProductCollectionBySpecifiedDateRangeTest() {
    GdnRestSingleResponse<ProductCollectionCountRestResponse> response =
        pbpFeignFallback.countProductCollectionBySpecifiedDateRange(null, null, null, true, true);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getDifferentPredictionTypeTest() {
    GdnRestListResponse<PredictionTypeResponse> response =
        pbpFeignFallback.getDifferentPredictionType();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getListOfPredictionsTest() {
    GdnRestListResponse<ProductImagePredictionResponse> response =
        pbpFeignFallback.getListOfPredictions();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductHistorySummaryTest() {
    GdnRestListResponse<ProductHistoryResponse> response =
        pbpFeignFallback.getProductHistorySummary(PAGE, SIZE, MASTER_PRODUCT_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterProductCollectionSummaryByKeywordAndAgeBetweenTest() {
    GdnRestListResponse<ProductCollectionResponse> response = pbpFeignFallback
        .filterProductCollectionSummaryByKeywordAndAgeBetween(PAGE, SIZE, BUSINESS_PARTNER_CODE, CATEGORY_CODE, KEYWORD,
            null, null, TimeFilterType.TODAY.name(),true, true);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterProductCollectionSummaryByKeywordAndAgeLessThanTest() {
    GdnRestListResponse<ProductCollectionResponse> response = pbpFeignFallback
        .filterProductCollectionSummaryByKeywordAndAgeLessThan(PAGE, SIZE, BUSINESS_PARTNER_CODE, CATEGORY_CODE,
            KEYWORD, null, TimeFilterType.FIVE_DAYS_AGO.name(), true, true);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void publishProductToPDTTest() {
    GdnBaseRestResponse response = pbpFeignFallback.updateProductCategory(PRODUCT_CODE, CATEGORY_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void compareProductAndCategoryWholesaleTest() {
    GdnBaseRestResponse response = pbpFeignFallback.compareProductAndCategoryWholesale(PRODUCT_CODE, CATEGORY_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void findCounterByKeyTest() {
    GdnRestSingleResponse<SequenceResponse> response = pbpFeignFallback.findCounterByKey(KEY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void reindexByProductCodeTest() {
    GdnBaseRestResponse response = pbpFeignFallback.reindexByProductCode(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void reindexActiveProductByProductCodeTest() {
    GdnBaseRestResponse response = pbpFeignFallback.reindexActiveProductByProductCode(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getImagePredictionAndCategoryMappingTest() {
    GenericStringListRequest genericStringListRequest = new GenericStringListRequest();
    GdnRestListResponse<ProductImagePredictionAndCategoryMappingResponse> response =
        pbpFeignFallback.getImagePredictionAndCategoryMapping(genericStringListRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void publishEditedEvent() {
    GdnBaseRestResponse response = pbpFeignFallback.publishEditedEvent(PRODUCT_CODE, KEYWORD);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void publishRevisedEvent() {
    GdnBaseRestResponse response = pbpFeignFallback.publishRevisedEvent(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void retryEditedResizeImage() {
    GdnBaseRestResponse response = pbpFeignFallback.retryEditedResizeImage(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateProductWorkflow() {
    GdnBaseRestResponse response = pbpFeignFallback.updateProductWorkflow(PRODUCT_CODE, STATE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void retryResizeImage() {
    GdnBaseRestResponse response = pbpFeignFallback.retryResizeImage(PRODUCT_CODE, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateReviewPending() {
    GdnBaseRestResponse response = pbpFeignFallback.updateReviewPending(PRODUCT_CODE, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateActivatedAndViewableTest() {
    GdnBaseRestResponse response = pbpFeignFallback.updateActivatedAndViewable(PRODUCT_CODE, false, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void checkAutoApprovalEligibility() {
    GdnRestSingleResponse<AutoApprovalTypeResponse> response =
        pbpFeignFallback.checkAutoApprovalEligibility(PRODUCT_CODE, null);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void deleteProductCollectionFallbackTest() {
    GdnBaseRestResponse response=
      pbpFeignFallback.deleteProductCollection(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID,Constants.DEFAULT_CLIENT_ID,Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME,true,new DeleteProductRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getHalalProductHistoryTest() {
    GdnBaseRestResponse response = pbpFeignFallback.getHalalProductHistory(PRODUCT_SKU, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductSkuByProductCodeTest() {
    GdnBaseRestResponse response = pbpFeignFallback.getProductSkuByProductCode(PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateSystemParameterTest() {
    GdnBaseRestResponse response = pbpFeignFallback.updateSystemParameter(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID,Constants.DEFAULT_CLIENT_ID,Constants.DEFAULT_REQUEST_ID,
        Constants.DEFAULT_USERNAME, new SystemParameterRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchSystemParameterWithShowOnUITrueTest() {
    GdnRestListResponse<ProductSystemParameterResponse> response =
        pbpFeignFallback.fetchSystemParameterShowOnUI(Constants.DEFAULT_STORE_ID,
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void migrateProductAndL5DetailsByProductSkuTrueTest() {
    ProductAndL5MigrationRequest productAndL5MigrationRequest =
      ProductAndL5MigrationRequest.builder().productSku(PRODUCT_SKU)
        .productType(ProductType.BIG_PRODUCT).dimensionsMissing(true).build();
    GdnBaseRestResponse response =
      pbpFeignFallback.migrateProductAndL5DetailsByProductSku(Constants.DEFAULT_STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productAndL5MigrationRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  void getProductLevel3BasicDetailsTest() {
    GdnRestSingleResponse<ProductL3BasicResponse> response =
        pbpFeignFallback.getProductLevel3BasicDetails(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
