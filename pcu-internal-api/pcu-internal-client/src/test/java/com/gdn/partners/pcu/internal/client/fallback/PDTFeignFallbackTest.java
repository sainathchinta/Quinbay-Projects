package com.gdn.partners.pcu.internal.client.fallback;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRProductListRequest;
import com.gdn.partners.pcu.internal.client.model.request.IprActionRequest;
import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRUpdateAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.campaign.clientsdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PrimaryFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorAssigneeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class PDTFeignFallbackTest {

  private PrimaryFilterRequest primaryFilterRequest = new PrimaryFilterRequest();
  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_ID = "product-id";
  private static final String REQUEST_ID = "requestId";
  private static final String USER_NAME = "userName";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String VENDOR_CODE = "vendorCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String STATUS = "status";
  private static final String SORT_BY = "asc";
  private static final int VERSION = 10;

  private PDTFeignFallback pdtFeignFallback = new PDTFeignFallback();

  @Test
  public void getBusinessPartnerListTest() {
    GdnRestListResponse<ProductBusinessPartnerMapperResponse> response =
        pdtFeignFallback.getBusinessPartnerList(PAGE, SIZE, primaryFilterRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getAssigneeListTest() {
    GdnRestListResponse<VendorAssigneeResponse> response =
        pdtFeignFallback.getAssigneeList(PAGE, SIZE, primaryFilterRequest);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductHistory() {
    GdnBaseRestResponse response =
        pdtFeignFallback.getProductHistory(PRODUCT_CODE, PAGE, SIZE, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getEditedByMerchantTest(){
    GdnRestSimpleResponse response = pdtFeignFallback.getEditedByMerchant(PRODUCT_CODE,VERSION);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE,response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductDetails() {
    GdnRestSingleResponse response = pdtFeignFallback.getProductDetails(REQUEST_ID, PRODUCT_CODE, USER_NAME);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void doProductNeedCorrectionTest(){
    GdnRestSingleResponse<NeedRevisionResponse> response = pdtFeignFallback
        .doProductNeedCorrection(VENDOR_CODE, new NeedRevisionRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductFilterInReviewTest() {
    GdnBaseRestResponse response = pdtFeignFallback.getProductFilterInReview(VENDOR_CODE, Boolean.FALSE, Boolean.FALSE,
      Boolean.FALSE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateProductContentTest() {
    GdnBaseRestResponse response = pdtFeignFallback.updateProductContent(StringUtils.EMPTY,
        new DistributionProductDetailRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateProductImageTest() {
    GdnBaseRestResponse response = pdtFeignFallback.updateProductImage(StringUtils.EMPTY,
        new DistributionProductDetailRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void doVendorProductActionsTest() {
    GdnBaseRestResponse response = pdtFeignFallback
        .doVendorProductActions(StringUtils.EMPTY, new ScreeningProductBulkActionsRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void rejectProductTest() {
    GdnBaseRestResponse response = pdtFeignFallback.rejectProduct(StringUtils.EMPTY, new RejectProductVendorRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductListTest() {
    GdnRestListResponse<DistributionProductResponse> response =
        pdtFeignFallback.getProductList(PAGE, SIZE, new FilterSummaryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void approveVendorProductTest() {
    GdnBaseRestResponse response =
        pdtFeignFallback.approveVendorProduct(StringUtils.EMPTY, new DistributionProductDetailRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void sendProductBackToVendorTest() {
    GdnBaseRestResponse response = pdtFeignFallback.sendProductBackToVendor(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductReviewConfigCountsTest() {
    GdnBaseRestResponse response = pdtFeignFallback.getProductReviewConfigCounts(VENDOR_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void countDistributionSummaryByFilterTest() {
    GdnRestSingleResponse<MapResponse> response =
        pdtFeignFallback.countDistributionSummaryByFilter(true, true, new DistributionTaskMultipleFilterRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterProductTest() {
    GdnRestListResponse<DistributionProductResponse> response =
        pdtFeignFallback.filterProduct(STATUS, PAGE, SIZE, new ProductListRequest());
  }

  @Test
  public void getProductImageFeedbackTest() {
    GdnRestSimpleResponse<ProductImageQcFeedbackResponse> response =
        pdtFeignFallback.getProductImageFeedback(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getSummaryByMultipleFilterTest() {
    GdnRestListResponse<DistributionProductResponse> response =
        pdtFeignFallback.getSummaryByMultipleFilter(PAGE, SIZE, SORT_BY, new DistributionTaskMultipleFilterRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getPDTDomainModelResponseByCodeTest() {
    GdnRestSingleResponse<PDTProductDomainEventModelResponse> response =
        pdtFeignFallback.getPDTDomainModelResponseByCode(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void saveProductDistributionTaskTest() {
      GdnBaseRestResponse response = pdtFeignFallback.getProductReviewConfigCounts(VENDOR_CODE);
      assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
      assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
      assertFalse(response.isSuccess());
    }

  @Test
  public void updateProductImageFeedbackTest() {
    GdnBaseRestResponse response = pdtFeignFallback.updateProductImageFeedback(new ProductImageQcFeedbackRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getProductHistoriesTest() {
    GdnRestListResponse<TaskHistoryResponse> response =
        pdtFeignFallback.getProductHistories(PAGE, SIZE, PRODUCT_CODE, false);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

    @Test
  public void isProductExistsTest() {
    GdnRestSimpleResponse<Boolean> response = pdtFeignFallback.isProductExists(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void rejectQCProductTest() {
    GdnBaseRestResponse response = pdtFeignFallback.rejectQCProduct(new RejectProductRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void approveQCProductTest() {
    GdnBaseRestResponse response = pdtFeignFallback.approveQCProduct(PRODUCT_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterProductBusinessPartnerMapperByWorkFlowStateTest() {
    GdnRestListResponse<ProductBusinessPartnerMapperResponse> response = pdtFeignFallback
        .filterProductBusinessPartnerMapperByWorkFlowState(PAGE, SIZE, false, StringUtils.EMPTY, StringUtils.EMPTY);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void deltaReindexPDTProductSolrTest() {
    GdnBaseRestResponse response = pdtFeignFallback.deltaReindexPDTProductSolr(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getReviewConfigCountTest() {
    GdnBaseRestResponse response = pdtFeignFallback.getReviewConfigCount(VENDOR_CODE, Boolean.FALSE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void republishEditedProduct() {
    GdnBaseRestResponse response = pdtFeignFallback.republishEditedProduct(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getVendorByCodeTest() {
    GdnBaseRestResponse response = pdtFeignFallback.getVendorByCode(VENDOR_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void quickApproveProduct() {
    GdnRestSingleResponse<VendorQuickApprovalResponse> response =
        pdtFeignFallback.quickApproveProduct(new VendorQuickApprovalRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateProductStateInPDTTest() {
    GdnBaseRestResponse response = pdtFeignFallback.updateProductStateInPDT(PRODUCT_CODE, null);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void saveDefaultSettingTest() {
    GdnBaseRestResponse response = pdtFeignFallback.saveDefaultSetting(null);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getDefaultSettingFilterTest() {
    GdnBaseRestResponse response = pdtFeignFallback.getDefaultSettingFilter(null);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getIPRProductList() {
    GdnRestListResponse<IPRProductListResponse> response =
        pdtFeignFallback.getIPRProductList(PAGE, SIZE, new IPRProductListRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchIPRProductDetails() {
    GdnRestSingleResponse<IPRProductDetailResponse> response =
        pdtFeignFallback.getIPRProductDetail(PRODUCT_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void updateAssignee() {
    GdnBaseRestResponse response = pdtFeignFallback.updateAssignee(new IPRUpdateAssigneeRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getPrimaryFilterCountsTest() {
    GdnRestSingleResponse<MapResponse> response = pdtFeignFallback.getPrimaryFilterCounts();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void findSuspensionInProgressProduct() {
    GdnRestListResponse<IprSuspensionInProgressResponse> response =
      pdtFeignFallback.findSuspensionInProgressProduct(PAGE, SIZE, BUSINESS_PARTNER_CODE, "ASC");
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void performIprAction() {
    GdnBaseRestResponse response = pdtFeignFallback.performIprAction(new IprActionRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchIprHistory() {
    GdnRestListResponse<IPRProductHistoryResponse> response =
        pdtFeignFallback.fetchIprHistory(PAGE, SIZE, PRODUCT_SKU);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void fetchInternalSystemParameter() {
    GdnBaseRestResponse response =
      pdtFeignFallback.fetchInternalSystemParameter(REQUEST_ID, REQUEST_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
