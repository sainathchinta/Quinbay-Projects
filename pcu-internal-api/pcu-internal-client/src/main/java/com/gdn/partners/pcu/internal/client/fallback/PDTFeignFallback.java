package com.gdn.partners.pcu.internal.client.fallback;

import com.gdn.partners.pcu.internal.client.model.request.IPRProductListRequest;
import com.gdn.partners.pcu.internal.client.model.request.IprActionRequest;
import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRUpdateAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import com.gdn.partners.pcu.internal.client.model.response.SystemParameterResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PrimaryFilterRequest;
import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.request.NeedRevisionRequest;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.request.RejectProductRequest;
import com.gdn.x.mta.distributiontask.request.VendorDefaultFilterRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorAssigneeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * Created by shivam 26/06/2019 AD.
 */

@Component
public class PDTFeignFallback implements PDTFeign {

  private PageMetaData pageMetaData = new PageMetaData(0, 0, 0);

  @Override
  public GdnRestListResponse<TaskHistoryResponse> getProductHistory(String productCode, int page,
      int size, boolean escapeString) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<NeedRevisionResponse> doProductNeedCorrection( String vendorCode,
      NeedRevisionRequest needRevisionRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSimpleResponse<Boolean> getEditedByMerchant(String productCode, int version) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<DistributionProductDetailResponse> getProductDetails(String requestId, String username, String productCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateProductContent(String vendorCode,
      DistributionProductDetailRequest productRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnBaseRestResponse updateProductImage(String vendorCode,
      DistributionProductDetailRequest productRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<DistributionProductResponse> getProductList(int page, int size,
      FilterSummaryRequest summaryFilterRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse doVendorProductActions(String action, ScreeningProductBulkActionsRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse rejectProduct(String vendorCode, RejectProductVendorRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnerList(int page, int size,
      PrimaryFilterRequest primaryFilterRequest) {
    return new GdnRestListResponse<ProductBusinessPartnerMapperResponse>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestListResponse<VendorAssigneeResponse> getAssigneeList(int page, int size,
      PrimaryFilterRequest primaryFilterRequest) {
    return new GdnRestListResponse<VendorAssigneeResponse>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSingleResponse<MapResponse> getProductFilterInReview(String vendorCode, Boolean postLive,
    Boolean edited, Boolean revised) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse approveVendorProduct(String vendorCode, DistributionProductDetailRequest productRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse sendProductBackToVendor(String productCode) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestSingleResponse<MapResponse> getProductReviewConfigCounts(String vendorCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<MapResponse> countDistributionSummaryByFilter(Boolean includeStatus,
      Boolean includeVendors, DistributionTaskMultipleFilterRequest request) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<DistributionProductResponse> filterProduct(String status, int page, int size,
      ProductListRequest productListRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<PDTProductDomainEventModelResponse> getPDTDomainModelResponseByCode(String productCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSimpleResponse<ProductImageQcFeedbackResponse> getProductImageFeedback(String productCode) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<DistributionProductResponse> getSummaryByMultipleFilter(Integer page, Integer size,
      String sortBy, DistributionTaskMultipleFilterRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse saveProductDistributionTask(ProductDistributionTaskRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse updateProductImageFeedback(ProductImageQcFeedbackRequest productImageQcFeedbackRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestListResponse<TaskHistoryResponse> getProductHistories(Integer page, Integer size,
      String productCode, boolean escapeString) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnRestSimpleResponse<Boolean> isProductExists(String productCode) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse rejectQCProduct(RejectProductRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnBaseRestResponse approveQCProduct(String productId) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }

  @Override
  public GdnRestListResponse<ProductBusinessPartnerMapperResponse> filterProductBusinessPartnerMapperByWorkFlowState(
      Integer page, Integer size, boolean isSearch, String searchCriteria, String workflowState) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public GdnBaseRestResponse deltaReindexPDTProductSolr(String productCode) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSingleResponse<MapResponse> getReviewConfigCount(String vendorCode, boolean postLive) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);

  }

  @Override
  public GdnBaseRestResponse republishEditedProduct(String productCode) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSingleResponse<VendorDetailResponse> getVendorByCode(String vendorCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }


  @Override
  public GdnRestSingleResponse<VendorQuickApprovalResponse> quickApproveProduct(
      VendorQuickApprovalRequest vendorQuickApprovalRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateProductStateInPDT(String productCode, ProductRetryStatusUpdate productRetryStatusUpdate) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse saveDefaultSetting(VendorDefaultFilterRequest vendorDefaultFilterRequest) {
      return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
          ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<VendorDefaultFilterResponse> getDefaultSettingFilter(String vendorEmail) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<IPRProductListResponse> getIPRProductList(int page, int size,
      IPRProductListRequest iprProductListRequest) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<IPRProductDetailResponse> getIPRProductDetail(String productSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnBaseRestResponse updateAssignee(@RequestBody IPRUpdateAssigneeRequest iprUpdateAssigneeRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSingleResponse<MapResponse> getPrimaryFilterCounts() {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<IprSuspensionInProgressResponse> findSuspensionInProgressProduct(
    int page, int size, String businessPartnerCode, String sortOrder) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnBaseRestResponse performIprAction(IprActionRequest iprActionRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<IPRProductHistoryResponse> fetchIprHistory(Integer page, Integer size,
      String productSku) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestSingleResponse<SystemParameterResponse> fetchInternalSystemParameter(String storeId,
    String requestId) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
      ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}
