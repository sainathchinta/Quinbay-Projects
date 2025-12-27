package com.gdn.partners.pcu.internal.service;

import java.io.IOException;
import java.util.List;

import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.partners.pcu.internal.web.model.request.BulkDeleteProductRequest;
import com.gdn.partners.pcu.internal.web.model.response.RejectProductResponse;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.common.exception.ApplicationException;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.web.model.request.PrimaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImageQcWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoConsignmentWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorSummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.AssigneeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BulkUpdatePendingWebResposne;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageFaultyTypeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.NeedRevisionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductBusinessPartnerMapperWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImageQcWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.VendorDetailWebResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;

public interface VendorService {

  /**
   * Returns business partners using primary filters
   *
   * @param page
   * @param size
   * @param request
   * @return
   * @throws Exception
   */
  List<ProductBusinessPartnerMapperWebResponse> getBusinessPartnerList(int page, int size,
  PrimaryFilterWebRequest request) throws Exception;

  /**
   * Returns list of assignee based on primary filters
   *
   * @param page
   * @param size
   * @param request
   * @return
   * @throws Exception
   */
  List<AssigneeWebResponse> getAssigneeList(int page, int size,
      PrimaryFilterWebRequest request) throws Exception;

  /**
   * Get product history by productCode
   * @param productCode
   * @param page
   * @param size
   * @return
   */
  Page<ProductHistoryWebResponse> getProductHistory(String productCode, int page, int size);

  /**
   * Service method to retrieve all product details by product code
   *
   * @param requestId
   * @param username
   * @param productCode
   * @return
   * @throws Exception
   */
  SingleBaseResponse<ProductDetailWebResponse> getProductDetailsByProductCode(String requestId,
         String username, String productCode) throws Exception;

  /**
   * This method is the verify whether merchant has modified product or not while vendor verification.
   *
   * @param productCode
   * @param version
   * @return
   */
  boolean getEditedByMerchant(String productCode, int version);

  /**
   * Get product filter count by vendorCode
   *
   * @param vendorCode
   * @param postLive
   * @param revised
   * @return
   */
  MapResponse getFilterCounts(String vendorCode, Boolean postLive, Boolean edited, Boolean revised);

  /**
   * Do product need correction from vendor
   * @param vendorCode
   * @param request
   */
  NeedRevisionWebResponse doProductNeedCorrection(String vendorCode, ScreeningProductBulkActionsWebRequest request);

  /**
   * Update product Request
   * @param type
   * @param vendorCode
   * @param distributionProductRequest
   */
  void updateProduct(String type, String vendorCode,
      DistributionProductDetailRequest distributionProductRequest);

  /**
   * Fetches list of products to be displayed on vendor review
   *
   * @param page
   * @param size
   * @param vendorSummaryFilterWebRequest
   * @param vendorCode
   * @return
   */
  Page<DistributionProductWebResponse> getVendorProductList(int page, int size,
      VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest, String vendorCode);

  /**
   * Assign and unassign assignee to the products.
   * @param action
   * @param request
   */
  void vendorProductActions(String action, ScreeningProductBulkActionsWebRequest request);

  /**
   *
   * @param vendorCode
   * @param distributionProductRequest
   */
  void approveVendorProduct(String vendorCode, DistributionProductDetailRequest distributionProductRequest);

  /**
   * service method to reject product by vendor
   * @param vendorCode
   * @param request
   * @throws Exception
   */
  List<RejectProductResponse> rejectProductByVendor(String vendorCode, BulkDeleteProductRequest request) throws Exception;

  /**
   * Get product screening notes
   *
   * @param productCode
   * @return
   */
  String getProductScreeningNotes(String productCode);

  /**
   *
   * @param vendorCode
   * @return
   */
  MapResponse getReviewConfigProductCounts(String vendorCode);

  /**
   * Bulk download filtered products
   * @param username
   * @param vendorCode
   * @param vendorSummaryFilterWebRequest
   */
  void bulkDownloadFilteredVendorProducts(String username, String vendorCode,
      VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest);

  /**
   * Send product back to vendor
   *
   * @param productCode
   */
  void sendProductBackToVendor(String productCode);

  /**
   * @param storeId
   * @param vendorCode
   * @param userName
   * @param requestId
   * @param multipartFile
   * @throws Exception
   */
  void saveBulkAssignFile(String storeId, String vendorCode, String userName, String requestId,
      MultipartFile multipartFile) throws Exception;

  /**
   * API to get the vendor list
   */
   List<VendorDetailWebResponse> getVendorList();

  /** Get product image feedback detail
   *
   * @param productCode
   * @return
   */
  ProductImageQcWebResponse getProductImageQcFeedback(String productCode) throws IOException;

  /**
   * Update product image qc feedback
   *
   * @param productImageQcWebRequest
   * @throws JsonProcessingException
   */
  void updateProductImageQcFeedback(ProductImageQcWebRequest productImageQcWebRequest)
      throws JsonProcessingException, ApplicationException;

  /**
   * Get different list of faulty tyoe
   *
   * @return
   */
  List<ImageFaultyTypeWebResponse> getDifferentFaultyType();

  /**
   * Reindex product by product code
   *
   * @param productCode
   * @return
   */
  boolean reindexProductToSolr(String productCode);

  /**
   *
   * @param bulkProcessType
   * @param status
   * @return
   */
  BulkUpdatePendingWebResposne checkCountOfUploads(String bulkProcessType, String status);

  /**
   *
   * @param vendorCode
   * @param postLive
   * @return
   */
  MapResponse getReviewProductCountsForConfig(String vendorCode, boolean postLive);

  /**
   * Republish edited Product
   *
   * @param productCode
   */
  void republishEditedProduct(String productCode);

  /**
   * Get vendor details by vendor code
   * @param vendorCode
   * @return
   */
  VendorDetailResponse getVendorDetail(String vendorCode);

  /*
   * Vendor Product Quick approval
   * @param vendorQuickApprovalRequest
   * @return
   */
  VendorQuickApprovalResponse vendorProductQuickApproval(VendorQuickApprovalRequest vendorQuickApprovalRequest);

  /**
   * Get vendor reviewer list
   * @return
   */
  List<String> getProductReviewers() throws Exception;

  /**
   * Vendor Auto Assignment
   *
   * @param vendorAutoConsignmentWebRequest if defaultSetting true send to pdt
   * @param storeId                         10001
   * @param vendorCode
   * @return
   */
  void autoAssignProducts(VendorAutoConsignmentWebRequest vendorAutoConsignmentWebRequest, String storeId,
    String vendorCode);

  /**
   * Vendor Default Setting
   *
   * @param vendorEmail
   * @return
   */
  VendorDefaultFilterResponse getDefaultSetting(String vendorEmail);

  /**
   * Check pending assignments
   *
   * @param storeId
   * @param username
   * @param processType
   * @return
   */
  InternalProcessPendingFilesResponse checkPendingAssignments(String storeId, String username, String processType);

  /**
   * Check pending assignments
   *
   * @param request
   * @param processType
   * @param requestId
   * @param storeId
   * @param username
   * @return
   */
  void saveBulkReviewFile(MultipartFile request, String processType, String requestId, String storeId,
                             String username) throws Exception;
}