package com.gdn.partners.pcu.internal.service;

import java.util.List;

import com.gdn.partners.pcu.internal.client.model.response.ProductL3BasicResponse;
import com.gdn.partners.pcu.internal.web.model.request.HalalProductsFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductHistoryWebResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.web.model.request.CountWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuggestionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SelectedMasterProductDownloadWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SuspensionProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.HalalCertificationWebDetailsResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalDashboardProductsWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HalalProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.TemplateDownloadFilePathWebResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryChangeCheckResponse;
import com.gdn.partners.pcu.internal.web.model.response.FilterCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MapWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductCollectionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductReviewerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductRevisionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuggestionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductSuspensionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ReviewProductWebResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

/**
 * Created by govind on 11/01/2019 AD.
 */
public interface ProductService {

  /**
   * Product Return for Correction
   * @param productReturnForCorrectionRequest
   */
  void returnForCorrection(ProductReturnForCorrectionRequest productReturnForCorrectionRequest);

  /**
   * Merge Product with duplicateProductId to masterProductId
   * @param masterProductId
   * @param duplicateProductId
   * @param isForceMerge
   */
  void mergeProduct(String masterProductId, String duplicateProductId, boolean isForceMerge);

  /**
   * Update product and publish to PDT while reviewing product
   * @param productRequest
   */
  void updateProductAndPublishToPDT(ProductRequest productRequest);

  /**
   * Get product by productId
   * @param productId
   * @return
   */
  ProductDetailResponse findProduct(String productId);

  /**
   * Generate BarCode
   * @return
   */
  String generateBarcode();

  /**
   * Update Product
   * @param productRequest
   * @param isActive
   */
  void updateProduct(ProductRequest productRequest, boolean isActive) throws Exception;

  /**
   * To change product assignment status
   *
   * @param productCode
   * @param assignedTo
   * @param assignedBy
   */
  void updateProductAssignmentStatus(String productCode, String assignedTo, String assignedBy);

  /**
   * Get product info by productCode
   *
   * @param productCode
   * @param inAllProducts
   * @param clientId
   * @param concatenateValueWithValueType
   * @return
   */
  ProductDetailWebResponse getProductDetail(String productCode, boolean inAllProducts, String clientId,
      boolean concatenateValueWithValueType);

  /**
   * Get filter counts by  activated and viewable
   *
   * @param activated
   * @param viewable
   * @return
   */
  FilterCountWebResponse getFilterCounts(boolean activated, boolean viewable);

  Page<ReviewProductWebResponse> getReviewProductsByFilterRequest(ReviewProductsFilterRequest request, boolean activated, boolean viewable,
    int page, int size);

  /**
   * API to suspend/activate products.
   *
   * @param request
   */
  void doSuspensionAction(SuspensionProductBulkActionsWebRequest request);

  /**
   * To approve a draft product
   *
   * @param productRequest
   */
  void approveDraft(ProductRequest productRequest) throws Exception;

  /**
   * Get product history by productId
   *
   * @param productId
   * @param page
   * @param size
   * @return Page<ProductHistoryWebResponse>
   */
  Page<ProductHistoryWebResponse> getProductHistory(String productId, int page, int size);

  /**
   * Do screening products bulk actions
   * @param actionType
   * @param screeningProductBulkActionsWebRequest
   */
  void doScreeningProductsBulkActions(String actionType,
      ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest);

  /**
   * Get product suggestion while screening
   *
   * @param productCode
   * @param productName
   * @param upcCode
   * @param categoryId
   * @param productSuggestionWebRequests
   * @param pageable
   * @return
   */
  List<ProductSuggestionWebResponse> getScreeningSuggestion(String productCode, String productName, String upcCode,
      String categoryId, List<ProductSuggestionWebRequest> productSuggestionWebRequests, Pageable pageable);

  /**
   * Get product screening search
   *
   * @param keyword
   * @param page
   * @param size
   * @return
   */
  List<ProductSuggestionWebResponse> filterProductsBySearchKeyword(String keyword, int page, int size);

  /**
   * return list of vendor who is reviewing the product concurrently
   *
   * @param productCodeRedisKey
   * @param userName
   * @return
   */
  ProductReviewerWebResponse getProductReviewerList(String productCodeRedisKey, String userName);

  /**
   * delete reviewer when he is done with review.
   *
   * @param productCodeRedisKey
   * @param userName
   */
  void deleteProductReviewer(String productCodeRedisKey, String userName);

  /**
   * Get product revision history of product
   * @param productCode
   * @return
   */
  List<ProductRevisionHistoryWebResponse> getProductRevisionHistory(String productCode);

  /**
   * Check if category change is possible
   *
   * @param presentCategory
   * @param targetCategory
   * @param isActive
   * @return
   */
  CategoryChangeCheckResponse checkCategoryChange(String presentCategory, String targetCategory, String productCode, boolean isActive);

  /**
   * Save history of product when category is changed
   *
   * @param productCode
   * @param presentCategory
   * @param requestCategory
   */
  void saveHistoryIfCategoryChanged(String productCode, String presentCategory, String requestCategory);

  /**
   * Get the product details by product code
   * @param productCode
   * @param categoryCode
   * @return
   * @throws Exception
   */
  ProductDetailResponse findDetailByProductCodeAndReplaceCategoryInfo(String productCode, String categoryCode) throws Exception;

  /**
   * Get the attribute details by attribute code
   * @param attributeCode
   * @return
   * @throws Exception
   */
  MasterAttributeResponse getAttributeInfoByAttributeCode(String attributeCode) throws Exception;

  /**
   * Get all the active and suspended products based on filter request
   * @param productSuspensionFilterRequest
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<ProductSuspensionWebResponse> getAllProducts(ProductSuspensionFilterRequest productSuspensionFilterRequest, Pageable pageable) throws Exception;

  /**
   * Get the product suspension history of productSku
   * @param productSku
   * @param page
   * @param size
   * @return
   */
  Page<ProductSuspensionHistoryWebResponse> getSuspensionHistory(String productSku, int page, int size);

  /**
   * Bulk product suspension
   *
   * @param multipartFile
   * @param type
   * @param requestId
   * @param storeId
   * @param userName
   */
   void saveProductSuspensionFile(MultipartFile multipartFile, String type, String requestId, String storeId, String userName)
       throws Exception;

  /**
   * Update product category
   *
   * @param productCode
   * @param categoryCode
   * @return
   */
   boolean updateProductCategory(String productCode, String categoryCode);

  /**
   * Get all the active active products based on filter request
   * @request summaryFilterWebRequest
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<ProductCollectionWebResponse> findProductCollectionSummaryByKeyword(
      SummaryFilterWebRequest summaryFilterWebRequest,
      Pageable pageable) throws Exception;

  /**
   * Bulk product suspension
   *
   * @param multipartFile
   * @param requestId
   * @param storeId
   * @param userName
   */
  void bulkUpdateMasterProductData(MultipartFile multipartFile, String requestId, String storeId, String userName)
      throws Exception;

  /**
   * API to fetch the filter counts for FINAL_QC, DISTRIBUTION_LIST and DALAM_PROCESS.
   *
   * @param countWebRequest
   * @return
   */
  MapWebResponse getFilterCountsBySource(CountWebRequest countWebRequest) throws Exception;

  /**
   * Download selected master products
   * @param username
   * @param selectedMasterProductDownloadWebRequest
   */
  void downloadBulkSelectedMasterProductsForInternal(String username,
      SelectedMasterProductDownloadWebRequest selectedMasterProductDownloadWebRequest);

  /**
   * Download all master products
   * @param username
   * @param summaryFilterWebRequest
   */
  void downloadBulKMasterProducts(String username, SummaryFilterWebRequest summaryFilterWebRequest);

  /**
   * API to fetch the product history
   *
   * @param source
   * @param productCode
   * @param productId
   * @param page
   * @param size
   */
  Page<HistoryWebResponse> findProductHistory(String source, String productCode, String productId, int page,
      int size) throws Exception;

  /**
   * Retry product publish to PDT
   * @param productCode
   */
  boolean retryProductPublishToPDT(String productCode);


  /**
   * Reindex by product Sku
   *
   * @param storeId
   * @param productSku
   */
  void reindexByProductSku(String storeId, String productSku);

  /**
   * @param storeId
   * @param productCode
   * @return
   */
  boolean retryProductNeedRevisionToPBP(String storeId, String productCode);

  /**
   *
   * @return
   * @param productCode
   * @param presentCategoryId
   * @param newCategoryId
   * @param isActive
   * @param productType
   */
  CategoryChangeWebResponse getProductCategoryChangeCheckResponse(String productCode, String presentCategoryId,
      String newCategoryId, boolean isActive, String productType) throws Exception;

  /**
   *
   * @param productCode
   */
  void reindexByProductCode(String productCode);

  /**
   * to get all the internal template download file paths
   *
   * @return
   */
  TemplateDownloadFilePathWebResponse getInternalDownloadTemplateFilePaths();

  /**
   * getting halal product details by product sku
   *
   * @param productSku
   * @return
   */
  HalalProductWebResponse getHalalProductDetailsByProductSku(String productSku);

  /**
   * to get halal certification details
   *
   * @param certificationNumber
   * @param page
   * @param size
   * @return
   */

  Page<HalalCertificationWebDetailsResponse> getHalalCertificationDetails(String certificationNumber, int page,
      int size);

  /**
   * Getting Halal Product History
   *
   * @param productSku
   * @param page
   * @param size
   * @return
   */
  Page<HalalProductHistoryWebResponse> getHalaProductHistory(String productSku, int page, int size);

  /**
   * get halal dashboard products summary
   *
   * @param page
   * @param size
   * @param halalProductsFilterWebRequest
   * @return
   */
  Page<HalalDashboardProductsWebResponse> getHalalDashboardProductsResponses(int page, int size,
      HalalProductsFilterWebRequest halalProductsFilterWebRequest);

  /**
   * to update the halal config of a product
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param productSku
   * @param curationStatus
   */
  void updateHalalConfigOfProduct(String storeId, String requestId, String username, String productSku,
      String curationStatus);

  /**
   * getProductBasicDetails
   *
   * @param productSkus
   * @return List<ProductBasicResponse>
   */
  List<ProductBasicResponse> getProductBasicDetails(List<String> productSkus);

  /**
   * getProductL3BasicDetails
   *
   * @param productCode
   * @return
   */
  ProductL3BasicResponse getProductL3BasicDetails(String productCode);
}
