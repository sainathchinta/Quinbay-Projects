package com.gdn.mta.product.service;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gda.mta.product.dto.AutoApprovalsDetailDto;
import com.gda.mta.product.dto.MasterProductEditDTO;
import com.gda.mta.product.dto.NeedRevisionProductsRequest;
import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gda.mta.product.dto.response.AgpSimpleQueryResponse;
import com.gda.mta.product.dto.response.OmniChannelSkuResponse;
import com.gda.mta.product.dto.response.ProductAndBrandResponse;
import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gdn.mta.product.enums.AddDeleteVariantStatus;
import com.gdn.mta.product.enums.ApiErrorCode;
import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.AutoNeedRevisionAndForceReviewResponse;
import com.gda.mta.product.dto.DalamProductListRequest;
import com.gda.mta.product.dto.DimensionRefreshRequest;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductDetailCompleteResponse;
import com.gda.mta.product.dto.ProductFilterRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gda.mta.product.dto.ValidateDuplicateProductRequest;
import com.gda.mta.product.dto.ValidateDuplicateProductResponse;
import com.gda.mta.product.dto.generator.ProductWfStateResponse;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.ImageQcEnableAndSyncResponse;
import com.gda.mta.product.dto.response.InProgressProductResponsePageResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.ProductFilterResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.common.base.service.GdnBaseService;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.micro.graphics.domain.event.model.BulkImageProcessResponse;
import com.gdn.micro.graphics.domain.event.model.ScaleImageResponse;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ApproveProductResponse;
import com.gdn.mta.domain.event.modal.ImageQcProcessedResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcRequestDomainEvent;
import com.gdn.mta.domain.event.modal.ImageQcResponseDomainEvent;
import com.gdn.mta.domain.event.modal.ProcessImageDomainEvent;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.mta.domain.event.modal.ProductWipDeleteResponse;
import com.gdn.mta.domain.event.modal.StuckProductEventPublishDto;
import com.gdn.mta.product.entity.ItemFlagDetails;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductMigration;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountRequest;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.BasicSizeChartDetailMapResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;

public interface ProductService extends GdnBaseService<Product> {

  void approvedContent(String storeId, Product product) throws Exception;

  void approvedImage(String storeId, Product product) throws Exception;

  List<ProductWorkflow> checkProductWorkflowsAlreadyExist(Product product) throws Exception;

  void delete(String productCode, String notes) throws Exception;

  void delete(String productCode, String notes, String state) throws Exception;

  void delete(String productCode, String productName, String notes, String state) throws Exception;

  void retryDelete(String id, String storeId, String productCode, String productName, Boolean isMarkForDelete) throws Exception;

  /**
   * update product information
   * @param  product : must not be null
   * @param  notes
   * @param brandCode
   * @param brandApprovalStatus
   * @throws Exception
   */
  void update(Product product, String notes, String brandCode, String brandApprovalStatus) throws Exception;


  /**
   * delete product business partner, so that they appear in external rejected list
   *
   * @param productCollection
   * @param notes
   * @throws Exception
   */
  void deleteProductBusinessPartnerForPostLiveRejection(
      List<ProductBusinessPartner> savedProductBusinessPartners,
      ProductCollection productCollection, String notes) throws Exception;

  /**
   * get product detail response
   *
   * @param id must not blank
   * @return product detail response
   * @throws Exception
   */
  ProductDetailResponse findDetailById(String id) throws Exception;

  List<ProductItemResponse> findProductItemByProductId(String id) throws Exception;

  Page<ProductItem> findByKeywordAndViewable(String storeId, String keyword, boolean viewable,
      boolean isOnlyExternal, Pageable pageable)
      throws Exception;

  Page<ProductItem> findByProductItemNameAndCategoryId(String storeId, String productItemName,
      String categoryId, Pageable pageable) throws Exception;

  Page<Product> findByName(String storeId, String name, Pageable pageable) throws Exception;

  Page<Product> findByNameAndViewableAndActivated(String storeId, String name, boolean viewable, boolean activated,
      Pageable pageable) throws Exception;

  /**
   * @deprecated use {@link #findByProductCodeExactMatch(String, String, Pageable)} instead
   *
   * @param storeId store id
   * @param productCode product code
   * @param pageable page information
   * @return page of product
   * @throws Exception when failed to find product
   */
  @Deprecated
  Page<Product> findByProductCode(String storeId, String productCode, Pageable pageable) throws Exception;

  Page<ProductItem> findByUpcCode(String storeId, String upcCode, Pageable pageable) throws Exception;

  /**
   * Validate Duplicate Product
   *
   * @param storeId
   * @param merchantCode
   * @param validateDuplicateProductRequest
   * @return
   * @throws Exception
   */
  ValidateDuplicateProductResponse validateDuplicateProduct(String storeId, String merchantCode,
      ValidateDuplicateProductRequest validateDuplicateProductRequest) throws Exception;

  Page<ProductCodeResponse> findByNameOrUpcCode(String storeId, String productName,
      String upcCode, String finalCategoryId, List<AttributeReqModel> modelList, Pageable pageable)
          throws Exception;

  /**
   * find product by viewable flag
   * @param storeId store id
   * @param viewable viewable flag searched
   * @param pageable page info
   * @return page of product
   * @throws Exception when failed to connect
   * @deprecated do not use this method
   */
  @Deprecated
  Page<Product> findByViewable(String storeId, boolean viewable, Pageable pageable) throws Exception;

  Page<Product> findByViewableAndActivated(String storeId, boolean viewable, boolean activated, Pageable pageable)
      throws Exception;

  /**
   * service call to get product collection details from Solr
   *
   * @param storeId
   * @param keyword
   * @param categoryCode
   * @param reviewPending
   * @param sortBy
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<SolrProductCollectionDTO> getActiveProductCollectionFromSolr(String storeId, String keyword, String categoryCode,
      Boolean reviewPending, String sortBy, Pageable pageable) throws Exception;

  /**
   * find product detail for given product code
   *
   * @param productCode
   * @param inAllProducts
   * @return
   * @throws Exception
   */
  ProductDetailResponse findProductDetailByProductCode(String productCode, boolean inAllProducts) throws Exception;

  /**
   * find product detail with preOrder for given product code
   *
   * @param storeId
   * @param productCode
   * @param inAllProducts
   * @param businessPartnerCode
   * @return
   * @throws Exception
   */
  ProductDetailCompleteResponse findProductDetailWithPreOrderByProductCode(String storeId, String productCode,
      boolean inAllProducts, String businessPartnerCode) throws Exception;

  /**
   * Publish image qc for content edit
   *
   * @param categoryResponses
   * @param restrictedKeywordsByFieldAndActionType
   * @param productLevel3
   * @return
   */
  ImageQcRequestDomainEvent publishImageQcEventForContentEdit(List<CategoryResponse> categoryResponses,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType, ProductLevel3 productLevel3, boolean needRevisionSubmit);

  /**
   * Update product colllection resize flag
   *
   * @param productCollection
   * @param resize
   * @param imageQcState
   * @return
   */
  ProductCollection updateProductCollectionResizedFlag(ProductCollection productCollection, boolean resize,
      int imageQcState);

  /**
   * @param storeId
   * @param productDetailResponse
   * @return
   */
  List<RestrictedKeywordsByFieldResponse> setBrandCodeAndBrandApprovalStatus(String storeId,
      ProductDetailResponse productDetailResponse);

  List<ProductWorkflow> findProductWorkflows(String storeId, String productId) throws Exception;

  String generateBarcode(String storeId) throws Exception;

  @Deprecated
  Double generateShippingWeight(Product product) throws Exception;

  void saveCallback(Product product) throws Exception;

  String saveProductCollection(String businessPartnerCode, String businessPartnerName, Product product)
      throws Exception;

  void submit(String storeId, Product product) throws Exception;

  void updateForMerge(Product product) throws Exception;

  /**
   * Update bulk master product details
   * @param simpleMasterProductUpdateRequestDTO
   * @param storeId
   * @param productCollection
   * @return
   * @throws Exception
   */
  SimpleMasterProductUpdateResponse updateForBulk(
      SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO, String storeId,
      ProductCollection productCollection) throws Exception;

  /**
   * Update product details
   * @param product
   * @param onlyProduct
   * @param notes
   * @param brandCode
   * @param brandApprovalStatus
   * @throws Exception
   */
  void update(Product product, boolean onlyProduct, String notes, String brandCode, String brandApprovalStatus)
      throws Exception;

  /**
   * Update product details
   * @param product
   * @param onlyProduct
   * @param notes
   * @param brandCode
   * @param brandApprovalStatus
   * @param isMarginExceeded
   * @param postLive
   * @param forceReviewNotes
   * @param onlyVatChanged
   * @throws Exception
   */
  void update(Product product, boolean onlyProduct, String notes, String brandCode, String brandApprovalStatus,
    boolean isMarginExceeded, boolean postLive, String forceReviewNotes, boolean onlyVatChanged) throws Exception;

  void publishToPDTByProductCode(String storeId, String productCode) throws Exception;

  void publishToPDTByProductCollection(ProductCollection productCollection) throws Exception;

  Boolean validateBarcode(String storeId, String barcode) throws Exception;

  /**
   * Get Basic details of product
   * 
   * @param productCode
   * @return
   * @throws Exception
   */
  ProductResponse findProductBasicDetailByProductCode(String productCode) throws Exception;

  Page<ProductHistory> findProductHistoryByStoreIdAndProductId(String storeId, String productId, Pageable pageable)
      throws Exception;

  /**
   * validate current bpCode is authorized to create product with given categoryCode
   * @param requestId id of requester
   * @param username username of requester
   * @param bpCode business partner code
   * @param categoryCode category code
   * @throws Exception when failed to validate
   */
  void validateCategory(String requestId, String username, String bpCode, String categoryCode) throws Exception;


  /**
   * updates WCS and X-INVENTORY With new merged product mappings
   * @param masterDataProduct master data of the product
   * @param productBusinessPartner pbp data
   * @throws Exception when failed to update merchant product
     */
  void updateWCSAndInventoryForMergedProduct(Product masterDataProduct,
      ProductBusinessPartner productBusinessPartner, Map<String, String> oldToNewProductItemIdMap)
      throws Exception;

  void processImage(String storeId, String productCode, boolean retryProcessImage) throws Exception;

  void rejectProcessImage(String storeId, String productCode) throws Exception;

  boolean approveImage(String storeId, String productCode, boolean retryApproveImage) throws Exception;

  /**
   * Delete images from source folder after image approval is done
   * @param storeId
   * @param productCode
   * @throws IOException
   */
  void deleteImages(String storeId, String productCode) throws IOException;

  ActivateImageResponse updateProductImageName(ActivateImageRequest request) throws Exception;

  /**
   * Update prd_collection solr
   *
   * @param productCollection
   */
  void updateSolrProductCollection(ProductCollection productCollection);

  /**
   * update image locations in PCB for product and Items
   * @param request
   * @param skipReview
   * @return
   * @throws Exception
   */
  ActivateImageResponse updateProductImagesName(ProductActivateImageRequest request, boolean skipReview)
      throws Exception;

  void approveContent(String storeId, String productCode, boolean retryApproveContent,
      boolean isProductActivated) throws Exception;

  void approveDraft(String storeId, String productCode) throws Exception;



  /**
   * Get product by exact match of product code
   *
   * @param storeId store id
   * @param productCode product code
   * @param pageable page information
   * @return Page of product filtered by store id, product code
   * @throws Exception when failed to find product
   */
  Page<Product> findByProductCodeExactMatch(String storeId, String productCode, Pageable
      pageable) throws Exception;

  /**
   * API to get product count by viewable criteria
   *
   *
   * @param storeId
   * @param viewable viewable flag status
   * @return number of viewable product
   * @throws Exception when failed to get product count
   */
  Integer getProductCountByViewable(String storeId, boolean viewable) throws Exception;

  /**
   * API to send mail to merchants whose product has been become live
   *
   * @param startUpdatedDate start date of notification
   * @param endUpdatedDate end date of notification
   * @throws Exception when failed to get list of recent approved product
   */
  void notifyMerchantOfRecentApprovedProducts(Date startUpdatedDate, Date endUpdatedDate);

  void updateProductContent(ProductRequest request) throws Exception;

  void updateProductImage(ProductRequest request) throws Exception;

  /**
   * service method to get product details by product codes
   *
   * @param requestId id of requester
   * @param username username of requester
   * @param productCodeList list of product code to search
   * @return list of ProductDetailResponse
   * @throws Exception when failed to get data
   */
  GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodes(
      String requestId, String username, List<String> productCodeList) throws Exception;

  /**
   * <p>Activate product image and name</p>
   *
   * @param request
   * @throws Exception
   */
  void activateAndUpdateImageName(ActivateImageRequest request) throws Exception;

  /**
   * <p>Get list all location path of product images and item images</p>
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  ActivateImageResponse isProductImagesActivated(String productCode) throws Exception;

  /**
   * create product level3
   *
   * @param productCode
   * @param isSkipNotification
   * @throws Exception
   */
  void createProductLevel3(String productCode, boolean isSkipNotification) throws Exception;

  /**
   * update rejected product
   *
   * @param request
   * @throws Exception
   */
  void updateRejectedProduct(ProductRequest request) throws Exception;

  Map<String, Object> getCategoryHierarchyByProductNameOrProductCode(String storeId,
      String keyword, Pageable pageable, String businessPartnerCode) throws Exception;

  Page<ProductItemResponse> getProductItemByKeywordAndCategoryCode(String keyword,
      String categoryCode, Pageable pageable, Boolean isOnlyExternal) throws Exception;

  Page<ProductItemDetailResponse> getProductItemDetailByKeywordAndCategoryCodes(String keyword,
      List<String> categoryCodes, Pageable pageable, Boolean isOnlyExternal) throws Exception;


  Page<ProductItemResponse> findProductItemByKeywordAndCategoryCodes(String keyword, List<String> categoryCodes,
      Pageable pageable, Boolean isOnlyExternal) throws Exception;

  void clearMasterProductCache(String productCode) throws Exception;


  /**
   * clear product data use whenever need cache evict synchronous
   *
   * @param productCode product code of product
   * @param productId   id of product
   */
  void clearMasterProductCacheSync(String productCode, String productId) throws Exception;

  ProductCollectionCountResponse countProductCollectionBySpecifiedDateRange(
      ProductCollectionCountRequest request) throws Exception;

  /**
   * publish domain event to process image to MTA
   * @param productRequest
   * @param message
   * @return
   */
  ProcessImageDomainEvent publishProcessImageRequest(ProductRequest productRequest, String message);

    /**
     * <p>publish productDetails to MTA</p>
     *
     * @param productCode
     * @param message
     * @throws Exception
     */
  ApproveProductResponse publishProductDetailsEvent(String productCode, String message) throws Exception;

  void saveProductHistory(String productCode, ProductHistory productHistory) throws Exception;

  /**
   * delete product by PDT flow to make sure if product is in distribution then needs to delete
   *
   * @param productCode must not blank
   * @param updatedBy must not blank
   * @param notes must not blank
   * @return
   */
  ProductWipDeleteResponse publishProductWipDeleteEvent(String productCode, String updatedBy,
      String notes);

  /**
   * generate productCode
   *
   * @return
   */
  String generateProductCode();


  /**
   * delete product in prd_collection by id list
   *
   * @param ids
   */
  void deleteProductInSolrProductCollectionByIds(List<String> ids) throws Exception;

  /**
   * get the list of product collections with updated flags
   *
   * @param storeId must not blank
   * @param productCollections must not blank
   * @throws Exception
   */
   List<ProductCollectionResponse> getProductCollectionsWithUpdatedFlags(String storeId,
      Page<ProductCollection> productCollections) throws Exception ;

  /**
   * Get active product codes from solr
   *
   * @param storeId
   * @param keyword
   * @param categoryCode
   * @param reviewPending
   * @param sortBy
   * @param pageable
   * @return
   */
  List<String> getActiveProductCodesFromSolr(String storeId, String keyword, String categoryCode, Boolean reviewPending,
      String sortBy, Pageable pageable) throws Exception;

   /**
   * get active product list by productCollection
   *
   * @param storeId
   * @param productCollections
   * @return List of ProductCollectionResponse
   */
  List<ProductCollectionResponse> getProductsByProductCodesInAndActivatedTrueAndViewableTrue(
      String storeId, List<ProductCollection> productCollections);

  /**
   * save product history
   *
   * @param storeId
   * @param productCode
   * @param username
   * @param activity
   * @param notes
   */
  void saveProductHistory(String storeId, String productCode, String username, String activity,
      String notes);

  /**
   *
   * @param storeId
   * @param activated
   * @param viewable
   * @param pageable
   * @return
   */
  Page<ProductCollection> getProductsByStoreIdAndActivatedAndViewable(
      String storeId, boolean activated, boolean viewable, Pageable pageable);

  /**
   * Get in progress products
   *
   * @param storeId
   * @param activated
   * @param reviewPending
   * @param pageable
   * @return
   */
  Page<ProductCollection> getProductsByStoreIdAndActivatedAndReviewPending(
      String storeId, boolean activated, boolean reviewPending, Pageable pageable);

  /**
   *
   * @param storeId
   * @param startDate
   * @param endDate
   * @param pageable
   * @return
   */
  Page<ProductCollection> getProductsByStoreIdAndUpdatedDateBetween(String storeId, Date startDate, Date endDate,
      Pageable pageable);

  /**
   *
   * @param storeId
   * @param productCode
   * @param pageable
   * @return
   */
  Page<ProductCollection> getProductByStoreIdAndProductCode(String storeId, String productCode, Pageable pageable);

  /**
   * Reindex active product by product code and store id
   *
   * @param storeId
   * @param productCode
   */
  void reindexActiveProductCollectionByStoreIdAndProductCode(String storeId, String productCode);

  /**
   *
   * @param storeId
   * @param productCodes
   * @param assignedTo
   * @param assignedBy
   */
  void assignProducts(String storeId, List<String> productCodes, String assignedTo, String assignedBy);

  /**
   * Update Solr for Master Product Update
   *
   * @param storeId
   * @param simpleMasterProductUpdateRequestDTO
   * @param existingProduct
   * @param productCollection
   * @throws Exception
   */
  Map<Boolean, ProductCollection> checkAndUpdateSolr(String storeId,
      SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO, Product existingProduct,
      ProductCollection productCollection) throws Exception;

  /**
   * update product history for attribute autofill
   * @param storeId
   * @param productId
   * @param attributeHistoryResponseList
   */
  void updateHistoryOnAttributeAutoFill(String storeId, String productId,
      List<AttributeHistoryResponse> attributeHistoryResponseList);

  /**
   * Update image paths and flags in PBP and PCB after resizing image
   *
   * @param productAndItemImageRequest
   * @param productCollection
   * @param resize
   * @return
   * @throws Exception
   */
  ProductCollection updateImagePathsAndFlagAfterResizingImage(ProductAndItemImageRequest productAndItemImageRequest,
      ProductCollection productCollection, boolean resize) throws Exception;

  /**
   * Update image paths and flags in PBP and PCB on failure resizing image
   *
   * @param bulkImageProcessResponse
   * @return
   * @throws Exception
   */
  boolean updateImagePathsAndFlagOnResizingImageFailure(BulkImageProcessResponse bulkImageProcessResponse) throws Exception;

  /**
   *
   * @param storeId
   * @param pageable
   * @return
   */
  Page<ProductCollection> getDraftProducts(String storeId, Pageable pageable);

  /**
   *
   * @param productCollection
   * @param postLive
   * @throws Exception
   */
  void updateProductCollectionPostLiveFlag(ProductCollection productCollection, boolean postLive) throws Exception;


  /**
   * update post live status and skip screening
   * @param productCollection
   * @param postLive
   */
  ProductCollection updatePostLiveStatus(ProductCollection productCollection, boolean postLive);

  /**
   *
   * @param storeId
   * @param productCode
   * @throws Exception
   */
  void updateProductAsPostLiveTrue(String storeId, String productCode) throws Exception;


  /**
   * Update solr or publish event
   *
   * @param productCollection
   * @param trustedSeller trustedSeller
   * @throws Exception
   */
  void updateSolrOrPublishEvent(ProductCollection productCollection, boolean trustedSeller) throws Exception;

  /**
   * Fetch list of L1 Product data by search filters(B2B API)
   *
   * @param productFilterRequest
   * @param storeId
   * @param page
   * @param size
   * @return
   */
  Page<ProductFilterResponse> getProductListFilter(String storeId, ProductFilterRequest productFilterRequest, int page,
      int size) throws Exception;


  /**
   * Update buyable and discoverable flag and send mail if margin exceeds
   *
   * @param storeId
   * @param existingCategoryCode
   * @param existingCategoryName
   * @param productCode
   * @param postLive
   * @param username
   */
  void updateBuyableAndDiscoverableFlagAndSendMailOnMarginChange(String storeId, String existingCategoryCode,
      String existingCategoryName, String productCode, boolean postLive, String username) throws Exception;

  /**
   * Check image qc response
   *
   * @param storeId
   * @param productCode
   * @return
   */
  boolean isForceReview(String storeId, String productCode);

  /**
   * Publish product image qc event
   *
   * @param bulkImageProcessResponse
   * @param productDetailResponse
   * @param restrictedKeywordsByFieldAndActionType
   */
  ImageQcRequestDomainEvent publishImageQcEvent(BulkImageProcessResponse bulkImageProcessResponse,
      ProductDetailResponse productDetailResponse,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType);

  /**
   * Publish product image qc event
   *
   * @param productCode
   * @param images
   * @param productDetailResponse
   * @param restrictedKeywordsByFieldAndActionType
   * @param businessPartnerCode
   */
  ImageQcRequestDomainEvent publishImageQcEventForEditedImages(String productCode, List<Image> images,
      ProductDetailResponse productDetailResponse,boolean isRevised,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType, String businessPartnerCode);

  /**
   * Publish product action retry event to PDT
   *
   * @param productActionRetryEvent
   */
  void publishProductActionRetryEvent(ProductActionRetryEvent productActionRetryEvent);

  /**
   * Publish processed image qc response to PDT
   *
   * @param storeId
   * @param productCode
   * @param productCollection
   * @param productDetailResponse
   * @param autoNeedRevisionAndForceReviewResponse
   * @param profileResponse
   * @param imageQcResponseDomainEvent
   * @return
   */
  ImageQcProcessedResponseDomainEvent publishImageQcProcessedResponseEvent(String storeId, String productCode,
      ProductCollection productCollection, ProductDetailResponse productDetailResponse,
      AutoNeedRevisionAndForceReviewResponse autoNeedRevisionAndForceReviewResponse,
    ProfileResponse profileResponse, ImageQcResponseDomainEvent imageQcResponseDomainEvent)
    throws Exception;

  /**
   * Publish auto need revision event
   *
   * @param storeId
   * @param productCode
   * @param predictionTypeList
   * @param contentNeedRevision
   * @param notes
   */
  void publishAutoNeedRevisionEvent(String storeId, String productCode, Set<String> predictionTypeList,
      boolean contentNeedRevision, String notes);

  /**
   * Publish add edited product to pdt event
   *
   * @param storeId
   * @param reviewTypes
   * @param productCollection
   * @return
   */
  AddEditedProductToPDTEvent publishAddEditedProductToPDTEvent(String storeId, String reviewTypes,
      ProductCollection productCollection, List<String> allModifiedFields) throws Exception;

  /**
   *
   * @param dalamProductListRequest
   * @return
   * @throws Exception
   */
  Page<ProductCollection> findProductsForDalamProcess(DalamProductListRequest dalamProductListRequest) throws Exception;

  /**
   * Get the dalam process products in between dates
   *
   * @param dalamProductListRequest
   * @return
   */
  Page<ProductCollection> findProductsForDalamProcessInBetweenAge(DalamProductListRequest dalamProductListRequest) throws Exception;

  /**
   * Get the dalam process products updated less than a date
   *
   * @param dalamProductListRequest
   * @return
   */
  Page<ProductCollection> findProductsForDalamProcessAgeLessThen(DalamProductListRequest dalamProductListRequest) throws Exception;

  /**
   * Check if image qc status is sync process
   *
   * @param storeId
   * @param productCode
   * @param parentCategoryCode
   * @return
   */
  ImageQcEnableAndSyncResponse getImageQcStatus(String storeId, String productCode, String parentCategoryCode);

  /**
   * Process image qc response
   *
   * @param storeId
   * @param imageQcResponseDomainEvent
   * @param imageCountMap
   * @param productCollection
   * @param productDetailResponse
   * @param trustedSeller True for Trusted Sellers
   * @param profileResponse
   */
  AutoNeedRevisionAndForceReviewResponse processImageQcResponse(String storeId, ImageQcResponseDomainEvent imageQcResponseDomainEvent,
      Map<String, Long> imageCountMap, ProductCollection productCollection, ProductDetailResponse productDetailResponse,
    boolean trustedSeller, ProfileResponse profileResponse) throws Exception;

  /**
   * Set review pending flag to false
   * @param storeId
   * @param productCode
   * @return
   */
  void setReviewPendingFlagToTrue(String storeId, String productCode) throws Exception;

  /**
   * Publish ProductStatusEvent change event on creation, activation, rejection, need for correction
   *
   * @param productDetailResponse
   * @param productCollection
   * @param productStatus
   * @param reason
   * @throws Exception
   */
  void publishProductStatusEvent(ProductDetailResponse productDetailResponse, ProductCollection productCollection,
      ProductStatus productStatus, String reason) throws Exception;

  /**
   * Publish ProductStatusEvent change event on creation, activation, rejection, need for correction
   *
   * @param productCode
   * @param productStatus
   * @param reason
   * @throws Exception
   */
  void publishProductStatusEventByProductCode(String productCode, ProductStatus productStatus, String reason)
      throws Exception;

  /**
   *
   * @param productDetailResponse
   * @param productId
   */
  void checkIfProductWasTakeDown(ProductDetailResponse productDetailResponse, String productId);


  /**
   * API to fetch the minimum price from the configuration.
   *
   */
  Integer getMinimumPrice(String storeId);

  /**
   * Check if seller is eligible to create MPP products
   *
   * @param storeId
   * @param businessPartnerCode
   * @param profileResponse
   * @return
   */
  boolean checkIfMPPIsAllowed(String storeId, String businessPartnerCode, ProfileResponse profileResponse) throws Exception;

  /**
   * Check if MPP is allowed
   *
   * @param profileResponse
   * @return
   * @throws Exception
   */
  boolean checkIfMPPIsAllowed(ProfileResponse profileResponse) throws Exception;

  /**
   * Save history for URL images
   *
   * @param productCode
   * @param itemFlagDetails
   */
  void saveHistoryForUrlImage(String productCode, List<ItemFlagDetails> itemFlagDetails,
      ProductCreationRequest productCreationRequest) throws Exception;

  /**
   * Save history for product migration activity
   *
   * @param productMigration
   */
  void saveMasterProductMigrationHistory(ProductMigration productMigration);

  /**
   * Save audit history for item skus
   *
   * @param productMigration
   */
  void saveSyncHistoryForItems(ProductMigration productMigration);

  /**
   * Update the product detail for new product created during migration
   *
   * @param productMigration
   * @param productAndItemsResponse
   */
  void updateProductCollectionForMigratedProduct(ProductMigration productMigration, ProductAndItemsResponse productAndItemsResponse) throws Exception;

  /**
   * Update image paths and flags in PBP and PCB after resizing edited image
   *
   * @param productAndItemImageRequest
   * @param productCollection
   * @param resize
   * @param setDgLevel
   * @return
   * @throws Exception
   */
  void updateEditedImagePathsAndFlagAfterResizingImage(ProductAndItemImageRequest productAndItemImageRequest,
      ProductCollection productCollection, boolean resize, boolean setDgLevel) throws Exception;

  /**
   * Get image qc processed response
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ProductImageQcProcessingResponse getProductImageQcProcessingResponse(String storeId, String productCode);

  /**
   * Get image qc data from pdt
   * @param storeId
   * @param productCode
   * @return
   * @throws IOException
   */
  ProductImageQcFeedbackResponse getImageQcResponseFromPDT(String storeId, String productCode)
      throws IOException;

  /**
   * Get product details from PDT
   *
   * @param productCode
   * @return
   */
  PDTProductDomainEventModel getPDTDomainModelResponseByCode(String productCode) throws Exception;

  /**
   * Update image qc response for the edited images
   *
   * @param storeId
   * @param imageQcResponseDomainEvent
   * @param productImageQcProcessingResponse
   * @param productCollection
   * @param imageCountMap
   * @param imageQcResponseFromPDT
   * @param pdtDomainModelResponseByCode
   * @param productDetailResponse
   * @param profileResponse
   * @return
   */
  AutoNeedRevisionAndForceReviewResponse updateImageQcResponse(String storeId, ImageQcResponseDomainEvent imageQcResponseDomainEvent,
      ProductImageQcProcessingResponse productImageQcProcessingResponse, ProductCollection productCollection,
      Map<String, Long> imageCountMap, ProductImageQcFeedbackResponse imageQcResponseFromPDT,
      PDTProductDomainEventModel pdtDomainModelResponseByCode, ProductDetailResponse productDetailResponse,
    ProfileResponse profileResponse) throws Exception;

  /**
   * Update review types
   *
   * @param storeId
   * @param productCode
   * @param reviewType
   * @throws Exception
   */
  ProductCollection updateReviewType(String storeId, String productCode, String reviewType);

  /**
   * Check if product activation is needed
   *
   * @param productId
   * @return
   */
  String isProductActivationNeeded(String storeId, String productId) throws Exception;

  /**
   * Take down or activate product by productCode
   * @param storeId
   * @param productCode
   * @param takeDown
   */
  void takeDownOrActivateProductByProductCode(String storeId, String productCode, boolean takeDown) throws Exception;

  /**
   * Publish edited product
   *
   * @param storeId
   * @param productCode
   * @param reviewType
   */
  void publishEditedProduct(String storeId, String productCode, String reviewType) throws Exception;

  /**
   * Update take down flag after vendor approval
   *
   * @param storeId
   * @param productCode
   */
  void updateImageQcDataAfterVendorApproval(String storeId, String productCode) throws IOException;


  /**
   * Update image in PCB
   *
   * @param imageResponses
   * @param productCode
   */
  ActivateImageResponse updateActiveImagesAndGetActivateImageResponse(String productCode,
      List<ScaleImageResponse> imageResponses) throws Exception;

  /**
   * Fetch auto approval enum by autoApprovalTypeRequest
   *
   * @param storeId
   * @param username
   * @param productCode
   * @param autoApprovalTypeRequest
   * @param profileResponse
   * @param productCollection
   * @return
   */
  AutoApprovalType findAutoApprovalTypeByRequest(String storeId, String username, String productCode,
    AutoApprovalTypeRequest autoApprovalTypeRequest, ProfileResponse profileResponse,
      ProductCollection productCollection) throws Exception;


  /**
   * Save product collection
   *
   * @param productCollection
   * @return
   */
  ProductCollection saveProductCollection(ProductCollection productCollection);

  /**
   * Remove product from PDT
   *
   * @param productCode
   */
  void removeProductFromPDT(String productCode) throws Exception;

  /**
   * Delete product from review product collection
   *
   * @param productIds
   */
  void deleteFromReviewProductCollection(List<String> productIds);

  /**
   * Check if product exists in PDT
   *
   * @param productCode
   * @param allProducts
   * @return
   */
  boolean checkIfProductExistsInPDT(String productCode, boolean allProducts);

  /**
   * Add product to review product collection
   *
   * @param productCollection
   */
  void addProductToReviewCollection(ProductCollection productCollection) throws Exception;


  /**
   * update auto approval type
   *
   * @param autoApprovalType
   * @param productCode
   */
  void updateAutoApprovalTypeByProductCode(AutoApprovalType autoApprovalType, String productCode);

  /**
   * Find in progress products by merchant code
   *
   * @param storeId
   * @param merchantCode
   * @param page
   * @param size
   * @return
   */
  InProgressProductResponsePageResponse findInProgressProductsByMerchantCode(String storeId, String merchantCode,
    int page, int size);

  /**
   * Get product collection by product code
   *
   * @param productCode
   * @param storeId
   * @return
   */
  String getProductStatus(String storeId, String productCode);

  /**
   * publish dimension refresh event for in review product
   * @param storeId
   * @param productCode
   * @param dimensionRefreshRequest
   */
  void publishDimensionRefreshEventForReviewPendingProducts(String storeId, String productCode, DimensionRefreshRequest dimensionRefreshRequest);

  /**
   * Fetch cogs value by material code
   *
   * @param storeId
   * @param materialCode
   * @return
   */
  CogsValueResponse fetchCogsValueByMaterialCode(String storeId, String materialCode);

  /**
   * Process Image qc for backlog products
   *  @param storeId
   * @param imageQcResponseDomainEvent
   */
  String processImageQcForBacklogProducts(String storeId, ImageQcResponseDomainEvent imageQcResponseDomainEvent);

  /**
   * Update Product workflow by state
   *
   * @param storeId
   * @param productCode
   * @param state
   */
  void updatePbpProductWorkflowState(String storeId, String productCode, String state);

  /**
   * Publish image qc backlog request event
   *
   * @param imageQcResponseDomainEvent
   */
  void publishImageQcBacklogRequestEvent(ImageQcResponseDomainEvent imageQcResponseDomainEvent);


  /**
   * Update L5 data
   *
   * @param storeId
   * @param productLevel3
   * @param productVariantUpdateRequest
   * @param editResponse
   * @param productDetailEdit  Is edit action requested by PDP web
   * @return
   */
  ItemsPriceStockImagesUpdateResponse editPriceStockVariantsInfo(String storeId, ProductLevel3 productLevel3,
      ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse, boolean productDetailEdit) throws Exception;

  /**
   * Get profile response by businessPartnerCode
   *
   * @param businessPartnerCode
   * @return
   */
  ProfileResponse getProfileResponse(String businessPartnerCode) throws Exception;

  /**
   * Save product collection
   *
   * @param productCollection
   * @return
   */
  ProductCollection saveProductCollectionForUPC(ProductCollection productCollection) throws Exception;

  /**
   * Validate pick up point
   *
   * @param requestId
   * @param request
   * @param profileResponse
   * @param existingSellerSkusAndProductDetailsMap
   */
  void validatePickupPointsAndFbb(String requestId, ProductCreationRequest request, ProfileResponse profileResponse,
      Map<String, OmniChannelSkuResponse> existingSellerSkusAndProductDetailsMap) throws Exception;

  /**
   * get ProductCollection by productSku
   * @param productSku
   * @return
   */
  ProductCollection getProductCollectionByProductSku(String productSku);

  /**
   * @param storeId
   * @param startUpdatedDate
   * @param endUpdatedDate
   * @return
   */
  List<ProductCollection> fetchProductsToSync(String storeId, Date startUpdatedDate,
    Date endUpdatedDate);

  /**
   * @param storeId
   * @param startUpdatedDate
   * @param endUpdatedDate
   * @param pageable
   * @return
   */
  Page<ProductCollection> fetchActiveProductsToSync(String storeId, Date startUpdatedDate, Date endUpdatedDate,
      Pageable pageable);

  /**
   * Fetch pre-live products to sync
   *
   * @param storeId
   * @param state
   * @param postLive
   * @param edited
   * @param resubmitCount
   * @param startUpdatedDate
   * @param endUpdatedDate
   * @param pageable
   * @return
   */
  Page<ProductCollection> fetchPreLiveProductsToSync(String storeId, String state, boolean postLive, boolean edited,
      int resubmitCount, Date startUpdatedDate, Date endUpdatedDate, Pageable pageable);


  /**
   * retry stuck products
   * @param productWfStateResponse
   * @return
   */
  StuckProductEventPublishDto getPDTEventDomainModel(ProductWfStateResponse productWfStateResponse);

  /**
   * check seller eligibility for a given product type
   * @param productType
   * @param profileResponse
   * @return
   */
  ApiErrorCode checkBpBopisEligibility(Integer productType, ProfileResponse profileResponse, CategoryDetailResponse categoryDetailResponse, ProductL3UpdateRequest productL3UpdateRequest, boolean isPureExternalUser);

  /**
   * check seller eligibility for a given product type
   * @param productType
   * @param profileResponse
   * @return
   */
  ApiErrorCode checkBpBopisEligibility(Integer productType, ProfileResponse profileResponse);
  /**
   * Check Product Limit Exceeded (if applicable)
   * @param businessPartnerCode
   * @param storeId
   * @return
   * @throws Exception
   */
  /**
   * Validate that the selected category may be used when productType == BOPIS.
   *
   * @param businessPartnerCode
   * @param storeId
   * @return ApiErrorCode if invalid, otherwise null
   */
  ApiErrorCode checkProductLimitExceeded(String businessPartnerCode, String storeId) throws Exception;

  /**
   * Validate sizeChartCode in Creation n Edit
   * @param sizeChartCode
   * @param storeId
   * @return
   */
  ApiErrorCode validateSizeChart(String sizeChartCode, String storeId);

  /**
   * Validate if we need to skip definitive action based on sku order
   * @param storeId store id
   * @param productCollection product collection
   * @param restrictedKeywordsWithActionTypeInProductDetails restricted keyword with action
   */
  void validateSkipDefinitiveAction(String storeId, ProductCollection productCollection,
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails);
  /**
   * Check active order data to skip restricted keyword action
   *
   * @param productMasterDataEditRequest product master data edit request
   * @param restrictedKeywordsWithActionTypeInProductDetails restricted keywords with action type in product details
   */

  void checkActiveOrderDataToSkipRestrictedKeywordAction(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails);

  /**
   * Check OH for a product
   * @param productCode
   * @param productSku
   * @return
   */
  AgpSimpleQueryResponse getQueryResponseForActiveOrderData(String productCode,
      String productSku);

  /**
   * override action to Manual_Review
   * @param productCode
   * @param restrictedKeywordsWithActionTypeInProductDetails
   */
  void overRideRestrictedKeywordActionOnActiveOrders(String productCode,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsWithActionTypeInProductDetails);

  /**
   * delete entries from product collection repo by storeId and productCode
   * @param storeId
   * @param productCode
   */
  void deleteProductCollectionByStoreIdAndProductCode(String storeId, String productCode);

  /**
   * find Products By AddDeleteVariant By PendingStatus
   *   @param storeId
   * @param productCode
   * @param requestId
   * @param userName
   * @return
   */
  List<String> findProductsByAddDeleteVariantByPendingStatus(String storeId, String productCode, String requestId,
      String userName) throws Exception;

  /**
   * Update products from product collection with add delete variant status
   * @param addDeleteVariantStatus PENDING/SUCCESS
   * @param productCode non null string
   */
  void updateAddDeleteVariantStatus(String productCode, AddDeleteVariantStatus addDeleteVariantStatus);

  /**
   * update add delete variant status based on status
   * @param productCode
   * @param addDeleteVariantStatus
   * @return
   */
  ProductCollection updateAddDeleteVariantStatusForListener(String productCode,
      AddDeleteVariantStatus addDeleteVariantStatus);

  /**
   * update product Item Business Partner for Migration
   * @param storeId not null
   * @param productBusinessPartnerId not null
   * @param productAndL5MigrationRequest request
   * @param username updated by
   * @param b2bActivated boolean
   * @param b2cActivated boolean
   */
  void migrateProductAndL5DetailsByProductSku(String storeId, String productBusinessPartnerId,
    ProductAndL5MigrationRequest productAndL5MigrationRequest, String username,
    boolean b2bActivated, boolean b2cActivated) throws Exception;

  /**
   * take Actions On Category Change From Vendor
   *
   * @param storeId                      not null
   * @param existingCategoryCode         not empty
   * @param existingCategoryName         not empty
   * @param productCode                  not empty
   * @param eligibleForShippingMigration boolean
   * @param marginExceeded               boolean
   */
  void takeActionsOnCategoryChangeFromVendor(String storeId, String existingCategoryCode,
    String existingCategoryName, String productCode, boolean eligibleForShippingMigration,
    boolean marginExceeded) throws Exception;

  /**
   * get size chart basic detail by size chart code
   *
   * @param sizeChartCodes not null
   * @return BasicSizeChartDetailMapResponse
   */

  BasicSizeChartDetailMapResponse getSizeChartBasicDetailBySizeChartCode(List<String> sizeChartCodes);

  /**
   * API to migrate Product And L5 Details By ProductSku
   *
   * @param storeId not null
   * @param productAndL5MigrationRequest  migration request not null
   * @param username updated by (nullable)
   */
  void migrateProductAndL5Details(String storeId, ProductAndL5MigrationRequest productAndL5MigrationRequest, String username)
    throws Exception;


  /**
   * API to fetch distinct business aprtner code for need revision products
   *
   * @param storeId not null
   * @param needRevisionProductsRequest  migration request not null
   */
  Page<String> getDistinctBusinessPartnerCodesForNeedRevisionProduct(String storeId,
      NeedRevisionProductsRequest needRevisionProductsRequest, Pageable pageable);

  /**
   * fetch all products by business partner for last x number of days
   *
   * @param storeId
   * @param businessPartnerCode
   * @return
   */
  Page<ProductCodeAndNameDetails> fetchProductsByBusinessPartnerCodeForLastXDays(String storeId,
      String businessPartnerCode, Pageable pageable);

  /**
   * populate prd product business partner attribute
   *
   * @param productCode
   * @param attributeCode
   * @param attributeId
   * @param attributeValue
   * @param skuValue
   * @param attributeName
   */
  void populateProductBusinessPartnerAttribute(String productCode, String attributeCode, String attributeId,
      String attributeValue, boolean skuValue, String attributeName);

  /**
   * update status in pcb for back fill attributes
   *
   * @param productCode
   * @param updatedStatus
   * @param errorMessage
   */
  void updateStatusInPCBForBackFillAttributes(String productCode, String updatedStatus, String errorMessage);

  /**
   * Verify auto approval rules
   *
   * @param autoApprovalsDetailDto
   * @return
   * @throws Exception
   */
  AutoApprovalType verifyAutoApprovalRules(AutoApprovalsDetailDto autoApprovalsDetailDto) throws Exception;
  /**
   * Perform master data update
   *
   * @param productMasterDataEditRequest master data edit request
   * @param masterProductEditDTO master product edit DTO
   * @param storeId store id
   * @param requestId request id
   * @param username username
   */
  ApiErrorCode performMasterDataUpdate(ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO, String storeId, String requestId, String username)
    throws Exception;
/**
   * Publish edited event to vendor and publish dimensions refresh event
   *
   * @param productMasterDataEditRequest master data edit request
   * @param masterProductEditDTO master product edit DTO
   * @param storeId store id
   */
  void publishEditedEventToVendorAndPublishDimensionsRefreshEvent(
    ProductMasterDataEditRequest productMasterDataEditRequest,
    MasterProductEditDTO masterProductEditDTO, String storeId) throws Exception;

  /**
   * Get all prodcuts mapped to a particular brand-name
   *
   * @param storeId
   * @param brandName
   * @param pageable
   * @return
   */
  Page<ProductAndBrandResponse> getProductsByBrandName(String storeId, String brandName,
      Pageable pageable) throws Exception;

  /**
   * Update brand data for a product
   *
   * @param storeId
   * @param productBrandUpdateRequest
   * @param productBusinessPartners
   * @return
   */
  Pair<ProductCollection, String> updateBrandData(String storeId,
      ProductBrandUpdateRequest productBrandUpdateRequest,
      List<ProductBusinessPartner> productBusinessPartners);
}
