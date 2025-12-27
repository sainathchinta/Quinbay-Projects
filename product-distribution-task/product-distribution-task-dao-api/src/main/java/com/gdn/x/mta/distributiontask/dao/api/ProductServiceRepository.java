package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gdn.x.mta.distributiontask.model.dto.CategoryDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductSkuDetailResponse;
import com.gdn.x.mta.distributiontask.model.dto.SellerAnalyticsResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;

import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;

/**
 * Created by virajjasani on 25/09/16.
 */
public interface ProductServiceRepository {

  /**
   * delete product collection from Product Service
   *
   * @param requestId request Id, must not blank
   * @param userName must not blank
   * @param needEmailNotification false if not need to notify to merchant on delete
   * @param rejectProductDTO must not null, holds product name, code, state,note
   * @throws Exception
   */
  void deleteProductCollection(String requestId, String userName, boolean needEmailNotification,
      RejectProductDTO rejectProductDTO) throws Exception;

  /**
   * get product details by product code list
   *
   * @param requestId
   * @param username
   * @param productCodes
   * @return
   * @throws Exception
   */
  List<ProductDetailResponse> findProductDetailsByProductCodes(String requestId,
      String username, List<String> productCodes) throws Exception;


  /**
   * Republish from PBP to PDT
   * 
   * @param requestId
   * @param userName
   * @param productCode
   * @throws Exception
   */
  void republishToPDT(String requestId, String userName, String productCode) throws Exception;

  /**
   * @param requestId
   * @param userName
   * @param taskHistory
   * @throws Exception
   */
  void submitHistory(String requestId, String userName, TaskHistory taskHistory)
      throws Exception;

  /**
   *
   * @param requestId
   * @param userName
   * @param productHistoryRequest
   * @throws Exception
   */
  void addToProductHistory(String requestId, String userName, ProductHistoryRequest productHistoryRequest)
      throws Exception;

  /**
   *
   * @param fromDateInMillis
   * @return
   */
  List<ConfigurationStatusResponse> getReviewConfigurationChanges(long fromDateInMillis);

  /**
   *
   * @param configurationStatusRequests
   * @return
   */
  List<ConfigurationStatusResponse> getReviewConfiguration(List<ConfigurationStatusRequest> configurationStatusRequests);

  /**
   *
   * @param storeId
   * @param productCode
   */
  void updateProductCollectionAsPostLive(String storeId, String productCode);

  /**
   * Get image qc response
   *
   * @param productCode
   * @return
   */
  ImageQcProcessedAndBrandResponse getImageQcPredictionResponse(String productCode);

  /**
   * Fetch category hierarchy from PCB for given category codes
   *
   * @param categoryCodeRequest
   * @return
   */
  List<CategoryHierarchyResponse> getCategoryHierarchyByCategoryCodes(
      CategoryCodeRequest categoryCodeRequest);

  /**
   * @param storeId
   * @param productCode
   * @param onlyCategoryChange
   * @param autoApprovalTypeRequest
   * @return
   */
  GdnRestSingleResponse<AutoApprovalTypeResponse> getAutoApprovalType(String storeId,
      String productCode, boolean onlyCategoryChange, AutoApprovalTypeRequest autoApprovalTypeRequest);

  /**
   *
   * @param storeId
   * @param productCode
   * @return
   */
  String getProductStatus(String storeId, String productCode);

  /**
   *
   * @param storeId
   * @param retryNeedRevisionRequest
   * @return
   */
  RetryAutoNeedRevisionResponse retryAutoNeedRevision(String storeId, RetryNeedRevisionRequest retryNeedRevisionRequest);

  /**
   *  Get category details by categoryCode
   * @param categoryCode
   * @return
   */
  CategoryDetailResponse getCategoryDetailByCategoryCode(String categoryCode);

  /**
   *
   * @param autoQcConfigChangeRequest
   * @return
   */
  boolean validateAutoQcConfigChange(AutoQcConfigChangeRequest autoQcConfigChangeRequest);

  /**
   *
   * @param categoryCodeRequest
   * @return
   */
  CategoryCodeResponse getCnCategoryCodesFromC1(CategoryCodeRequest categoryCodeRequest);


  /**
   * Validate protected brand
   *
   * @param brandCode
   * @param sellerCode
   * @return
   */
  boolean validateProtectedBrand(String brandCode, String sellerCode);

  /**
   * Filter by brand name
   * @param brandName
   * @return
   */
  BrandResponse filterByBrandName(String brandName);

  /**
   *  Get Basic details of product by productCode
   * @param productCode
   * @return
   */
  ProductResponse getProductBasicDetailByProductCode(String productCode);

  /**
   *  Clear cache in PCB for products by productId and productCode
   * @param id
   * @param productCode
   * @return
   */
  void clearProductCacheSyncByProductIdAndProductCode(String id, String productCode);


  /**
   * @param attributeCode attributeCode
   * @param value brand code
   * @param fetchByPredefinedAttributeCode
   * @throws Exception
   */
  PredefinedAllowedAttributeValueResponse getPredefinedAllowedAttributeValueByAttributeCodeAndValue(
    String attributeCode, String value, boolean fetchByPredefinedAttributeCode);


  /**
   * @param productCode not null
   * @param inAllProducts boolean
   * @param originalImages boolean
   * @return ProductDetailResponse productDetailResponse
   */

  ProductDetailResponse getProductDetailByProductCode(String productCode, boolean inAllProducts,
    boolean originalImages);

  /**
   * @param storeId
   * @param productCode
   */
  void processProductVendorSearchAutoHeal(String storeId, String productCode);

  /**
   * @param productCode
   */
  List<PrdProductResponse> getProductBasicDetailByProductCodeFromXProduct(String productCode);

  /**
   * @param productSku String
   * @param productCode String
   * @return ProductSkuDetailResponse
   */
  ProductSkuDetailResponse getProductDetailForProduct(String productSku, String productCode);

  /**
   * Fetching the sellerAnalyticsResponse from Product Analytics
   *
   * @param sellerCode sellerCode
   * @return SellerAnalyticsResponse
   */
  SellerAnalyticsResponse getSellerAnalyticsResponse(String sellerCode);

  /**
   * to suspend ipr product
   *
   * @param suspensionProductRequest SuspensionProductRequest
   */
  void suspendIprProduct(SuspensionProductRequest suspensionProductRequest);

  /**
   * Fetch parent category code from Cn category
   * @param categoryCode
   * * @return C1 Category code
   */
  CategoryDTO fetchParentCategoryFromCnCategoryCode(String categoryCode);
}
