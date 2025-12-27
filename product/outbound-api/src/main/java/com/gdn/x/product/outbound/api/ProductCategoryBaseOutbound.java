package com.gdn.x.product.outbound.api;

import java.util.List;
import java.util.Map;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;

public interface ProductCategoryBaseOutbound {

  CategoryDetailResponse getCategoryDetailByCategoryCode(String requestId, String username,
      String categoryCode);

  CategoryDetailResponse getCategoryDetail(String requestId, String username, String categoryId);

  List<CategoryResponse> getCategoryHierarchy(String requestId, String username, String categoryCode);

  /**
   * Get product detail by products from active products
   * @param requestId
   * @param username
   * @param productCode
   * @return
   */
  ProductDetailResponse getProductDetailByProductCode(String requestId, String username,
      String productCode) throws Exception;

  /**
   * Get product detail by products from all products
   * @param requestId
   * @param username
   * @param productCode
   * @param inAllProducts
   * @return
   */
  ProductDetailResponse getProductDetailByProductCodeForAllProducts(String requestId, String username,
      String productCode, boolean inAllProducts) throws Exception;

  ProductItemDetailResponse getProductItemDetailByItemCode(String requestId, String username,
      String itemCode);

  /**
   * Get Parent category response for Master Catalog by productCode
   * @param requestId
   * @param username
   * @param productCode
   * @return
   */
  List<CategoryResponse> getMasterParentCategoryResponseByProductCode(String requestId,
      String username, String productCode);

  /**
   * Get sales category mappings for product code
   *
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param productCode
   * @return
   */
  GdnRestSingleResponse<ProductSalesCategoryMappingResponse> getSalesCategoryMappingByProductCode(String storeId, String requestId,
      String username, String productCode, boolean ignoreHalalCategories);

  /**
   * Generate shipping weight
   *
   * @param storeId
   * @param categoryCode
   * @param length
   * @param width
   * @param height
   * @param weight
   * @return
   */
  double generateShippingWeight(String storeId, String categoryCode, double length, double width, double height, double weight);

  /**
   * Fetching category names by category codes
   *
   * @param categoryCodes
   * @return
   */
  CategoryNamesResponse getCategoryNames(List<String> categoryCodes);

  /**
   * Fetching Cn category codes by C1 category codes
   *
   * @param categoryCodes
   * @return
   */
  List<String> getCnCategoryCodes(List<String> categoryCodes);

  /**
   * Fetch master data for transaction API used by X-Cart
   *
   * @param itemCode
   * @return
   * @throws ApplicationException
   */
  ProductMasterDataResponse getMasterDataForTransaction(String itemCode) throws ApplicationException;

  List<ProductItemDetailResponse> findProductItemDetailsBySkuCodes(List<String> skuCodes,
    boolean fetchArchived, boolean originalImages) throws Exception;

  /**
   * get product images by product code
   * @param productCode
   * @return
   * @throws Exception
   */
  List<ImageResponse> getProductImagesByProductCode(String productCode) throws Exception;

  /**
   * get item images by item code
   *
   * @param skuCodesRequest@return
   * @throws Exception
   */
  GdnRestListResponse<ItemImageResponse> getProductItemImagesByItemCode(SkuCodesRequest skuCodesRequest) throws Exception;

  /**
   *
   * @param attributeCode
   * @return
   */
  AttributeResponse getAttributeDetailByAttributeCode(String attributeCode) throws Exception;

  /**
   * get basic product details
   * @param productCode
   * @return
   */
  ProductResponse getProductBasicDetails(String productCode);

  /**
   * get basic product and attribute details
   *
   * @param productCode
   * @param inAllProducts
   * @return
   */
  ProductAndAttributeDetailResponse getProductAndAttributeDetails(String productCode, boolean inAllProducts);

  /**
   * Get brand url from brandCode
   *
   * @param brandCode
   * @return
   */
  String getBrandLogoUrl(String brandCode);

  /**
   * fetching size chart details
   *
   * @param storeId
   * @param sizeChartCode String
   * @return sizeChartResponse
   */
  SizeChartResponse fetchSizeChartDetails(String storeId, String sizeChartCode) throws ApplicationRuntimeException;

  /**
   * Check if size chart code is valid or not
   * @param sizeChartCode
   * @return
   */
  boolean isSizeChartCodeValid(String sizeChartCode);

  Map<String, Boolean> getCategoryAndEligibleFlagMap(String storeId, String sizeChartAttributeCode,
      List<String> categoryCodes);

  List<String> getCategoryCodesByAttributeCode(String storeId, String attributeCode);


  /**
   * update status in pcb for back fill attributes
   *
   * @param productCode
   * @param updatedStatus
   * @param errorMessage
   */
  void updateStatusInPCBForBackFillAttributes(String productCode, String updatedStatus, String errorMessage);

  /**
   * Get product basic info details by product codes
   *
   * @param productCodes
   */
  List<BasicInfoProductResponse> getBasicInfoProductDetailsListByProductCodes(List<String> productCodes);

  /**
   * Delegate to PCB to validate omni-channel SKU list under a seller.
   */
  ValidOmniChannelSkuResponse checkOmniChannelSkuExistsInSeller(String storeId, String requestId,
      String username, boolean needUomInfo, OmniChannelSkuRequest request) throws ApplicationException;
}
