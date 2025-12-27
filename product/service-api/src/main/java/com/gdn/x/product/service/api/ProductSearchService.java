package com.gdn.x.product.service.api;

import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Set;

import org.apache.solr.client.solrj.SolrServerException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.HalalDashboardProductsResponseVo;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.PristineProductAndItemsResponseVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCountResponseVo;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductSummaryResponseV2Vo;
import com.gdn.x.product.model.vo.ProductsToItemCatalogMapping;
import com.gdn.x.product.model.vo.ReviewProductDetailVO;
import com.gdn.x.product.model.vo.SimplePristineProductRequestVo;
import com.gdn.x.product.model.vo.SimpleProductRequestVo;
import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.common.web.param.MandatoryRequestParam;

public interface ProductSearchService {

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param needCategoryHierarchies
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getAllProductAndItemsSortByProductCodeAsc(
      String storeId, String username, String requestId, boolean needCategoryHierarchies,
      Pageable page) throws Exception;

  List<ProductAndItemSolr> getItemSkuAndCodeByStoreIdAndItemSkus(String storeId,
      Set<String> itemSku) throws Exception;

  MasterDataDetailWithProductAndItemsResponseVo getListOfProductsBySimpleProductRequests(
      String storeId, String username, String requestId,
      List<SimpleProductRequestVo> simpleProductRequests) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param productCatentryId
   * @return
   */
  ProductAndItemsVO getProductAndItemsByProductCatentryId(String storeId, String username,
      String requestId, String productCatentryId) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param productCatentryId
   * @param needCategoryHierarchies
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByProductCatentryIds(
      String storeId, String username, String requestId, Set<String> productCatentryId,
      boolean needCategoryHierarchies) throws Exception;

  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByProductCode(String storeId,
      String username, String requestId, String productCode, boolean needCategoryHierarchies)
      throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param productCode
   * @param needCategoryHierarchies
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByProductCodes(String storeId,
      String username, String requestId, Set<String> productCode, boolean needCategoryHierarchies)
      throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param productSku
   * @param needCategoryHierarchies
   * @return
   * @throws Exception
   */
  MasterDataWithProductItemsVo getProductAndItemsByProductSkus(String storeId,
      String username, String requestId, Set<String> productSku, boolean needCategoryHierarchies)
      throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param productCodes
   * @param needCategoryHierarchies
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsSyncByProductCodes(
      String storeId, String username, String requestId, String productCodes, boolean needCategoryHierarchies,
      boolean combineOthersBundlings, boolean off2On) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param productCode
   * @param itemSku
   * @param needCategoryHierarchies
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemResponseVo getMasterDataAndProductAndItemData(
      String storeId, String username, String requestId, String productCode, String itemSku,
      boolean needCategoryHierarchies, boolean combineOthersBundlings, boolean off2On) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param month
   * @param year
   * @param page
   * @return
   */
  Page<ProductsToItemCatalogMapping> getProductAndItemWithoutSalesCatalog(String storeId,
      String username, String requestId, int month, int year, Pageable page) throws Exception;

  Page<ProductsToItemCatalogMapping> getProductAndItemWithoutSalesCatalogPerDay(String storeId,
      String username, String requestId, int fromDay, int toDay, int month, int year,
      Pageable page);

  List<ProductAndItemSolr> getProductSkuAndNameByPickupPointCode(String storeId,
      String pickupPointCode);

  /**
   * Whether pickupPointCode is used or not
   *
   * @param storeId
   * @param pickupPointCode
   * @return
   */
  boolean isPickupPointCodeUsed(String storeId, String pickupPointCode);

  /**
   * To get product and items from X-product
   * @param storeId
   * @param username
   * @param requestId
   * @param pristineIds
   * @return MasterDataDetailWithProductAndItemsResponseVo X-product data response
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByPristineIds(String storeId,
      String requestId, String username, Set<String> pristineIds) throws Exception;

  /**
   * To get product and items from X-product
   * @param storeId
   * @param username
   * @param requestId
   * @param productIds
   * @param productSkus
   * @return MasterDataDetailWithProductAndItemsResponseVo X-product data response
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsByProductCodesAndProductSkus(
      String storeId, String username, String requestId, Set<String> productIds,
      Set<String> productSkus) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param needCategoryHierarchies
   * @param pristineShortId
   * @param defaultItemSku
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductAndItemsSyncByPristineId(String storeId,
      String username, String requestId, String pristineShortId, boolean needCategoryHierarchies,
      String defaultItemSku) throws Exception;

  /**
   * Get product and items response by pristine Id
   * @param storeId
   * @param username
   * @param requestId
   * @param pristineShortId
   * @param off2On
   * @return
   */
  PristineProductAndItemsResponseVO getProductAndItemsResponseByPristineId(String storeId, String username,
      String requestId, String pristineShortId, boolean off2On) throws Exception;

  /**
   * To get product masterdata detail by productCodes and product skus
   * @param storeId
   * @param username
   * @param requestId
   * @param productIds
   * @param productSkus
   * @return MasterDataDetailWithProductAndItemsResponseVo X-product data response
   */
  MasterDataDetailWithProductAndItemsResponseVo getProductMasterDataDetailByProductCodesAndSkus(
      String storeId, String username, String requestId, Set<String> productIds,
      Set<String> productSkus) throws Exception;

  /**
   * @param storeId
   * @param username
   * @param requestId
   * @param itemSku
   * @param needCategoryHierarchies
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  MasterDataDetailWithProductAndItemsResponseVo getPristineProductAndItemsInfoByItemSku(String storeId, String username,
      String requestId, String itemSku, boolean needCategoryHierarchies, boolean combineOthersBundlings, boolean off2On)
      throws Exception;

  /**
   * To get productcodes and productSkus from X-product
   * @param storeId
   * @param username
   * @param requestId
   * @param pristineIds
   * @return list of SimplePristineProductRequestVo
   */
  List<SimplePristineProductRequestVo> getProductCodesAndSkusByPristineIds(String storeId,
      String requestId, String username, Set<String> pristineIds) throws Exception;

  /**
   * Fetch product and item details for active items
   *
   * @param mandatoryRequestParam
   * @param itemSkus
   * @param pristine
   * @param fullFetch
   * @param needWholesaleData
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  List<ProductItemsVo> getProductAndItemsInfoForActiveItem(MandatoryRequestParam mandatoryRequestParam, Set<String> itemSkus,
      boolean pristine, boolean fullFetch, boolean needWholesaleData, boolean combineOthersBundlings, boolean off2On)
      throws Exception;

  /**
   * Fetch product and items details for all (active/suspended/postlive-rejected) items
   *
   * @param mandatoryRequestParam
   * @param itemSkus
   * @param pristine
   * @param fullFetch
   * @param needWholesaleData
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  List<ProductAndItemsVO> getProductAndItemsInfoForAllItems(MandatoryRequestParam mandatoryRequestParam,
      Set<String> itemSkus, boolean pristine, boolean fullFetch, boolean needWholesaleData,
      boolean combineOthersBundlings, boolean off2On)
      throws Exception;
  /**
   * Get Product detail fields for review by ProductSku
   * @param storeId
   * @param username
   * @param requestId
   * @param productSku
   * @param itemSku
   * @return
   * @throws Exception
   */
  ReviewProductDetailVO getProductDetailForReviewByProductSku(String storeId,
      String username, String requestId, String productSku, String itemSku) throws Exception;

  /**
   * Get Product detail fields for official store by MerchantCode and list of brands
   *
   * @param storeId
   * @param clientId
   * @param officialStoreRequestVO with merchantId mandatory
   * @param pageable
   * @return list of product detail with paging
   * @throws Exception
   */
  Page<ProductDetailVo> getProductsForOfficialStore(String storeId, String clientId,
      OfficialStoreRequestVO officialStoreRequestVO, Pageable pageable) throws Exception;

  /**
   * Get Product detail fields for review by ProductCode
   * @param storeId
   * @param username
   * @param requestId
   * @param productCode
   * @param itemSku
   * @return
   * @throws Exception
   */
  ReviewProductDetailVO getProductDetailForReviewByProductCode(String storeId,
      String username, String requestId, String productCode, String itemSku) throws Exception;


  /**
   * Get all item skus
   * @param storeId
   * @param merchantCode
   * @param createdDate
   * @param pageable
   */
  Page<ProductAndItemSolr> getItemsByMerchantCode(String storeId,
      String merchantCode,
      Date createdDate,
      Pageable pageable);

  /**
   * Get active Products by MerchantCode and list of category codes
   *
   * @param storeId
   * @param activeProductsRequestVO
   * @param pageable
   * @throws Exception
   */
  Page<ActiveProductDetailVo> getActiveProductsListForMerchant(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageable) throws Exception;

  /**
   * Get Products by MerchantCode and list of category codes for suspension
   *
   * @param storeId
   * @param activeProductsRequestVO
   * @param pageable
   * @throws Exception
   */
  Page<ActiveProductDetailVo> getActiveProductsListForSuspension(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageable) throws Exception;

  /**
   * Get suspended items by MerchantCode and list of category codes.
   *
   * @param storeId
   * @param activeProductsRequestVO
   * @param pageable
   * @throws Exception
   */
  Page<ItemInfoVO> getSuspendedItemList(String storeId,
      ActiveProductsRequestVO activeProductsRequestVO, Pageable pageable) throws Exception;

  /**
   * save product witho
   *
   * @param itemList
   * @param isArchive
   */
  Product updateForceReviewForProduct(String storeId, List<Item> itemList, boolean forceReview, boolean isArchive);


  /**
   * Fetch complete L1, L2, L3, L4 information by L2 code
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param itemCode
   * @return
   */
  MasterDataDetailWithProductAndItemsResponseVo findMasterDataWithProductAndItemsInfoByItemCode(String storeId,
      String username, String requestId, String itemCode, boolean needCategoryHierarchies) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param pristineId
   * @return
   * @throws IOException
   * @throws SolrServerException
   */
  List<ItemPriceVO> getItemPriceByPristineId(String storeId, String username, String requestId, String pristineId) throws IOException, SolrServerException;

  /**
   * get product counts by using filters and type
   * @param type
   * @param merchantCode
   * @param getActiveCounts
   * @return
   */
  ProductCountResponseVo getProductCountByType(String type, String merchantCode, boolean getActiveCounts);

  /**
   *
   * @param type
   * @param merchantCode
   * @param isActiveCounts
   * @return
   */
  ProductCountResponseVo getProductCountByTypeCacheable(String type, String merchantCode, boolean isActiveCounts);

  /**
   * get MasterDataWithProductItemsVo from input product code
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param productCode
   * @param needCategoryHierarchies
   * @param combineOthersBundlings
   * @param off2On
   * @return
   * @throws Exception
   */
  MasterDataWithProductItemsVo getMasterDataWithProductItemsVo(String storeId, String username,
    String requestId, String productCode, boolean needCategoryHierarchies,
    boolean combineOthersBundlings, boolean off2On) throws Exception;

  /**
   * fetch product summary response based on filter V2
   * @param storeId
   * @param page
   * @param size
   * @param productSummaryRequestV2
   * @return
   */
  Page<ProductSummaryResponseV2Vo> getProductSummaryResponseV2(String storeId, int page, int size, ProductSummaryRequestV2 productSummaryRequestV2);

  /**
   * get Halal Dashboard products
   *
   * @param storeId
   * @param page
   * @param size
   * @param halalProductsFilterRequest
   * @return
   */
  Page<HalalDashboardProductsResponseVo> getHalalDashboardProducts(String storeId, int page, int size,
      HalalProductsFilterRequest halalProductsFilterRequest);
}
