package com.gdn.mta.bulk.service;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.ItemBasicL4Response;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductSkuResponse;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;

public interface XProductOutboundService {

  List<UnmappedSkuResponse> getUnmappedSkus(List<String> categoryCodes);

  SimpleListStringResponse bulkUpdateOff2OnByProductSkus(Map<String, Boolean> stringBooleanMap, String username,
      String requestId, Boolean updateOff2OnHistory);

  Page<ProductL3SummaryResponse> getProductL3SummaryResponse(
      ProductSummaryRequest productSummaryRequest, int page, int size, String requestId,
      String username);

  Page<ItemBasicL4Response> getL4ItemListByProductSku(int page, int size, String requestId, String username,
      ItemLevel4ListingWebRequest itemLevel4ListingWebRequest);

  /**
   * Fetch Item and merchant details by set of item Codes
   * @param requestId
   * @param username
   * @param itemCodesSet
   * @return
   */
  List<ItemCodeDetailResponse> getItemDetailsByItemCodes(String requestId, String username,
      SimpleSetStringRequest itemCodesSet);

  /**
   * Fetch item summary by input filter
   *
   * @param requestId
   * @param username
   * @param pageable
   * @param itemSummaryRequest
   * @return
   */
  Page<ItemSummaryResponse> getItemSummaryByFilter(String requestId, String username, Pageable pageable, ItemSummaryRequest itemSummaryRequest);

  /**
   * @param itemSku
   * @param itemViewConfigBaseRequest
   */
  void updateProductItemViewConfig(String itemSku, ItemViewConfigBaseRequest itemViewConfigBaseRequest)
      throws Exception;

  /**
   *
   * @param productSku
   * @param itemPickupPointViewConfigBaseRequest
   * @throws Exception
   */
  void updateItemPickupPointViewConfigWithProductStatus(String productSku,
      ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) throws Exception;

  /**
   * @param showDeleted
   * @param productSku
   * @param includeForceReview
   */
  GdnRestSingleResponse<ProductAndItemsResponse> getProductAndItemsWithProductData(boolean showDeleted,
      String productSku, boolean includeForceReview);

  /**
   * get product detail by itemSKu
   *
   * @param request
   * @return
   */
  GdnRestListResponse<ProductAndItemInfoResponseV2> getProductInfoByItemSku(GetProductInfoRequestV2 request);

  /**
   *
   * @param storeId
   * @param catalogCode
   * @param categoryCode
   * @param productSkus
   */
  void addSalesCategory(String storeId, String catalogCode, String categoryCode, List<String> productSkus);

  /**
   *
   * @param storeId
   * @param catalogCode
   * @param categoryCode
   * @param productSkus
   */
  void deleteSalesCategory(String storeId, String catalogCode, String categoryCode, List<String> productSkus);

  /**
   * @param productSku
   * @param doArchive
   * @param suppressError
   */
  void archiveByProductSku(String productSku, boolean doArchive, boolean suppressError);

  /**
   * Fetch L3 list by ProductSkuSummaryRequest
   *
   *
   * @param merchantCode
   * @param page
   * @param size
   * @param productSkuSummaryRequest
   * @return
   */
  Page<ProductSkuSummaryResponse> getProductSkuSummaryResponse(String merchantCode, int page, int size,
    ProductSkuSummaryRequest productSkuSummaryRequest);

  /**
   * Get pickupPoints by itemSkus
   *
   * @param itemSkusList
   * @return
   */
  GdnRestListResponse<ItemSkuPickupPointCodeResponse> getItemPickupPointCodeByItemSkus(
      SimpleListStringRequest itemSkusList);

  Page<ItemPickupPointListingResponse> getItemPickupPointList(Pageable pageable,
    ItemPickupPointListingRequest itemPickupPointListingRequest);

  /**
   * Get item summary by itemSkus and pp code
   *
   * @param itemPickupPointRequest
   * @return
   */
  List<ItemSummaryListResponse> getItemSummaryByItemSkuAndPPCode(
    List<ItemPickupPointRequest> itemPickupPointRequest);

  /**
   * Delete L5 with pickupPointCode and businessPartnerCode
   *
   * @param deleteItemPickupPointRequest
   * @return
   */
  List<DeleteItemPickupPointResponse> deleteItemPickupPointByPickupPointCode(
      DeleteItemPickupPointRequest deleteItemPickupPointRequest);

  /**
   * get active products by pickup point code and business partner code
   *
   * @param businessPartnerCode
   * @param pickupPointCode
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<ProductSkuPickupPointResponseV2> getActiveProductsByPickupPointCode(String businessPartnerCode,
      String pickupPointCode, int page, int size) throws Exception;

  /**
   * get list of l4 for a particular product sku
   *
   * @param productSku
   * @return
   * @throws Exception
   */
  List<ItemBasicDetailV2Response> getItemBasicDetails(String productSku);


  List<ItemBasicDetailV2Response> getItemBasicDetailsByItemSku(String itemSku);
  /**
   * get list of l5 for a particular product sku
   *
   * @param productSku
   * @param cncActivated
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<ItemL5ListingResponse> getItemL5Details(String productSku, boolean cncActivated, int page, int size);

  /**
   *
   *
   * @param l5Ids must not be empty
   * @param cncActivated must be true or false
   * @param page must not be empty
   * @param size must not be empty
   * @return
   */
  Page<ItemL5ListingResponse> getItemL5DetailsByL5Ids(List<String> l5Ids, boolean cncActivated, int page, int size);

  /**
   * fetch l3 or l4 price range
   *
   * @param merchantCode
   * @param list
   * @return
   */
  List<PriceRangeResponse> fetchPriceRange(String merchantCode, Set<String> list);

  /**
   * Fetch item basic details
   *
   * @param storeId                 storeId
   * @param fetchBundleRecipe       boolean
   * @param simpleListStringRequest must not be null
   * @return List of ItemBasicDetailV2Response
   */
  List<ItemBasicDetailV2Response> getItemBasicDetailByItemSku(String storeId, boolean fetchBundleRecipe,
      SimpleListStringRequest simpleListStringRequest);

  /**
   * Fetch product basic details
   *
   * @param storeId    storeId
   * @param productSku must not be null
   * @return BasicProductResponse
   */
  BasicProductResponse getBasicProductInfo(String storeId, String productSku);

  /**
   * Check if product is shared
   *
   * @param storeId
   * @param productSku
   * @param businessPartnerCode
   * @return
   */
  boolean isSharedProduct(String storeId, String productSku, String businessPartnerCode);

  /**
   Fetches a paginated list of ProductSkuResponse based on the provided ProductSummaryRequest.

   @param productSummaryRequest the request object containing filters and criteria for fetching product SKUs
   @param page the page number to retrieve (0-based index)
   @param size the number of items per page
   @param requestId the unique identifier for the request
   @param username the username of the user making the request
   @return a paginated list of ProductSkuResponse objects */

  Page<ProductSkuResponse> getProductSkuResponse(ProductSummaryRequest productSummaryRequest, int page, int size,
      String requestId, String username);

  /**
   * Fetches the basic product L3 details based on the provided ProductLevel3SummaryRequest.
   *
   * @param request the request object containing filters and criteria for fetching product basic information.
   * @return a BulkDownloadProductBasicInfoResponse object containing the product basic information details.
   * @throws ApplicationException if an error occurs during the processing of the request.
   */
  BulkDownloadProductBasicInfoResponse getProductBasicInfoDetails(ProductLevel3SummaryRequest request)
      throws ApplicationException;

  /**
   * get product L3 details by productSku
   *
   * @param productSku
   * @return
   * @throws ApplicationException
   */
  ProductL3Response getProductL3DetailsByProductSku(String productSku)
      throws ApplicationException;

  /**
   * Fetch product basic details
   *
   * @param storeId                 storeId
   * @param productSku              must not be null
   * @param sharedProductInfoNeeded
   * @return ProductBasicResponse
   */

  ProductBasicResponse getProductBasicInfo(String storeId, String productSku, boolean sharedProductInfoNeeded);

  /**
   * Fetch product basic details
   *
   * @param productCode to map prd_product to prd_item
   * @return L3VersionResponse
   */
  boolean mapProductToItem(String productCode);
}
