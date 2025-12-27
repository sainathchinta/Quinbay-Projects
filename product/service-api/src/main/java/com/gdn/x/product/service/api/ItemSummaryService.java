package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.rest.web.model.response.ItemCodeBasicDetailResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import com.gdn.common.exception.ApplicationException;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.BulkItemSummaryRequestVo;
import com.gdn.x.product.model.vo.CampaignItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryRequestVO;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemsSummaryDetailRequestVo;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;

public interface ItemSummaryService {

  /**
   * @param storeId
   * @param itemSkus
   * @return
   */
  Map<String, String> getItemNameByItemSkus(String storeId, List<String> itemSkus, boolean includeMarkForDelete);

  /**
   * Query to solr using pagination. If fields in the itemFilter is null then that fields will be
   * ignored in the query criteria. The condition is using AND condition. All field is equal search
   * except itemName and itemSkuKeywords. Promotion product discont will also be set if exists.
   *
   * @param storeId
   * @param itemFilter
   * @param orderBy
   * @param sortBy
   * @param pageRequest
   * @return null if no data found
   */
  ItemSummaryPageResponseVo getItemSummaryByFilter(String storeId, String username, String requestId,
      ItemSummaryRequestVO itemFilter, String orderBy, String sortBy, PageRequest pageRequest);


  /**
   * Query to solr using pagination.
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param itemFilter
   * @param orderBy
   * @param sortBy
   * @param pageRequest
   * @return null if no data found
   */
  ItemSummaryPageResponseVo getItemNamesByKeyword(String storeId, String username, String requestId,
      ItemSummaryRequestVO itemFilter, String orderBy, String sortBy, PageRequest pageRequest);

  /**
   * @param storeId     store unique identifier
   * @param username    username
   * @param requestId   request identifier
   * @param itemFilter  item summary filter request
   * @param pageRequest paging request information
   *
   * @param sortBy
   * @param orderBy
   * @return item summary details based on filters
   */
  ItemSummaryPageResponseVo getBulkItemSummaryByFilter(String storeId, String username, String requestId,
      BulkItemSummaryRequestVo itemFilter, PageRequest pageRequest, String sortBy, String orderBy);

  ItemSummaryPageResponseVo getItemSummaryByArchivedFilter(String storeId, String username,
      String requestId, ItemSummaryRequestVO itemFilter, PageRequest pageRequest) throws Exception;

  /**
   * @param storeId
   * @param productSkus
   * @return
   */
  Map<String, String> getProductNameByProductSkus(String storeId, List<String> productSkus);

  Map<String, String> getProductNamesByProductCodes(String storeId, List<String> productCodes);

  /**
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param itemSku
   * @param merchantCode
   * @param updateItemSummaryRequestVo
   * @param wholsalePriceActivated
   * @return
   * @throws Exception
   */
  ItemSummaryResponseVO updateItemSummary(String storeId, String requestId, String username, String itemSku,
      String merchantCode, UpdateItemSummaryRequestVo updateItemSummaryRequestVo, Boolean wholsalePriceActivated)
      throws Exception;

  /**
   * item listing update
   * @param storeId
   * @param requestId
   * @param username
   * @param productSku
   * @param productType
   * @param itemListingUpdateRequestVos
   * @throws Exception
   */
  void updateItemListing(String storeId, String requestId, String username, String productSku, ProductType productType,
      List<ItemListingUpdateRequestVo> itemListingUpdateRequestVos) throws Exception;

  /**
   * API to fetch all the items from the campaignItemSummaryRequestVO filter request
   * @param storeId
   * @param username
   * @param requestId
   * @param campaignItemSummaryRequestVO
   * @param pageRequest
   */
  ItemSummaryPageResponseVo getCampaignItemSummaryByFilter(String storeId, String username,
      String requestId, CampaignItemSummaryRequestVO campaignItemSummaryRequestVO, PageRequest pageRequest)
      throws ApplicationException;

  /**
   * Get Promo Item Summary from Solr. This query uses positive boosting on a list of boostProductSkus if present.
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param itemSummaryRequestVO
   * @param orderBy
   * @param sortBy
   * @param pageRequest
   * @return
   */
  ItemSummaryPageResponseVo getPromoItemSummaryByFilter(String storeId, String requestId, String username,
      ItemSummaryRequestVO itemSummaryRequestVO, String orderBy, String sortBy, PageRequest pageRequest);

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param itemFilter
   * @param page
   * @param size
   * @return
   */
  ItemSummaryPageResponseVo getItemsSummaryDetailByFilter(String storeId, String username, String requestId,
      ItemsSummaryDetailRequestVo itemFilter, int page, int size) throws Exception;

  /**
   * find item pickupPoints and generated item name
   * @param storeId
   * @param productSku
   * @param page
   * @param size
   * @param fbbActivated
   * @return
   */
  Page<ItemPickupPointVo> getItemPickupPointsAndItemNameByProductSku(String storeId, String productSku, int page,
      int size, boolean fbbActivated);

  /**
   * Get item images response
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ItemImagesListResponse> getItemImagesListResponse(String storeId, Set<String> itemSkus) throws Exception;

  /**
   * Get item basic details by productSku
   *
   * @param storeId
   * @param productSku
   * @return
   * @throws Exception
   */
  List<ItemBasicDetailV2Response> getItemBasicDetailsByProductSku(String storeId, String productSku) throws Exception;

  /**
   * Get basic item details by itemSkus
   *
   * @param storeId
   * @param fetchBundleRecipe
   * @param itemSkus
   * @return
   * @throws Exception
   */
  List<ItemBasicDetailV2Response> getBulkItemDetailsByItemSkus(String storeId, boolean fetchBundleRecipe,
      List<String> itemSkus) throws Exception;


  /**
   * Get item basic details by itemCodes
   *
   * @param storeId
   * @param itemCodes
   * @return
   * @throws Exception
   */
  List<ItemBasicDetailV2Response> getItemBasicDetailsByItemCodes(String storeId, List<String> itemCodes)
      throws Exception;

  /**
   * Get item basic details by itemSkus
   *
   * @param storeId
   * @param inAllProducts
   * @param needProductData
   * @param needCategoryData
   * @param itemSkus
   * @param needFbbFlag
   * @param needAttributeData
   * @return
   * @throws Exception
   */
  List<ItemBasicDetailV2Response> getItemBasicDetailsByItemSkus(String storeId, boolean inAllProducts,
      boolean needProductData, boolean needCategoryData, List<String> itemSkus, boolean needFbbFlag,
      boolean needAttributeData) throws Exception;

  /**
   * Fetch the item summary by item sku
   *  @param storeId
   * @param username
   * @param requestId
   * @param itemSku
   * @param fetchViewConfigByChannel
   */
  ItemSummaryPageResponseVo getItemSummaryByItemSku(String storeId, String username, String requestId, String itemSku,
      String fetchViewConfigByChannel) throws Exception;

  /**
   * Get item detailList for itemSkus
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ItemSummaryListResponse> getItemSummaryByItemSkusList(String storeId, List<String> itemSkus) throws Exception;

  /**
   *  @param storeId
   * @param itemSkus
   * @param wholeSalePriceActivated
   */
  void updateWholeSaleActivationFlag(String storeId, List<String> itemSkus, boolean wholeSalePriceActivated);

  /**
   * @param storeId   not null
   * @param itemCodes list of item codes
   * @param searchKey
   * @param page      start
   * @param size      size
   * @param sortBy
   * @param orderBy
   * @return basic item summary response
   */
  Page<ItemCodeBasicDetailResponse> fetchBasicItemDetailsByItemCodes(String storeId,
    String itemCodes, String searchKey, int page, int size, String sortBy, String orderBy);
}
