package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;

/**
 * Created by govind on 01/08/2018 AD.
 */
public interface ProductReindexingService {

  /**
   * Get Simple Master Data Detail Response by productCodes
   * @param storeId
   * @param username
   * @param requestId
   * @param productCodes
   * @return
   * @throws Exception
   */
  SimpleMasterDataDetailWithProductAndItemsResponseVo
  getMasterDataProductDetailResponseByProductCodes(
      String storeId, String username, String requestId, Set<String> productCodes) throws Exception;

  /**
   * Get Simple Master Data Detail Response by productSkus
   * @param storeId
   * @param username
   * @param requestId
   * @param productSkus
   * @return
   * @throws Exception
   */
  SimpleMasterDataDetailWithProductAndItemsResponseVo
  getMasterDataProductDetailResponseByProductSkus(
      String storeId, String username, String requestId, Set<String> productSkus) throws Exception;

  /**
   * Get Simple Master Data Detail Response by productSkus
   * @param storeId
   * @param username
   * @param requestId
   * @param itemSku
   * @return
   * @throws Exception
   */
  SimpleMasterDataDetailWithProductAndItemsResponseVo getMasterDataProductDetailResponseByItemSku(
      String storeId, String username, String requestId, String itemSku, boolean instantPickup,
      String pickupPointCode) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param itemSku
   * @param pickupPointCode
   * @return
   * @throws Exception
   */
  SimpleMasterDataDetailWithProductAndItemsV2ResponseVo getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(
      String storeId, String username, String requestId, String itemSku, String pickupPointCode, String fetchViewConfigByChannel) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param productSkus
   * @param itemSkus
   * @return
   * @throws Exception
   */
  SimpleMasterDataDetailWithProductAndItemsV2ResponseVo getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(
      String storeId, String username, String requestId, List<String> productSkus,
      List<String> itemSkus, boolean showDeleted, int page, int pageSize, String fetchViewConfigByChannel) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param itemSku
   * @param pageable
   * @return
   * @throws Exception
   */
  Page<OfflineItemDetailVo> getOfflineItemsByItemSku(
      String storeId, String username, String requestId, String itemSku, Pageable pageable) throws Exception;
}
