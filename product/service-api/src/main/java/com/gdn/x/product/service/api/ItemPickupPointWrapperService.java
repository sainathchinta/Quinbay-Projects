package com.gdn.x.product.service.api;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointMigrationEvent;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.model.vo.ItemPickupPointTransactionResponse;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequestList;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.EanUpcPickupPointCodeRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.EanUpcPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointPriceResponse;

import java.util.List;

import org.springframework.data.domain.Page;

public interface ItemPickupPointWrapperService {

  /**
   * Publish events to migrate items to new L5 collection
   *
   * @param storeId
   * @param itemSku
   * @param status
   */
  void migrateItemPickupPointCollection(String storeId, String itemSku, String status);

  /**
   * Process migration event for list of item skus
   *
   * @param itemPickupPointMigrationEvent
   */
  void processItemMigrationEvent(ItemPickupPointMigrationEvent itemPickupPointMigrationEvent);

  /**
   * Update ItemPickupPoint on item change event
   *
   * @param itemChange
   * @throws Exception
   */
  void updateItemPickupPointOnItemChange(ItemChange itemChange) throws Exception;

  /**
   * Update ItemPickupPoint on offline item change event
   *
   * @param offlineItemChange
   */
  void updateItemPickupPointOnOfflineItemChange(OfflineItemChange offlineItemChange);

  /**
   * Update markForDelete to true by merchantCode
   *
   * @param storeId
   * @param businessPartnerCode
   */
  void updateMarkForDeleteByMerchantCode(String storeId, String businessPartnerCode);

  /**
   * fetch product details for transaction by itemSku and pickupPointCode
   * @param storeId
   * @param requestId
   * @param username
   * @param itemPickupPointRequests
   * @return
   */
  List<ItemPickupPointTransactionResponse> findProductForTransactionByItemSkusAndPickupPointCode(String storeId, String requestId,
      String username, List<ItemPickupPointRequest> itemPickupPointRequests) throws Exception;

  /**
   * fetch item summary by itemSku and pickupPointCode
   *
   * @param
   * @param storeId
   * @param requestId
   * @param username
   * @param withValueAndValueTypes
   * @param itemPickupPointRequests
   * @param clientId
   * @param catalogType
   * @param excludeDistributionPickupPoint
   * @return
   */
  List<ItemSummaryListResponse> fetchItemSummaryByItemSkusAndPickupPointCode(String storeId,
    String requestId, String username, boolean withValueAndValueTypes,
    List<ItemPickupPointRequest> itemPickupPointRequests, String fetchViewConfigByChannel,
    String clientId, String catalogType, boolean excludeDistributionPickupPoint) throws Exception;

  /**
   * fetch item summary by itemSku and pickupPointCode
   *
   * @param
   * @param storeId
   * @param requestId
   * @param username
   * @param withValueAndValueTypes
   * @param itemPickupPointRequests
   * @param clientId
   * @param catalogType
   * @param excludeDistributionPickupPoint
   * @return
   */
  List<ItemSummaryListResponse> findItemSummaryByItemSkusAndPickupPointCode(String storeId, String requestId,
      String username, boolean withValueAndValueTypes, List<ItemPickupPointRequest> itemPickupPointRequests,
      String fetchViewConfigByChannel, String clientId, String catalogType, boolean excludeDistributionPickupPoint) throws Exception;

  /**
   * update ItemPickupPoint ViewConfig With ProductStatus In ItemPickupPoint
   *
   * @param storeId
   * @param userName
   * @param requestId
   * @param productSku
   * @param itemPickupPointViewConfigBaseRequest
   * @return
   */
  void updateItemPickupPointViewConfigWithProductStatus(String storeId, String userName, String requestId,
      String productSku, ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest) throws Exception;

  /**
   * @param mandatoryRequestParam
   * @param itemPickupPointUpdateRequestVo
   * @param editProductDetailDTO
   * @return
   * @throws Exception
   */
  EditItemResponse updateItemPickupPoint(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, EditProductDetailDTO editProductDetailDTO) throws Exception;

  /**
   *  Fetch Complete price detail response by ItemSku and PickupPointCode
   *
   * @param storeId
   * @param itemPickupPointRequests
   * @return
   * @throws Exception
   */
  List<ItemPickupPointPriceResponse> findPriceDetailsByItemSkuAndPickupPointCode(String storeId, List<ItemPickupPointRequest> itemPickupPointRequests);

  /**
   * @param storeId
   * @param deleteItemPickupPointRequest
   * @return
   */
  List<DeleteItemPickupPointResponse> deleteItemPickupPointsByPickupPointCode(String storeId,
      DeleteItemPickupPointRequest deleteItemPickupPointRequest) throws Exception;

  /**
   * Find L5s by itemSkus
   *
   * @param storeId
   * @param simpleListStringRequest
   * @param page
   * @param size
   * @param fetchViewConfigByChannel
   * @return
   * @throws Exception
   */
  Page<ItemPickupPointL5Response> findItemPickupPointsByItemSkus(String storeId,
      SimpleListStringRequest simpleListStringRequest, int page, int size,
      String fetchViewConfigByChannel) throws Exception;

  /**
   * Create Fbb pickup point
   *
   * @param storeId
   * @param createFbbPickupPointRequest
   * @return
   */
  CreateFbbPickupPointResponse createFbbPickupPoint(String storeId,
      CreateFbbPickupPointRequest createFbbPickupPointRequest);

  /**
   * Return basic product l5 details on basis on item sku and pp code
   *
   * @param storeId
   * @param itemPickupPointRequests
   * @param inAllProducts
   * @param fetchViewConfigByChannel
   * @return
   * @throws Exception
   */
  List<ProductL5DetailResponse> findProductL5DetailByItemSkusAndPickupPointCode(String storeId,
    List<ItemPickupPointRequest> itemPickupPointRequests, boolean inAllProducts,
      String fetchViewConfigByChannel) throws Exception;

  /**
   * auto create pickup point with item sku and pp code
   *
   * @param autoCreatePickupPointRequestList
   * @param mandatoryRequestParam
   * @return
   */
  AutoCreatePickupPointListResponse autoCreatePickupPoint(
      AutoCreatePickupPointRequestList autoCreatePickupPointRequestList, MandatoryRequestParam mandatoryRequestParam);

  /**
   * Find fbb true online L5s
   *
   * @param storeId
   * @param itemSkus
   * @return
   */
  List<ItemSkuPickupPointCodeResponse> findFbbTrueOnlinePickupPointsAndItemSkusIn(String storeId,
      List<String> itemSkus);

  /**
   * fetch Basic Details By ItemSku And PickupPointCode List
   *
   * @param storeId string
   * @param itemPickupPointRequest list of itemSku and pickupPointCode
   * @return
   */
  List<ItemPickupPointBasicResponse> fetchBasicDetailsByItemSkuAndPickupPointCodeList(
    String storeId, List<ItemPickupPointRequest> itemPickupPointRequest);

  /**
   * itemPickupPoint by UPC code and PP Code
   *
   * @param storeId string
   * @param eanUpcPickupPointCodeRequest upc code and pickupPointCode
   * @param fetchViewConfigByChannel
   * @return
   */
  List<EanUpcPickupPointCodeResponse> fetchItemDetailsByEanUpcCode(
      String storeId, EanUpcPickupPointCodeRequest eanUpcPickupPointCodeRequest, String fetchViewConfigByChannel);

  /**
   * cnc at l5 response by storeId and productSku
   *
   * @param storeId string
   * @param productSku
   * @return
   */

  boolean getCncAtL5ByProductSku(String storeId, String productSku);

}
