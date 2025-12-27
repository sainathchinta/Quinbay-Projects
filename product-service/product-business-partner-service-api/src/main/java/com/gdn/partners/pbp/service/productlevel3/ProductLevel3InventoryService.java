package com.gdn.partners.pbp.service.productlevel3;

import com.gda.mta.product.dto.UpdateOrInsertStockVo;
import com.gda.mta.product.dto.InventoryUpsertModel;
import com.gda.mta.product.dto.ItemSkuPickupPointSyncStockDto;
import com.gda.mta.product.dto.SyncStockUpdateOrInsertVo;
import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;

import java.util.List;
import java.util.Map;

public interface ProductLevel3InventoryService {

  ProductLevel3Inventory findInventoryByBusinessPartnerCodeAndGdnSku(String businessPartnerCode,
      String gdnSku) throws Exception;

  List<ProductLevel3Inventory> findInventoryByBusinessPartnerCodeAndListOfGdnSku(
      String businessPartnerCode, List<String> gdnSkus) throws Exception;

  List<ProductLevel3Inventory> findInventoryByGdnSkuMap(
      Map<String, String> gdnSkuMap) throws Exception;


  List<ProductLevel3Inventory> findInventoryByInventoryFilter(String businessPartnerCode,
      Integer stock, ProductLevel3InventoryCriteria inventoryCriteria) throws Exception;

  void insertInventory(List<ProductLevel3Inventory> inventories) throws Exception;

  void updateMinimumStockAlert(String businessPartnerCode, String gdnSku, Integer minimumStockAlert)
      throws Exception;

  void updatePickupPoint(String businessPartnerCode, String gdnSku, String pickupPointCode)
      throws Exception;

  void updateStock(String businessPartnerCode, String gdnSku, Integer deltaStock) throws Exception;

  ApiErrorCode updateStockV2(String businessPartnerCode, String gdnSku, Integer deltaStock) throws Exception;

  void updateSyncStockByBusinessPartnerCode(String businessPartnerCode, boolean syncStock)
      throws Exception;

  void updateSyncStockByBusinessPartnerCodeAndGdnSku(String businessPartnerCode, String gdnSku,
      boolean syncStock, List<ItemSkuPickupPointSyncStockDto> pickupPointSyncStockDtoList) throws Exception;

  /**
   * Find inventory details by itemSku and pickupPoint code
   *
   * @param inventoryDetailInfoRequestDTOList
   * @return
   */
  List<ProductLevel3Inventory> findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList) throws Exception;

  /**
   * Update stock by itemSku and pickupPointCode
   *
   * @param businessPartnerCode
   * @param gdnSku
   * @param pickupPointCode
   * @param deltaStock
   * @throws Exception
   */
  ApiErrorCode updateStockForItemPickupPointWithErrorCode(String businessPartnerCode, String gdnSku,
      String pickupPointCode, Integer deltaStock) throws Exception;

  /**
   * Update stock by itemSku and pickupPointCode
   *
   * @param businessPartnerCode
   * @param gdnSku
   * @param pickupPointCode
   * @param deltaStock
   * @throws Exception
   */
  ApiErrorCode updateStockForItemPickupPoint(String businessPartnerCode, String gdnSku,
    String pickupPointCode, Integer deltaStock, Integer deltaQuota, boolean updatePreOrderQuota) throws Exception;


  /**
   * Delete pickup point from x-inv
   *
   * @param webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOS
   * @throws Exception
   */
  void deleteByItemSkuAndPickupPointCode(
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOS)
      throws Exception;

  /**
   * update stock in x-inventory or insert new l5
   * @param insertStockVo
   * @return
   */
  ApiErrorCode updateOrInsertStock(UpdateOrInsertStockVo insertStockVo) throws Exception;


  /**
   * update sync stock for l5 in x-inventory or insert new l5
   * @param syncStockUpdateVo
   */
  void updateSyncStockOrInsertStock(SyncStockUpdateOrInsertVo syncStockUpdateVo) throws Exception;

  /**
   * @param businessPartnerCode
   * @param inventoryUpsertModel
   * @return
   * @throws Exception
   */
  WebInventoryUpdatePickupPointResponseDTO updatePickupPointInInventory(String businessPartnerCode, List<InventoryUpsertModel> inventoryUpsertModel)
      throws Exception;
}
