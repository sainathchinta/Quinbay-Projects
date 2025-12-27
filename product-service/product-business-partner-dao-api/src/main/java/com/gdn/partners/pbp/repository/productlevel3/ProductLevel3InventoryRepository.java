package com.gdn.partners.pbp.repository.productlevel3;

import java.util.List;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.dao.UpdateSyncStockByWebItemPickupPointRequest;
import com.gdn.partners.pbp.dao.WebInventoryUpdateStockRequestV2DTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebItemSkuRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.UpdateSyncStockByWebMerchantCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryInsertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateMinimumStockAlertRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointCodeRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryDetailInfoResponseDTO;

public interface ProductLevel3InventoryRepository {

  List<InventoryDetailInfoResponseV2DTO> findDetailByBusinessPartnerCodeAndGdnSku(
      MandatoryRequestParam mandatoryRequestParam, List<InventoryDetailInfoRequestDTO> request)
      throws Exception;

  List<InventoryDetailInfoResponseDTO> findDetailByBusinessPartnerCodeAndStock(
      MandatoryRequestParam mandatoryRequestParam, String businessPartnerCode, Integer stock)
      throws Exception;

  void insertInventory(MandatoryRequestParam mandatoryRequestParam,
      List<WebInventoryInsertRequestDTO> request) throws Exception;

  void updateMinimumStockAlert(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdateMinimumStockAlertRequestDTO request) throws Exception;

  void updatePickupPoint(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdatePickupPointCodeRequestDTO request) throws Exception;

  void updateStockDecrease(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdateStockRequestV2DTO request) throws Exception;

  void updateStockIncrease(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdateStockRequestV2DTO request) throws Exception;

  ApiErrorCode updateStockIncreaseV2(MandatoryRequestParam mandatoryRequestParam,
      WebInventoryUpdateStockRequestV2DTO request) throws Exception;

  void updateSyncStockByBusinessPartnerCode(MandatoryRequestParam mandatoryRequestParam,
      UpdateSyncStockByWebMerchantCodeRequestDTO request) throws Exception;

  void updateSyncStockByBusinessPartnerCodeAndGdnSku(MandatoryRequestParam mandatoryRequestParam,
      UpdateSyncStockByWebItemSkuRequestDTO request) throws Exception;

  List<InventoryDetailInfoResponseV2DTO> findByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
    String storeId, String channelId, String clientId, String requestId, String username,
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList);

  void updateSyncStockAtL5(String storeId, String channelId, String clientId, String requestId, String username,
      List<UpdateSyncStockByWebItemPickupPointRequest> updateSyncStockByWebItemPickupPointRequestList);


  void deleteByItemSkuAndPickupPointCode(String storeId, String channelId, String clientId, String requestId,
      String username,
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> deleteByWebItemSkuAndPickupPointCodeDTOS);
}
