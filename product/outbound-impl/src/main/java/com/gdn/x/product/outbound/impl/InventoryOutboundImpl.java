package com.gdn.x.product.outbound.impl;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.InventoryStockInfoDTO;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.InventoryStockAvailableResponseDTO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.outbound.api.feign.InventoryFeign;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Slf4j
@Service
public class InventoryOutboundImpl implements InventoryOutbound {

  @Autowired
  private InventoryFeign inventoryFeign;

  @Value("${call.new.inventory.api.for.stock.availability}")
  private boolean callNewInventoryApiForStockAvailability;

  private List<InventoryStockInfoDTO> getStockInfoByProductSkuList(String requestId, String username,
      List<String> productSkuList, String businessPartnerCode, int batchSize) {
    List<List<String>> productSkuPartitions = Lists.partition(productSkuList, batchSize);
    List<InventoryStockInfoDTO> inventoryStockInfoDTOList = new ArrayList<>();
    for (List<String> productSkuBatch : productSkuPartitions) {
      GdnRestListResponse<InventoryStockInfoDTO> productSkuInventoryInfoResponse = inventoryFeign
          .findDetailByWebProductSkus(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID_X_PRODUCT, requestId, username,
              new InventoryDetailStockInfoRequestDTO(businessPartnerCode, productSkuBatch));
      if (!productSkuInventoryInfoResponse.isSuccess() || Objects.isNull(productSkuInventoryInfoResponse.getContent())) {
        log.error("Error to fetch inventory info for following L3 : {}, error - {}",
            productSkuBatch, productSkuInventoryInfoResponse.getErrorMessage());
        throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
            ErrorMessages.FAILED_TO_FETCH_INVENTORY_INFO_BY_L3);
      }
      inventoryStockInfoDTOList.addAll(productSkuInventoryInfoResponse.getContent());
    }
    return inventoryStockInfoDTOList;
  }

  @Override
  public boolean deleteByItemSkuAndPickupPointCode(String requestId, String username,
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> request) {
    GdnBaseRestResponse gdnBaseRestResponse =
        inventoryFeign.deleteByItemSkuAndPickupPointCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, requestId, username, new ListRequestDTO<>(request));
    if (!gdnBaseRestResponse.isSuccess()) {
      log.error("Error while deleting L5's in inventory : {}, error - {}",
          request, gdnBaseRestResponse.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorMessages.FAILED_TO_FETCH_INVENTORY_INFO_BY_L3);
    }
    return gdnBaseRestResponse.isSuccess();
  }

  private boolean isProductHavingStock(String requestId, String username, String productSku, boolean isPreOrder) {
    GdnRestSingleResponse<InventoryStockAvailableResponseDTO> gdnRestSingleResponse = inventoryFeign
        .checkStockAvailable(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID_X_PRODUCT,
            requestId, username, productSku, isPreOrder);
    if (!gdnRestSingleResponse.isSuccess() || Objects.isNull(gdnRestSingleResponse.getValue())) {
      log.error("Error while checking for OOS in inventory for productSku : {} ", productSku);
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorMessages.FAILED_TO_FETCH_OOS_INFO_BY_L3);
    }
    return gdnRestSingleResponse.getValue().isStockAvailable();
  }

  @Override
  public boolean isProductInStock(String productSku, boolean isPreOrder) {
    boolean isProductInStock = false;
    if (callNewInventoryApiForStockAvailability) {
      isProductInStock = isProductHavingStock(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, productSku, isPreOrder);
    } else {
      List<InventoryStockInfoDTO> inventoryStockInfoDTOList =
          getStockInfoByProductSkuList(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              Arrays.asList(productSku), StringUtils.EMPTY, 1);
      isProductInStock = Optional.ofNullable(inventoryStockInfoDTOList).orElse(new ArrayList<>()).stream().findFirst()
          .map(inventoryStockInfoDTO -> (Optional.ofNullable(inventoryStockInfoDTO.getTotalStock()).orElse(
              inventoryStockInfoDTO.getWarehouseTotalAvailableStock()
                  + inventoryStockInfoDTO.getWebTotalAvailableStock()) > 0)).orElse(false);
    }
    return isProductInStock;
  }
}
