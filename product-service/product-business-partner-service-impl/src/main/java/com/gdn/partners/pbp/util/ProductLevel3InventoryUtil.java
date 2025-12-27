package com.gdn.partners.pbp.util;

import java.util.Date;
import java.util.Objects;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pbp.dao.WebInventoryUpdateStockRequestV2DTO;
import com.gdn.x.inventory.v2.rest.web.model.enums.ActionKeyEnum;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdateStockRequestDTO;

public final class ProductLevel3InventoryUtil {
  private ProductLevel3InventoryUtil() {}

  public static InventoryBaseRequest generateInventoryBaseRequest(ActionKeyEnum actionKey) {
    InventoryBaseRequest inventoryBaseRequest = new InventoryBaseRequest();
    inventoryBaseRequest.setActionKey(actionKey.name());
    inventoryBaseRequest.setCustomerLogonId(GdnMandatoryRequestParameterUtil.getUsername());
    inventoryBaseRequest.setTransactionDate(new Date());
    return inventoryBaseRequest;
  }

  public static InventoryBaseRequest generateInventoryBaseRequest(String actionKey) {
    InventoryBaseRequest inventoryBaseRequest = new InventoryBaseRequest();
    inventoryBaseRequest.setActionKey(actionKey);
    inventoryBaseRequest.setCustomerLogonId(GdnMandatoryRequestParameterUtil.getUsername());
    inventoryBaseRequest.setTransactionDate(new Date());
    return inventoryBaseRequest;
  }

  public static MandatoryRequestParam generateMandatoryRequestParam() throws Exception {
    return MandatoryRequestParam.generateMandatoryRequestParam(
        GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        GdnMandatoryRequestParameterUtil.getAuthenticator());
  }

  public static MandatoryRequestParam convertToInventoryMandatoryRequestParam(
      com.gdn.common.web.param.MandatoryRequestParam source) throws Exception {
    return MandatoryRequestParam.generateMandatoryRequestParam(source.getStoreId(),
        source.getChannelId(),
        source.getClientId(), source.getRequestId(), source.getUsername(),
        source.getAuthenticator());
  }

  public static boolean shouldProceedWithUpdate(Integer deltaStock, Integer deltaQuota,
      boolean updatePreOrderQuota) {
    boolean hasStockUpdate = hasValidValue(deltaStock);
    boolean hasQuotaUpdate = updatePreOrderQuota && hasValidValue(deltaQuota);

    return hasStockUpdate || hasQuotaUpdate;
  }

  public static boolean hasValidValue(Integer value) {
    return Objects.nonNull(value) && !Integer.valueOf(0).equals(value);
  }

  public static WebInventoryUpdateStockRequestV2DTO buildUpdateRequest(String businessPartnerCode,
      String gdnSku, String pickupPointCode, Integer deltaStock, Integer deltaQuota,
      boolean updatePreOrderQuota) {

    WebInventoryUpdateStockRequestV2DTO request = new WebInventoryUpdateStockRequestV2DTO();
    request.setWebItemSku(gdnSku);
    request.setWebMerchantCode(businessPartnerCode);
    request.setPickupPointCode(pickupPointCode);

    Integer stockValue = Objects.nonNull(deltaStock) ? Math.abs(deltaStock) : 0;
    request.setStock(stockValue);

    if (updatePreOrderQuota && hasValidValue(deltaQuota)) {
      request.setPreOrderQuota(Math.abs(deltaQuota));
      //TODO: UPDATE_PRE_ORDER_QUOTA
      request.setInventoryBaseRequest(
          ProductLevel3InventoryUtil.generateInventoryBaseRequest(ActionKeyEnum.UPDATE_WEB_STOCK));
    } else {
      request.setInventoryBaseRequest(
          ProductLevel3InventoryUtil.generateInventoryBaseRequest(ActionKeyEnum.UPDATE_WEB_STOCK));
    }
    return request;
  }

  public static boolean isStockDecrease(Integer deltaStock) {
    return Objects.nonNull(deltaStock) && deltaStock < 0;
  }
}
