package com.gdn.partners.pbp.util;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.inventory.v2.rest.web.model.enums.ActionKeyEnum;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryBaseRequest;

import java.util.Date;

public final class OfflineItemInventoryUtil {
  private static final String DASH_SEPARATOR = "-";
  
  private OfflineItemInventoryUtil() {
  }

  public static MandatoryRequestParam convertToInventoryMandatoryRequestParam(
      com.gdn.common.web.param.MandatoryRequestParam source) throws Exception {
    return MandatoryRequestParam
        .generateMandatoryRequestParam(source.getStoreId(), source.getChannelId(),
            source.getClientId(), source.getRequestId(), source.getUsername(),
            source.getAuthenticator());
  }

  public static InventoryBaseRequest generateInventoryBaseRequest(ActionKeyEnum actionKey) {
    InventoryBaseRequest inventoryBaseRequest = new InventoryBaseRequest();
    inventoryBaseRequest.setActionKey(actionKey.name());
    inventoryBaseRequest.setCustomerLogonId(GdnMandatoryRequestParameterUtil.getUsername());
    inventoryBaseRequest.setTransactionDate(new Date());
    return inventoryBaseRequest;
  }

  public static MandatoryRequestParam generateMandatoryRequestParam() throws Exception {
    return MandatoryRequestParam
        .generateMandatoryRequestParam(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(),
            GdnMandatoryRequestParameterUtil.getAuthenticator());
  }

  public static String generateL5OfflineItemId(String itemSku, String pickupPointCode) {
    return itemSku + DASH_SEPARATOR + pickupPointCode;
  }
  
}
