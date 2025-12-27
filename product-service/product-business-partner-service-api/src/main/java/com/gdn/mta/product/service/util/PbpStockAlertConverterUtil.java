package com.gdn.mta.product.service.util;

import java.util.Date;

import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;

public interface PbpStockAlertConverterUtil {
  PbpStockAlert converterForPbpStockAlert(String storeId, String gdnSku, Integer availableStock,
      Integer minimumStock, String businessPartnerCode, ProductLevel3 productLevel3,
      PbpStockAlert pbpStockAlert, Boolean isMinimumStock, Boolean isOOS, Boolean isNewAlert);

  PbpStockAlert converterForPbpStockAlert(String storeId, String gdnSku, Integer availableStock,
      Integer minimumStock, String businessPartnerCode, ProductAndItemsResponse productLevel3,
      PbpStockAlert pbpStockAlert, Boolean isMinimumStock, Boolean isOOS, Boolean isNewAlert);

  PbpStockAlert converterNewPbpStockAlert(String storeId, String businessPartnerCode, String gdnSku,
      String itemName, Integer availableStock, Integer minimumStock, boolean isMinimumStock,
      boolean isOos, Date eventTimestamp, String pickupPointCode);
}
