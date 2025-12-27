package com.gdn.mta.product.service.util;

import java.util.Date;

import org.springframework.stereotype.Component;

import com.gdn.mta.product.entity.PbpStockAlert;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;

@Component
public class PbpStockAlertConverterUtilBean implements PbpStockAlertConverterUtil {

  @Override
  public PbpStockAlert converterForPbpStockAlert(String storeId, String gdnSku,
      Integer availableStock, Integer minimumStock, String businessPartnerCode,
      ProductLevel3 productLevel3, PbpStockAlert pbpStockAlert, Boolean isMinimumStock,
      Boolean isOOS, Boolean isNewAlert) {
    if (isNewAlert) {
      pbpStockAlert = new PbpStockAlert();
      pbpStockAlert.setStoreId(storeId);
      pbpStockAlert.setGdnSku(gdnSku);
      pbpStockAlert.setOosAlertAttempt(0);
      pbpStockAlert.setAvailableStock(availableStock);
      pbpStockAlert.setMinimumStock(minimumStock);
      pbpStockAlert.setBusinessPartnerCode(businessPartnerCode);
      pbpStockAlert.setMarkForDelete(false);
      pbpStockAlert.setIsMinimumStock(isMinimumStock);
      pbpStockAlert.setIsOos(isOOS);
      if(isOOS) {
        pbpStockAlert.setOosDate(new Date());
      }
      pbpStockAlert.setProductName(productLevel3.getProductName());
    } else {
      if (pbpStockAlert.isMarkForDelete()) {
        pbpStockAlert.setMarkForDelete(false);
      }
      pbpStockAlert.setGdnSku(gdnSku);
      pbpStockAlert.setAvailableStock(availableStock);
      if (minimumStock != null) {
        pbpStockAlert.setMinimumStock(minimumStock);
      }
      pbpStockAlert.setBusinessPartnerCode(businessPartnerCode);
      if (isMinimumStock != null) {
        pbpStockAlert.setIsMinimumStock(isMinimumStock);
      }
      if (isOOS != null) {
        pbpStockAlert.setIsOos(isOOS);
        if(isOOS) {
          pbpStockAlert.setOosDate(new Date());
        }
      }
      pbpStockAlert.setProductName(productLevel3.getProductName());
    }
    return pbpStockAlert;
  }


  @Override
  public PbpStockAlert converterForPbpStockAlert(String storeId, String gdnSku,
      Integer availableStock, Integer minimumStock, String businessPartnerCode,
      ProductAndItemsResponse productLevel3, PbpStockAlert pbpStockAlert, Boolean isMinimumStock,
      Boolean isOOS, Boolean isNewAlert) {
    if (isNewAlert) {
      pbpStockAlert = new PbpStockAlert();
      pbpStockAlert.setStoreId(storeId);
      pbpStockAlert.setGdnSku(gdnSku);
      pbpStockAlert.setOosAlertAttempt(0);
      pbpStockAlert.setAvailableStock(availableStock);
      pbpStockAlert.setMinimumStock(minimumStock);
      pbpStockAlert.setBusinessPartnerCode(businessPartnerCode);
      pbpStockAlert.setMarkForDelete(false);
      pbpStockAlert.setIsMinimumStock(isMinimumStock);
      pbpStockAlert.setIsOos(isOOS);
      if(isOOS) {
        pbpStockAlert.setOosDate(new Date());
      }
      pbpStockAlert.setProductName(productLevel3.getProduct().getMasterDataProduct() != null
          ? productLevel3.getProduct().getMasterDataProduct().getProductName() : "-");
    } else {
      if (pbpStockAlert.isMarkForDelete()) {
        pbpStockAlert.setMarkForDelete(false);
      }
      pbpStockAlert.setGdnSku(gdnSku);
      if (availableStock != null) {
        pbpStockAlert.setAvailableStock(availableStock);
      }
      if (pbpStockAlert.getAvailableStock() == null) {
        pbpStockAlert.setAvailableStock(0);
      }
      if (minimumStock != null) {
        pbpStockAlert.setMinimumStock(minimumStock);
      }
      pbpStockAlert.setBusinessPartnerCode(businessPartnerCode);
      if (isMinimumStock != null) {
        pbpStockAlert.setIsMinimumStock(isMinimumStock);
      }
      if (isOOS != null) {
        pbpStockAlert.setIsOos(isOOS);
        if(isOOS) {
          pbpStockAlert.setOosDate(new Date());
        }
      }
      pbpStockAlert.setProductName(productLevel3.getProduct().getMasterDataProduct() != null
          ? productLevel3.getProduct().getMasterDataProduct().getProductName() : "-");
    }
    return pbpStockAlert;
  }
  
  @Override
  public PbpStockAlert converterNewPbpStockAlert(String storeId,String businessPartnerCode, String gdnSku,String itemName, 
      Integer availableStock, Integer minimumStock, boolean isMinimumStock, boolean isOos, Date eventTimestamp, String pickupPointCode) {
    PbpStockAlert pbpStockAlert = new PbpStockAlert();
    pbpStockAlert.setStoreId(storeId);
    pbpStockAlert.setBusinessPartnerCode(businessPartnerCode);
    pbpStockAlert.setGdnSku(gdnSku);
    pbpStockAlert.setProductName(itemName);
    pbpStockAlert.setAvailableStock(availableStock);
    pbpStockAlert.setMinimumStock(minimumStock);
    pbpStockAlert.setIsMinimumStock(isMinimumStock);
    pbpStockAlert.setIsOos(isOos);
    pbpStockAlert.setPickupPointCode(pickupPointCode);
    if(isOos) {
      pbpStockAlert.setOosDate(eventTimestamp);
    }
    pbpStockAlert.setOosAlertAttempt(0);
    pbpStockAlert.setEventTimestamp(eventTimestamp);
    return pbpStockAlert;
  }
}
