package com.gdn.mta.bulk.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gda.mta.product.dto.LogAuditTrailUpdatedProductRequest;
import com.gda.mta.product.dto.ProductItemLevel3Response;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.ProductValueComparision;


public class ProductValueComparisonUtil {

  private ProductValueComparisonUtil() {
  }

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductValueComparisonUtil.class);

  public static List<LogAuditTrailUpdatedProductRequest> generateLogAuditTrailProductUpdate(
      String businessPartnerCode, String changedBy, String gdnSku, String clientHost,
      ProductLevel3Response savedProduct, ProductLevel3Response updatedProductValues)
      throws Exception {
    if (savedProduct == null || updatedProductValues == null)
      return new ArrayList<>();
    Map<String, ProductValueComparision> productValueComparisionMap = new HashMap<>();
    productValueComparisionMap.put(
        ProductUpdateErrorMessages.CHANGE_PRODUCT_NAME,
        new ProductValueComparision(savedProduct.getProductName(), updatedProductValues
            .getProductName()));
    productValueComparisionMap.put(
        ProductUpdateErrorMessages.CHANGE_PRODUCT_DESCRIPTION,
        new ProductValueComparision(savedProduct.getDescription(), updatedProductValues
            .getDescription()));
    populateProductValueComparisionMap(productValueComparisionMap, savedProduct.getItems().get(0),
        updatedProductValues.getItems().get(0));
    populateProductValueComparisionMap(productValueComparisionMap, savedProduct.getItems().get(0)
        .getPrices().get(0), updatedProductValues.getItems().get(0).getPrices().get(0));
    populateProductValueComparisionMap(productValueComparisionMap, savedProduct.getItems().get(0)
        .getViewConfigs().get(0), updatedProductValues.getItems().get(0).getViewConfigs().get(0));
    return createProductUpdateLogs(businessPartnerCode, changedBy, productValueComparisionMap,
        gdnSku, clientHost);
  }

  public static List<LogAuditTrailUpdatedProductRequest> generateLogAuditTrailProductUpdate(
      String businessPartnerCode, String changedBy, String gdnSku, String clientHost,
      ProductLevel3SummaryResponse savedProduct, ProductLevel3SummaryResponse updatedProductValues)
      throws Exception {
    Map<String, ProductValueComparision> productValueComparisionMap = new HashMap<>();
    if (savedProduct != null && updatedProductValues != null)
    {
      populateProductValueComparisionMap(productValueComparisionMap, savedProduct,
          updatedProductValues);
      populateProductValueComparisionMap(productValueComparisionMap, savedProduct.getPrices().get(0),
          updatedProductValues.getPrices().get(0));
      populateProductValueComparisionMap(productValueComparisionMap, savedProduct.getViewConfigs()
          .get(0), updatedProductValues.getViewConfigs().get(0));
    }
    return createProductUpdateLogs(businessPartnerCode, changedBy, productValueComparisionMap,
        gdnSku, clientHost);
  }

  private static void populateProductValueComparisionMap(
      Map<String, ProductValueComparision> productValueMap,
      ProductLevel3SummaryResponse existingProductPriceValues,
      ProductLevel3SummaryResponse updatedProductPriceValues) {
    populateProductValueComparisionMap(productValueMap,
        existingProductPriceValues.getAvailableStockLevel2(),
        existingProductPriceValues.getSynchronizeStock(),
        existingProductPriceValues.getOff2OnActiveFlag(),
        existingProductPriceValues.getMerchantSku(),
        updatedProductPriceValues.getAvailableStockLevel2(),
        updatedProductPriceValues.getSynchronizeStock(),
        updatedProductPriceValues.getOff2OnActiveFlag(), updatedProductPriceValues.getMerchantSku());
  }

  private static void populateProductValueComparisionMap(
      Map<String, ProductValueComparision> productValueMap,
      ProductItemLevel3Response existingProductItemValues,
      ProductItemLevel3Response updatedProductItemValues) {
    if (existingProductItemValues == null || updatedProductItemValues == null)
      return;
    populateProductValueComparisionMap(productValueMap,
        existingProductItemValues.getAvailableStockLevel2(),
        existingProductItemValues.getSynchronizeStock(),
        existingProductItemValues.getOff2OnActiveFlag(),
        existingProductItemValues.getMerchantSku(),
        updatedProductItemValues.getAvailableStockLevel2(),
        updatedProductItemValues.getSynchronizeStock(),
        updatedProductItemValues.getOff2OnActiveFlag(), updatedProductItemValues.getMerchantSku());
  }

  private static void populateProductValueComparisionMap(
      Map<String, ProductValueComparision> productValueMap, Integer existingAvailableStockLevel2,
      Boolean existingSynchronizeStock, Boolean existingOff2OnActiveFlag,
      String existingMerchantSku, Integer updatedavailableStockLevel2,
      Boolean updatedSynchronizeStock, Boolean updatedOff2OnActiveFlag, String updatedMerchantSku) {
    ProductUpdateErrorMessages.DECIMAL_FORMAT.setMaximumFractionDigits(0);
    productValueMap.put(ProductUpdateErrorMessages.CHANGE_STOCK_VALUE, new ProductValueComparision(
        String.valueOf(existingAvailableStockLevel2), String.valueOf(updatedavailableStockLevel2)));
    productValueMap.put(
        ProductUpdateErrorMessages.CHANGE_SYNC_STOCK_VALUE,
        new ProductValueComparision(ProductUpdateErrorMessages.SUCCESS_MSG
            .get(existingSynchronizeStock), ProductUpdateErrorMessages.SUCCESS_MSG
            .get(updatedSynchronizeStock)));
    productValueMap.put(
        ProductUpdateErrorMessages.CHANGE_OFF_2_ON_ACTIVE_FLAG,
        new ProductValueComparision(String.valueOf(existingOff2OnActiveFlag), String
            .valueOf(updatedOff2OnActiveFlag)));
    productValueMap.put(ProductUpdateErrorMessages.CHANGE_SELLER_SKU,
        new ProductValueComparision(existingMerchantSku, updatedMerchantSku));
  }

  private static void populateProductValueComparisionMap(
      Map<String, ProductValueComparision> productValueMap,
      ProductLevel3PriceResponse existingProductPriceValues,
      ProductLevel3PriceResponse updatedProductPriceValues) {
    ProductUpdateErrorMessages.DECIMAL_FORMAT.setMaximumFractionDigits(0);
    productValueMap.put(
        ProductUpdateErrorMessages.CHANGE_SELLING_PRICE,
        new ProductValueComparision(ProductUpdateErrorMessages.DECIMAL_FORMAT
            .format(existingProductPriceValues.getSalePrice()),
            ProductUpdateErrorMessages.DECIMAL_FORMAT.format(updatedProductPriceValues
                .getSalePrice())));
    productValueMap
        .put(
            ProductUpdateErrorMessages.CHANGE_NORMAL_PRICE,
            new ProductValueComparision(ProductUpdateErrorMessages.DECIMAL_FORMAT
                .format(existingProductPriceValues.getPrice()),
                ProductUpdateErrorMessages.DECIMAL_FORMAT.format(updatedProductPriceValues
                    .getPrice())));
  }

  private static void populateProductValueComparisionMap(
      Map<String, ProductValueComparision> productValueMap,
      ProductLevel3ViewConfigResponse existingProductViewValues,
      ProductLevel3ViewConfigResponse updatedProductViewValues) {
    ProductUpdateErrorMessages.DECIMAL_FORMAT.setMaximumFractionDigits(0);
    productValueMap.put(ProductUpdateErrorMessages.CHANGE_BUYABLE, new ProductValueComparision(
        ProductUpdateErrorMessages.SUCCESS_MSG.get(existingProductViewValues.getBuyable()),
        ProductUpdateErrorMessages.SUCCESS_MSG.get(updatedProductViewValues.getBuyable())));
    productValueMap.put(ProductUpdateErrorMessages.CHANGE_DISPLAYABLE, new ProductValueComparision(
        ProductUpdateErrorMessages.SUCCESS_MSG.get(existingProductViewValues.getDisplay()),
        ProductUpdateErrorMessages.SUCCESS_MSG.get(updatedProductViewValues.getDisplay())));
  }

  private static List<LogAuditTrailUpdatedProductRequest> createProductUpdateLogs(
      String businessPartnerCode, String changedBy,
      Map<String, ProductValueComparision> productValueMap, String gdnSku, String clientHost)
      throws Exception {
    List<LogAuditTrailUpdatedProductRequest> auditLogForProductUpdateList = new ArrayList<>();
    Date currentTime = Calendar.getInstance().getTime();
    ProductValueComparision productValueComparision = null;
    for (String activity : productValueMap.keySet()) {
      productValueComparision = productValueMap.get(activity);
      if (productValueComparision == null || productValueComparision.getNewValue() == null) {
        LOGGER.debug(
            "Failed to Update product details in Audit Log Table as updated values not found for "
                + "activity: {} BusinessPartnerCode: {} ", activity, businessPartnerCode);
      } else if ((StringUtils.isEmpty(productValueComparision.getOldValue()) && StringUtils
          .isNotEmpty(productValueComparision.getNewValue())) || !productValueComparision
          .getOldValue().equals(productValueComparision.getNewValue())) {
        LOGGER.debug(
            "Updating product details in Audit Log Table for activity: {} BusinessPartnerCode: {} ",
            activity, businessPartnerCode);
        LogAuditTrailUpdatedProductRequest auditLogForProductUpdate =
            new LogAuditTrailUpdatedProductRequest();
        auditLogForProductUpdate.setAccessTime(currentTime);
        auditLogForProductUpdate.setActivity(activity);
        auditLogForProductUpdate.setBusinessPartnerCode(businessPartnerCode);
        auditLogForProductUpdate.setChangedBy(changedBy);
        auditLogForProductUpdate.setGdnSku(gdnSku);
        auditLogForProductUpdate.setClientHost(clientHost);
        auditLogForProductUpdate.setOldValues(productValueComparision.getOldValue());
        auditLogForProductUpdate.setNewValues(productValueComparision.getNewValue());
        auditLogForProductUpdate.setStatus(true);
        auditLogForProductUpdateList.add(auditLogForProductUpdate);
      }
    }
    return auditLogForProductUpdateList;
  }
}
