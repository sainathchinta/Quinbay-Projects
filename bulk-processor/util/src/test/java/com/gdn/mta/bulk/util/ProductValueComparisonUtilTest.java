package com.gdn.mta.bulk.util;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.LogAuditTrailUpdatedProductRequest;
import com.gda.mta.product.dto.ProductItemLevel3Response;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;


public class ProductValueComparisonUtilTest {

  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00106";
  private static final String DEFAULT_BUSINESS_GDN_SKU = "BLI-00106-001-002";
  private static final String DEFAULT_BUSINESS_CLIENTHOST = "0.0.0.1";
  public static final String PRODUCT_NAME = "PRODUCT NAME";
  public static final String PRODUCT_DESCRIPTION = "PRODUCT DESCRIPTION";
  public static final Double PRODUCT_PRICE = 10000D;
  public static final Integer PRODUCT_AVAILABLE_STOCK = 1;

  public static final String PRODUCT_NAME_UPDATED = "PRODUCT NAME UPDATED";
  public static final String PRODUCT_DESCRIPTION_UPDATED = "PRODUCT DESCRIPTION UPDATED";
  public static final Double PRODUCT_PRICE_UPDATED = 20000D;
  public static final Integer PRODUCT_AVAILABLE_STOCK_UPDATED = 2;

  @Test
  public void generateLogAuditTrailProductUpdateTest() throws Exception {
    ProductLevel3Response savedProduct = getProductLevel3Response(true);
    ProductLevel3Response updatedProductValues = getProductLevel3Response(false);
    List<LogAuditTrailUpdatedProductRequest> auditTrailProductUpdateList =
        ProductValueComparisonUtil.generateLogAuditTrailProductUpdate(
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME, DEFAULT_BUSINESS_GDN_SKU,
            DEFAULT_BUSINESS_CLIENTHOST, savedProduct, updatedProductValues);
    List<LogAuditTrailUpdatedProductRequest> updatedProductRequestList =
        getLogAuditTrailUpdatedProductRequest(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_GDN_SKU, DEFAULT_BUSINESS_CLIENTHOST, true);
    boolean founditem;
    for (LogAuditTrailUpdatedProductRequest updatedProductRequest : updatedProductRequestList) {
      founditem = false;
      for (LogAuditTrailUpdatedProductRequest auditTrailProductUpdate : auditTrailProductUpdateList) {
        if (auditTrailProductUpdate.getActivity().equals(updatedProductRequest.getActivity())) {
          assertEqual(auditTrailProductUpdate, updatedProductRequest);
          founditem = true;
          break;
        }
      }
      if (!founditem)
        Assertions.assertTrue(founditem);
    }
  }

  @Test
  public void generateLogAuditTrailProductUpdateTestWithotNameDescription() throws Exception {
    ProductLevel3SummaryResponse savedProduct = getProductLevel3SummaryResponse(true);
    ProductLevel3SummaryResponse updatedProductValues = getProductLevel3SummaryResponse(false);
    List<LogAuditTrailUpdatedProductRequest> auditTrailProductUpdateList =
        ProductValueComparisonUtil.generateLogAuditTrailProductUpdate(
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME, DEFAULT_BUSINESS_GDN_SKU,
            DEFAULT_BUSINESS_CLIENTHOST, savedProduct, updatedProductValues);
    List<LogAuditTrailUpdatedProductRequest> updatedProductRequestList =
        getLogAuditTrailUpdatedProductRequest(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME,
            DEFAULT_BUSINESS_GDN_SKU, DEFAULT_BUSINESS_CLIENTHOST, false);
    boolean founditem;
    for (LogAuditTrailUpdatedProductRequest updatedProductRequest : updatedProductRequestList) {
      founditem = false;
      for (LogAuditTrailUpdatedProductRequest auditTrailProductUpdate : auditTrailProductUpdateList) {
        if (auditTrailProductUpdate.getActivity().equals(updatedProductRequest.getActivity())) {
          assertEqual(auditTrailProductUpdate, updatedProductRequest);
          founditem = true;
          break;
        }
      }
      if (!founditem)
        Assertions.assertTrue(founditem);
    }
  }

  private void assertEqual(LogAuditTrailUpdatedProductRequest savedLogAuditTrail,
      LogAuditTrailUpdatedProductRequest updatedLogAuditTrail) {
    Assertions.assertEquals(savedLogAuditTrail.getActivity(), updatedLogAuditTrail.getActivity());
    Assertions.assertEquals(savedLogAuditTrail.getNewValues(), updatedLogAuditTrail.getNewValues());
    Assertions.assertEquals(savedLogAuditTrail.getOldValues(), updatedLogAuditTrail.getOldValues());
  }

  private List<LogAuditTrailUpdatedProductRequest> getLogAuditTrailUpdatedProductRequest(
      String businessPartnerCode, String changedBy, String gdnSku, String clientHost,
      boolean withdescription) {
    ProductUpdateErrorMessages.DECIMAL_FORMAT.setMaximumFractionDigits(0);
    List<LogAuditTrailUpdatedProductRequest> auditLogForProductUpdateList = new ArrayList<>();
    if (withdescription) {
      auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
          gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_PRODUCT_NAME, PRODUCT_NAME,
          PRODUCT_NAME_UPDATED));
      auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
          gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_PRODUCT_DESCRIPTION,
          PRODUCT_DESCRIPTION, PRODUCT_DESCRIPTION_UPDATED));
    }
    auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
        gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_STOCK_VALUE,
        String.valueOf(PRODUCT_AVAILABLE_STOCK), String.valueOf(PRODUCT_AVAILABLE_STOCK_UPDATED)));
    auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
        gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_SYNC_STOCK_VALUE,
        ProductUpdateErrorMessages.SUCCESS_MSG.get(true),
        ProductUpdateErrorMessages.SUCCESS_MSG.get(false)));
    auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
        gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_OFF_2_ON_ACTIVE_FLAG,
        String.valueOf(true), String.valueOf(false)));
    auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
        gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_SELLING_PRICE,
        ProductUpdateErrorMessages.DECIMAL_FORMAT.format(PRODUCT_PRICE),
        ProductUpdateErrorMessages.DECIMAL_FORMAT.format(PRODUCT_PRICE_UPDATED)));
    auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
        gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_NORMAL_PRICE,
        ProductUpdateErrorMessages.DECIMAL_FORMAT.format(PRODUCT_PRICE),
        ProductUpdateErrorMessages.DECIMAL_FORMAT.format(PRODUCT_PRICE_UPDATED)));
    auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
        gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_BUYABLE,
        ProductUpdateErrorMessages.SUCCESS_MSG.get(true),
        ProductUpdateErrorMessages.SUCCESS_MSG.get(false)));
    auditLogForProductUpdateList.add(createProductUpdateLogs(businessPartnerCode, changedBy,
        gdnSku, clientHost, ProductUpdateErrorMessages.CHANGE_DISPLAYABLE,
        ProductUpdateErrorMessages.SUCCESS_MSG.get(true),
        ProductUpdateErrorMessages.SUCCESS_MSG.get(false)));
    return auditLogForProductUpdateList;
  }

  private LogAuditTrailUpdatedProductRequest createProductUpdateLogs(String businessPartnerCode,
      String changedBy, String gdnSku, String clientHost, String activity, String oldValue,
      String newValue) {
    LogAuditTrailUpdatedProductRequest auditLogForProductUpdate =
        new LogAuditTrailUpdatedProductRequest();
    auditLogForProductUpdate.setActivity(activity);
    auditLogForProductUpdate.setBusinessPartnerCode(businessPartnerCode);
    auditLogForProductUpdate.setChangedBy(changedBy);
    auditLogForProductUpdate.setGdnSku(gdnSku);
    auditLogForProductUpdate.setClientHost(clientHost);
    auditLogForProductUpdate.setOldValues(oldValue);
    auditLogForProductUpdate.setNewValues(newValue);
    auditLogForProductUpdate.setStatus(true);
    return auditLogForProductUpdate;
  }

  private ProductLevel3Response getProductLevel3Response(boolean saved) {
    ProductLevel3Response productLevel3Response = new ProductLevel3Response();
    if (saved) {
      productLevel3Response.setProductName(PRODUCT_NAME);
      productLevel3Response.setDescription(PRODUCT_DESCRIPTION);
    } else {
      productLevel3Response.setProductName(PRODUCT_NAME_UPDATED);
      productLevel3Response.setDescription(PRODUCT_DESCRIPTION_UPDATED);
    }
    productLevel3Response.setItems(getProductItemLevel3ResponseList(saved));
    return productLevel3Response;
  }

  private ProductLevel3SummaryResponse getProductLevel3SummaryResponse(boolean saved) {
    ProductLevel3SummaryResponse response = new ProductLevel3SummaryResponse();
    if (saved) {
      response.setAvailableStockLevel2(PRODUCT_AVAILABLE_STOCK);
    } else {
      response.setAvailableStockLevel2(PRODUCT_AVAILABLE_STOCK_UPDATED);
    }
    response.setSynchronizeStock(saved);
    response.setOff2OnActiveFlag(saved);
    response.setMerchantSku(DEFAULT_BUSINESS_GDN_SKU);
    response.setPrices(getProductLevel3PriceResponse(saved));
    response.setViewConfigs(getProductLevel3ViewConfigResponse(saved));
    return response;
  }

  private List<ProductItemLevel3Response> getProductItemLevel3ResponseList(boolean saved) {
    ProductItemLevel3Response response = new ProductItemLevel3Response();
    if (saved) {
      response.setAvailableStockLevel2(PRODUCT_AVAILABLE_STOCK);
    } else {
      response.setAvailableStockLevel2(PRODUCT_AVAILABLE_STOCK_UPDATED);
    }
    response.setSynchronizeStock(saved);
    response.setOff2OnActiveFlag(saved);
    response.setMerchantSku(DEFAULT_BUSINESS_GDN_SKU);
    response.setPrices(getProductLevel3PriceResponse(saved));
    response.setViewConfigs(getProductLevel3ViewConfigResponse(saved));
    List<ProductItemLevel3Response> responseList = new ArrayList<>();
    responseList.add(response);
    return responseList;
  }

  private List<ProductLevel3PriceResponse> getProductLevel3PriceResponse(boolean saved) {
    ProductLevel3PriceResponse response = new ProductLevel3PriceResponse();
    if (saved) {
      response.setSalePrice(PRODUCT_PRICE);
      response.setPrice(PRODUCT_PRICE);
    } else {
      response.setSalePrice(PRODUCT_PRICE_UPDATED);
      response.setPrice(PRODUCT_PRICE_UPDATED);
    }
    List<ProductLevel3PriceResponse> responseList = new ArrayList<>();
    responseList.add(response);
    return responseList;
  }

  private List<ProductLevel3ViewConfigResponse> getProductLevel3ViewConfigResponse(boolean saved) {
    ProductLevel3ViewConfigResponse response = new ProductLevel3ViewConfigResponse();
    response.setBuyable(saved);
    response.setDisplay(saved);
    List<ProductLevel3ViewConfigResponse> responseList = new ArrayList<>();
    responseList.add(response);
    return responseList;

  }
}
