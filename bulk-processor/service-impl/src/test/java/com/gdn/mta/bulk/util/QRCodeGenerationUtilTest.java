package com.gdn.mta.bulk.util;

import java.util.Map;
import java.util.TreeMap;

import com.gdn.mta.bulk.AllowedQRGenerationType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.partners.bulk.util.Constant;

public class QRCodeGenerationUtilTest {

  private Map<Integer, String> headerRow = new TreeMap<>();

  @Test
  public void validateHeadersForExcelUpload_fulfillmentTypeTest() {
    headerRow.put(0, ExcelHeaderNames.FULFILLMENT_TYPE);
    Assertions.assertThrows(RuntimeException.class,
        () -> QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, null, false));
  }

  @Test
  public void validateHeadersForExcelUpload_productTypeTest() {
    headerRow.put(0, ExcelHeaderNames.FULFILLMENT_TYPE);
    Assertions.assertThrows(RuntimeException.class,
        () -> QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, Constant.PRODUCT,
            true));
  }

  @Test
  public void validateHeadersForExcelUpload_itemTypeTest() {
    headerRow.put(0, ExcelHeaderNames.FULFILLMENT_TYPE);
    Assertions.assertThrows(RuntimeException.class,
        () -> QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, Constant.ITEM, true));
  }

  @Test
  public void validateHeadersForExcelUpload_itemPickupPointType_itemSkuOnlyTest() {
    headerRow.put(0, ExcelHeaderNames.ITEM_SKU);
    Assertions.assertThrows(RuntimeException.class,
        () -> QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow,
            Constant.ITEM_PICKUP_POINT, true));
  }

  @Test
  public void validateHeadersForExcelUpload_itemPickupPointType_ppCodeOnlyTest() {
    headerRow.put(0, ExcelHeaderNames.PICKUP_POINT_CODE_EN);
    Assertions.assertThrows(RuntimeException.class,
        () -> QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow,
            Constant.ITEM_PICKUP_POINT, true));
  }

  @Test
  public void validateHeadersForExcelUpload_fulfillmentType_happyFlowTest() {
    headerRow.put(0, ExcelHeaderNames.FULFILLMENT_TYPE);
    QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, null, true);
  }

  @Test
  public void validateHeadersForExcelUpload_addToBag_success() {
    headerRow.put(0, ExcelHeaderNames.ITEM_SKU);
    headerRow.put(1, ExcelHeaderNames.PICKUP_POINT_CODE_EN);
    QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, AllowedQRGenerationType.ADD_TO_BAG.getValue(), true);
  }

  @Test
  public void validateHeadersForExcelUpload_fulfillmentTypeFalse_happyFlowTest() {
    QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, null, false);
  }

  @Test
  public void validateHeadersForExcelUpload_productType_happyFlowTest() {
    headerRow.put(0, ExcelHeaderNames.PRODUCT_SKU);
    QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, Constant.PRODUCT, true);
  }

  @Test
  public void validateHeadersForExcelUpload_itemType_happyFlowTest() {
    headerRow.put(0, ExcelHeaderNames.ITEM_SKU);
    QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, Constant.ITEM, true);
  }

  @Test
  public void validateHeadersForExcelUpload_itemPickupPointType_happyFlowTest() {
    headerRow.put(0, ExcelHeaderNames.ITEM_SKU);
    headerRow.put(1, ExcelHeaderNames.PICKUP_POINT_CODE_EN);
    QRCodeGenerationUtil.validateHeadersForExcelUpload(headerRow, Constant.ITEM_PICKUP_POINT, true);
  }
}
