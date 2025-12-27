package com.gdn.mta.bulk.models;

import java.util.Map;

/**
 * Created by priyanka on 21/02/17.
 */
public final class BulkUploadFileHeaders {
  public static final int NAMA_PRODUCK = 0;
  public static final int EAN_UPC = 1;
  public static final int MERCHANT_SKU = 2;
  public static final int DESCRIPTION = 3;
  public static final int UNIQUE_SELLING_POINT = 4;
  public static final int BUYABLE = 5;
  public static final int TIPE_PENANGANAN = 15;
  public static final int HARGA = 21;
  public static final int HARGA_PENJUALAN = 22;
  public static final int AVAILABLE_STOCK = 23;
  public static final int MINIMUM_STOCK = 24;
  public static final int PICKUP_POINT_CODE = 16;
  public static final int LENGTH_PANJANG = 17;
  public static final int WIDTH_LEBAR = 18;
  public static final int HEIGHT_TINGGI = 19;
  public static final int WEIGHT_BERAT = 20;

  public static final int VARIANT_START_INDEX = 7;
  public static final int IMAGES_END_INDEX = 9;
  public static final int SHIPMENT_END_INDEX = 6;
  public static final int PRICE_STOCK_END_INDEX = 4;

  public static final int IMAGES_END_INDEX_CN = 8;
  public static final int PARENT_COLUMN_COUNT = 1;

  public static final String PRODUCT_INFO = "productInfo";
  public static final String VARIANT = "variant";
  public static final String IMAGES = "images";
  public static final String FEATURES = "features";
  public static final String ATTRIBUTES = "Attributes";

  public static final String SHIPMENT_INFO = "shipmentInfo";
  public static final String PRICE_AND_STOCK = "priceAndStock";
  public static final String SKU_VISIBILITY = "skuVisibility";

  private BulkUploadFileHeaders() {
  }

  public static int getUrlVideo(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(BulkUploadFileHeaders.SHIPMENT_INFO) - 1;
  }

  public static int getLength(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(BulkUploadFileHeaders.SHIPMENT_INFO) + 2;
  }

  public static int getWidth(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(BulkUploadFileHeaders.SHIPMENT_INFO) + 3;
  }

  public static int getHeight(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(BulkUploadFileHeaders.SHIPMENT_INFO) + 4;
  }

  public static int getWeight(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(BulkUploadFileHeaders.SHIPMENT_INFO) + 5;
  }

  // -1 for parent column
  public static int getVariantEnd(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(BulkUploadFileHeaders.IMAGES) - 1;
  }

  public static int getNormalPrice(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(PRICE_AND_STOCK);
  }

  public static int getSellingPrice(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(PRICE_AND_STOCK) + 1;
  }

  public static int getAvailableStock(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(PRICE_AND_STOCK) + 2;
  }

  public static int getMinimumStock(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(PRICE_AND_STOCK) + 3;
  }

  public static int getProductType(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(SHIPMENT_INFO);
  }

  public static int getPickupPointCode(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(SHIPMENT_INFO) + 1;
  }

  public static int getDeliveryStatus(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(SKU_VISIBILITY);
  }

  public static int getCncStatus(Map<String, Integer> indexColumnGroups) {
    return indexColumnGroups.get(SKU_VISIBILITY) + 1;
  }

}
