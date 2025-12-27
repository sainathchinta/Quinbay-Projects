package com.gdn.mta.bulk.models;

/**
 * Created by priyanka on 16/02/17.
 */
public final class ExcelHeaderNames {
  public static final String URL_VIDEO = "Url Video";
  public static final String SELLER_SKU = "Seller SKU";
  public static final String TIPE_PENANGAN = "Tipe Penanganan";
  public static final String HARGA = "Harga (Rp)";
  public static final String KODE_PICKUP_POINT = "Kode Toko/Gudang";
  public static final String PANJANG = "Panjang (cm)";
  public static final String LEBAR = "Lebar (cm)";
  public static final String TINGGI = "Tinggi (cm)";
  public static final String BERAT = "Berat (gram)";
  public static final String DESKRIPSI = "Deskripsi";
  public static final String UNIQUE_SELLING_POINT = "Unique Selling Point";
  public static final String BUYABLE = "Buyable";
  public static final String STATUS_PENGIRIMAN="Status/Pengiriman";
  public static final String CNC = "Click & Collect";

  //for international merchant
  public static final String PRODUCT_NAME = "Product Name";
  public static final String DESCRIPTION = "Description";

  public static final String HANDLING_TYPE = "Handling Type";
  public static final String PICKUP_POINT_CODE = "Shop/Warehouse Code";
  public static final String LENGTH = "Length (cm)";
  public static final String WIDTH = "Width (cm)";
  public static final String HEIGHT = "Height (cm)";
  public static final String WEIGHT = "Weight (gram)";

  public static final String PRICE = "Price (Rp)";
  public static final String SELLING_PRICE = "Selling Price (Rp)";

  public static final String DELIVERY_STATUS = "Status/Shipping";

  //upsert header file names
  public static final String PICKUP_POINT_CODE_IN = "Kode alamat pengambilan";
  public static final String BLIBLI_SKU_IN = "SKU Blibli (SKU item)";
  public static final String LIST_PRICE_IN = "Harga normal";
  public static final String OFFER_PRICE_IN = "Harga jual";
  public static final String STOCK_IN = "Stok";
  public static final String DELIVERY_STATUS_IN = "Status/pengiriman(0 = Offline, 1 = Online)";
  public static final String CNC_STATUS_IN = "Click & Collect(0= Off, 1 = On)";
  public static final String BLIBLI_SKU_EN = "Blibli SKU (Item SKU)";
  public static final String PICKUP_POINT_CODE_EN = "Pickup point code";
  public static final String LIST_PRICE_EN = "Normal Price (Rp)";
  public static final String OFFER_PRICE_EN = "Selling Price (Rp)";
  public static final String STOCK_EN = "Stock";
  public static final String DELIVERY_STATUS_EN = "Status/Shipping(0 = Offline, 1 = Online)";
  public static final String CNC_STATUS_EN = "Click & Collect(0= Off, 1 = On)";
  public static final String DELIVERY_STATUS_IN_CNC_1P = "Pengiriman(0 = Offline, 1 = Online)";
  public static final String DELIVERY_STATUS_EN_CNC_1P = "Delivery(0 = Offline, 1 = Online)";

  //QR Code constants
  public static final String ITEM_SKU = "Item SKU";
  public static final String PRODUCT_SKU = "Product SKU";
  public static final String FULFILLMENT_TYPE = "Fulfilment Type";
  public static final String PICKUP_POINT_NAME = "Pickup point Name";
  public static final String ERROR_MESSAGE = "Error Message";
  public static final String INSTORE_HEADER = "Instore Status";


  public static final String VARIASI_HEADER = "Nama Variasi";
  public static final String EXTERNAL_CATEGORY = "External category";

  private ExcelHeaderNames(){
  }
}
