package com.gdn.partners.bulk.util;

import java.util.Arrays;
import java.util.Collection;

public interface StoreCopyConstants {

  //Excel Header
  String BLIBLI_PRODUCT_SKU = "Blibli Product SKU";
  String PARENT_PRODUCT_NAME = "Parent Product name";
  String BLIBLI_SKU = "Blibli SKU";
  String NAMA_PRODUK = "Nama Produk";
  String SKU_CODE = "SKU Code";
  String PRODUCT_CODE = "Product Code";
  String COPIED_PRODUCT_NAME = "Copied Product name (Please use same name for all variants)";
  String SELLER_SKU = "Seller SKU";
  String HATGA = "Harga (Rp)";
  String HARGA_PENJUALAN = "Harga Penjualan (Rp)";
  String STOK = "Stok";
  String TOKO_GUDANG = "Toko/Gudang";
  String MINIMUM_STOCK = "Minimum stock";
  String SKU_STATUS = "SKU status (0= offline, 1 = Online, 2=Teaser, 3 = B2B)";
  String SHIPPING_TYPE = "Shipping type";

  //Error messages
  String FILE_IS_EMPTY = "File is empty";
  String HEADER_VALIDATION_FAILED = "Header validation failed";
  String HEADER_SIZE_VALIDATION_FAILED = "Header validation failed";
  Collection<String> TEMPLATE_HEADER =
      Arrays.asList(BLIBLI_PRODUCT_SKU, PARENT_PRODUCT_NAME, BLIBLI_SKU, NAMA_PRODUK, SKU_CODE, PRODUCT_CODE,
          COPIED_PRODUCT_NAME, SELLER_SKU, HATGA, HARGA_PENJUALAN, STOK, TOKO_GUDANG, MINIMUM_STOCK, SKU_STATUS, SHIPPING_TYPE);
  String PICKUP_POINT_CODE_SHIPPING_TYPE_EMPTY = "Pickup point code or Shipping type is empty";
  //Store copy request fields
  String FIELD_PRODUCT_CODE = "productCode";
  String FIELD_PRODUCT_SKU = "productSku";
  String FIELD_PRODUCT_NAME = "productName";
  String FIELD_ITEM_SKU = "itemSku";
  String FIELD_ITEM_CODE = "itemCode";
  String FIELD_COPY_PRODUCT_NAME = "copyProductName";
  String FIELD_SELLER_SKU = "sellerSku";
  String FIELD_LIST_PRICE = "listPrice";
  String FIELD_OFFER_PRICE = "offerPrice";
  String FIELD_STOCK = "stock";
  String FIELD_MINIMUM_STOCK = "minimumStock";
  String FIELD_SHIPPING_TYPE = "shippingType";
  String FIELD_PICKUP_POINT_CODE = "pickupPointCode";
  String FIELD_STATUS = "status";
  String ERROR_HEADER = "Notes";
  String CHANNEL_ID = "web";
  String CLIENT_ID = "'x-bulk";
}
