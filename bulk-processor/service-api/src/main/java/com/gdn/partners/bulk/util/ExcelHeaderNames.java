package com.gdn.partners.bulk.util;

import java.util.Collection;
import java.util.Set;

import com.google.common.collect.ImmutableSet;

public interface ExcelHeaderNames {
  String BLIBLI_SKU = "Blibli SKU";
  String NAMA_PRODUK = "Nama Produk";
  String SKU_CODE = "SKU Code";
  String SELLER_SKU = "Seller SKU";
  String HARGA_RP = "Harga (Rp)";
  String HARGA_PENJUALAN_RP = "Harga Penjualan (Rp)";
  String STOK = "Stok";
  String TOKO_GUDANG = "Toko/Gudang";
  String DITAMPILKAN = "Ditampilkan";
  String DAPAT_DIBELI = "Dapat Dibeli";
  String OFFLINE_TO_ONLINE = "Offline To Online";
  String WAREHOUSE_STOCK = "Warehouse Stock";
  String NAMA_PRODUCK = "Nama Produk";
  String URL_VIDEO = "Url Video";
  String TIPE_PENANGAN = "Tipe Penanganan";
  String HARGA = "Normal Harga (Rp)";
  String HARGA_PENJULAN = "Harga Penjualan (Rp)";
  String AVAILABLE_STOCK = "Available Stock";
  String MINIMUM_STOCK = "Minimum Stock";
  String DELIVERY_STATUS = "Delivery Status";
  String CNC_STATUS = "CNC";
  String KODE_PICKUP_POINT = "Kode Toko/Gudang";
  String PANJANG = "Panjang (cm)";
  String LEBAR = "Lebar (cm)";
  String TINGGI = "Tinggi (cm)";
  String BERAT = "Berat (gram)";
  String DESKRIPSI = "Deskripsi";
  String UNIQUE_SELLING_POINT = "Unique Selling Point";
  String BUYABLE = "Buyable";
  String MELALUI_PARTNER_LOGISTIK_BLIBLI = "Melalui partner logistik Blibli";
  String DIKIRIMKAN_OLEH_SELLER = "Dikirimkan oleh seller";
  String KEUNGGULAN_PRODUK = "Keunggulan produk";
  String BISA_DIBELI= "Bisa Dibeli";
  String STOK_TERSEDIA = "Stok Tersedia";
  String STOK_MINIMUM = "Stok Minimum";
  String STATUS_CNC="Click & Collect";

  String FOTO_PREFIX = "Foto-";

  //for international merchant

  String PRODUCT_NAME = "Product Name";
  String DESCRIPTION = "Description";
  String HANDLING_TYPE = "Handling Type";
  String PICKUP_POINT_CODE = "Shop/Warehouse Code";
  String LENGTH = "Length (cm)";
  String WIDTH = "Width (cm)";
  String HEIGHT = "Height (cm)";
  String WEIGHT = "Weight (gram)";
  String PRICE = "Normal Price (Rp)";
  String SELLING_PRICE = "Selling Price (Rp)";
  String IMAGE_PREFIX = "Image-";
  String MODEL_EAN_UPC = "Model/EAN/UPC";
  String PARENT = "Parent";
  String THROUGH_BLIBLI_LOGISTIC_PARTNER = "Through Blibli logistic partner";
  String SHIPPED_BY_SELLER = "Shipped by seller";
  String BOPIS = "Bopis";
  String BLIBLI_PRODUCT_SKU = "Blibli Product SKU";
  String IN_STORE_HEADER = "Instore";
  String PARENT_PRODUCT_NAME = "Parent Product name";
  String AMPHI_SKU_STATUS = "SKU status (0= offline, 1 = Online, 2=Teaser, 3 = B2B)";
  String EXTERNAL_SKU_STATUS = "SKU status (0= offline, 1 = Online)";
  String SKU_STATUS_OFFLINE = "0";
  String SKU_STATUS_ONLINE = "1";
  String SKU_STATUS_TEASER = "2";
  String SKU_STATUS_B2B = "3";
  String IN_STORE = "Instore";
  String PRODUCT_INFO = "Info Produk";
  String VARIENT = "Varian";
  String IMAGES = "Gambar & Video";
  String SHIPMENT_INFO = "Info pengiriman";
  String PRICE_AND_STOCK = "Harga & Stok";
  String SKU_VISIBILITY = "SKU visibility";
  String SKU_VISIBILITY_ID = "Tampilan SKU";
  String OTHER_ATTRIBUTES = "Atribut lainnya";
  String RECOMMENDED_ATTRIBUTES = "Rekomendasi atribut";
  String KEYWORD = "Keyword";
  String KEYWORD_TYPE = "Keyword Type";
  String KEYWORD_ACTION = "Keyword Action";
  String REASON = "Reason (only for Auto Reject and Auto Revision)";
  String CATEGORY_CHANGE_TO = "Change Category to";
  String APPLICABLE_FOR_ALL = "Keyword applicable for All Categories";
  String APPLICABLE_FOR_THESE = "Applicable for these categories only";
  String NOT_APPLICABLE = "Not Applicable for these categories only";
  String YES = "Yes";
  String NO = "No";
  String CHANGE_CATGEORY_TO = "Change category to";
  String DELETE_KEYWORD_FOR_ALL_CATEGORY = "Delete Keyword across all categories";
  String DELETE_KEYWORD_ACROSS_THESE_CATEGORY = "Delete Keyword across these categories";
  String DO_NOT_DELETE_ACROSS_THESE_CATEGORY = "Do not delete Keywords across these categories";
  String BRAND_AUTH_SELLER_CODE = "Seller Code (Mandatory)";
  String BRAND_AUTH_SELLER_NAME = "Seller Name";
  String BRAND_AUTH_BRAND_CODE = "Brand Code (Mandatory)";
  String BRAND_NAME = "Brand Name";
  String AUTH_START_DATE = "Authorisation Start date(DD/MM/YYYY)";
  String AUTH_END_DATE = "Authorisation End date(DD/MM/YYYY)";
  String BFB_BASE_PRICE = "BfB Base Price";
  String BFB_MANAGED_INFO = "BfB Managed";
  String BFB_STATUS= "BfB Status";
  String BFB_BASE_PRICE_ID = "BfB Harga dasar";
  String BFB_MANAGED_INFO_ID = "BfB Diatur";
  Collection<String> RESTRICTED_KEYWORD_UPSERT_TEMPLATE_HEADER =
      ImmutableSet.of(KEYWORD, KEYWORD_TYPE, KEYWORD_ACTION, REASON, CATEGORY_CHANGE_TO, APPLICABLE_FOR_ALL,
          APPLICABLE_FOR_THESE, NOT_APPLICABLE);
  Collection<String> RESTRICTED_KEYWORD_DELETE_TEMPLATE_HEADER =
      ImmutableSet.of(KEYWORD, DELETE_KEYWORD_FOR_ALL_CATEGORY, DELETE_KEYWORD_ACROSS_THESE_CATEGORY,
          DO_NOT_DELETE_ACROSS_THESE_CATEGORY);
  Collection<String> APPLICABLE_FOR_ALL_LIST = ImmutableSet.of(YES, NO);
  Collection<String> KEYWORD_TYPE_VALUES =
      ImmutableSet.of("Used Product", "Competitor's name", "Doctor's prescription", "Illegal drugs", "Fake product",
          "Cigarette", "Google Voucher", "Gambling", "Others");
  Collection<String> KEYWORD_ACTION_VALUES =
      ImmutableSet.of("Straight rejection", "Need revision", "No action", "Change category to");
  Collection<String> KEYWORD_REASON_ACTION_VALUES = ImmutableSet.of("Straight rejection", "Need revision");
  Collection<String> BRAND_AUTH_ADD_TEMPLATE_HEADER =
      Set.of(BRAND_AUTH_SELLER_CODE, BRAND_AUTH_SELLER_NAME, BRAND_AUTH_BRAND_CODE, BRAND_NAME,
          AUTH_START_DATE, AUTH_END_DATE);
  Collection<String> BRAND_AUTH_DELETE_TEMPLATE_HEADER =
      Set.of(BRAND_AUTH_SELLER_CODE, BRAND_AUTH_SELLER_NAME, BRAND_AUTH_BRAND_CODE, BRAND_NAME);
  String BUNDLE_INFO = "Bundle Info";
  String VARIANT_IMAGE_INFO_EN = "You can add photo for each variant.";
  String VARIANT_IMAGE_INFO_ID = "Anda bisa menambahkan foto untuk tiap varian.";
  String SHIPPED_BY_BLIBLI_ID = "Dikirim oleh blibli";
  String SHIPPED_BY_SELLER_ID = "Dikirim oleh seller";
  String BOPIS_ID = "Produk non-fisik";
}
