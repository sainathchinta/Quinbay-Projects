package com.gdn.mta.bulk.dto;

import java.text.DecimalFormat;
import java.util.Map;

import com.google.common.collect.ImmutableMap;

public final class ProductUpdateErrorMessages {

  public static final String CHANGE_SELLING_PRICE = "Ubah harga jual";
  public static final String CHANGE_NORMAL_PRICE = "Ubah harga Normal";
  public static final String CHANGE_STOCK_VALUE = "Ubah Stock";
  public static final String CHANGE_SYNC_STOCK_VALUE = "Synchronize Stock";
  public static final String CHANGE_BUYABLE = "Ubah Dapat dibeli";
  public static final String CHANGE_DISPLAYABLE = "Ubah Ditampilkan";
  public static final String CHANGE_PRODUCT_NAME = "Ubah nama produk";
  public static final String CHANGE_PRODUCT_DESCRIPTION = "Ubah deskripsi produk";
  public static final String CHANGE_OFF_2_ON_ACTIVE_FLAG = "Ubah offline to online";
  public static final String CHANGE_SELLER_SKU = "Seller Sku";
  public static final String WHOLE_SALE_TURN_OFF = "Wholesale price has been turned off";
  public static final String WHOLE_SALE_UPDATED = "Wholesale price has been updated";
  public static final String MAXIMUM_PRODUCT_ERROR_EN =
    "The number of products in uploaded excel have crossed the max limit. Please upload the file "
      + "again with number of products less than value : ";
  public static final String MAXIMUM_ROW_ERROR_EN =
    "The number of rows in uploaded excel have crossed the max limit. Please upload the file "
      + "again with number of rows less than value : ";
  public static final String MAXIMUM_PRODUCT_ERROR_IN =
    "Jumlah produk dalam Excel yang diunggah telah melewati batas maksimum. Harap unggah file "
      + "kembali dengan jumlah produk kurang dari :";
  public static final String MAXIMUM_ROW_ERROR_IN =
    "Jumlah baris dalam Excel yang diunggah telah melewati batas maksimum. Harap unggah file "
      + "kembali dengan jumlah baris kurang dari :";

  public static final DecimalFormat DECIMAL_FORMAT = new DecimalFormat("#");
  public static final Map<Boolean, String> SUCCESS_MSG = ImmutableMap.<Boolean, String>builder().put(true, "Ya")
      .put(false, "Tidak").build();
  public static final String CAMPAIGN_UPDATE_FAILED = "Add products to campaign failed";
  public static final String KEYWORD_INVALID = "Kata pencarian tidak ditemukan.";
  public static final String KEYWORD_TYPE_INVALID = "Tipe kata pencarian tidak valid.";
  public static final String KEYWORD_ACTION_INVALID = "Tindakan untuk kata pencarian tidak valid.";
  public static final String REASON_INVALID = "Reason Should not be empty";
  public static final String DESTINATION_CATEGORY_EMPTY =
      "Destination category should not be empty for category change";
  public static final String APPLICABLE_FOR_ALL_INVALID = "Applicable for all should be either Yes or No";
  public static final String APPLICABLE_FOR_THESE_INVALID = "Applicable to these categories should not be empty";
  public static final String DELETE_FOR_ALL_INVALID = "Delete Keyword across all categories should be either Yes or No";
  public static final String DELETE_FOR_THESE_INVALID = "Delete Keyword across these categories should not be empty";
  public static final String SELLER_CODE_EMPTY = "Seller Code missing";
  public static final String BRAND_CODE_EMPTY = "Brand Code missing";
  public static final String DUPLICATE_BRAND_CODE_AND_SELLER_CODE = "Duplicate brand code and seller code entry";
  public static final String INVALID_DATE_FORMAT = "Brand authorization invalid date format.";
  public static final String PRODUCT_CODE_EMPTY = "Product Code empty";
  public static final String REASON_EMPTY = "Reason is empty";
  public static final String COMMENT_EMPTY = "Comment is empty";
  public static final String FIRST_MASTER_SKU_EMPTY = "Master SKU 1 wajib diiisi";
  public static final String SECOND_MASTER_SKU_EMPTY = "Master SKU 2 wajib diiisi";
  public static final String REVIEW_ACTION_EMPTY = "Wajib memilih tindakan.";
  public static final String INVALID_REVIEW_ACTION = "Tindakan Peninjauan Tidak Valid";
  public static final String DUPLICATE_REVIEW_ACTION_FOUND = "Gagal melakukan tindakan ini karena sudah ada data yang sama";
  public static final String FIRST_ANCHOR_SKU_AND_SECOND_ANCHOR_SKU_IS_SAME = "Master SKU 1 & 2 wajib berbeda.";
  public static final String BULK_PRICE_UPDATE_NOT_ALLOWED_FOR_SELLER = "Bulk price update not allowed for seller";
  public static final String INVALID_GENERIC_TEMPLATE_FILE_TYPE = "Generic template file type is invalid";
  public static final String INVALID_ADD_PICKUP_POINT_REQUEST =
    "Invalid add pickup point request for non multi pickup point seller";
}
