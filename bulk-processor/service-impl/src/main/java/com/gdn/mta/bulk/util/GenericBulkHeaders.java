package com.gdn.mta.bulk.util;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class GenericBulkHeaders {

  public static final String CATEGORY = "Kategori*";
  public static final String PRODUCT_NAME = "Nama Produk*";
  public static final String UPC = "Model/EAN/UPC";
  public static final String SELLER_SKU = "Seller SKU";
  public static final String DESCRIPTION = "Deskripsi*";
  public static final String USP = "Keunggulan produk";
  public static final String BRAND = "Merek*";
  public static final String WARNA = "Warna";
  public static final String FAMILY_COLOUR = "Family Colour";
  public static final String UKURAN = "Ukuran";
  public static final String VARANSI = "Variasi";
  public static final String PARENT = "Parent";
  public static final String IMAGE = "Foto";
  public static final String IMAGE_1 = "Foto-1*";
  public static final String IMAGE_2 = "Foto-2";
  public static final String IMAGE_3 = "Foto-3";
  public static final String IMAGE_4 = "Foto-4";
  public static final String IMAGE_5 = "Foto-5";
  public static final String IMAGE6 = "Foto-6";
  public static final String IMAGE_7 = "Foto-7";
  public static final String IMAGE_8 = "Foto-8";
  public static final String URL_VIDEO = "Url Video";
  public static final String PRODUCT_TYPE = "Tipe Penanganan*";
  public static final String PRODUCT_TYPE_NAME = "Tipe Penanganan";
  public static final String PICKUP_POINT = "Kode Toko/Gudang*";
  public static final String LENGTH = "Panjang (cm)*";
  public static final String WIDTH = "Lebar (cm)*";
  public static final String HEIGTH = "Tinggi (cm)*";
  public static final String WEIGHT = "Berat (gram)*";
  public static final String SALE_PRICE = "Harga Penjualan (Rp)*";
  public static final String STOCK = "Available Stock*";
  public static final String DELIVERY_STATUS = "Delivery Status*";
  public static final String CNC = "CNC*";
  public static final String ATTRIBUTE_NAME = "Pilih Attribut";
  public static final String ATTRIBUTE_VALUE = "Pilih value";
  public static final String PRODUCT_INFORMATION = "Product Information";
  public static final String BFB_BASE_PRICE = "BfB Base Price";
  public static final String BFB_MANAGED_INFO = "BfB Managed";
  public static final String BFB_STATUS= "BfB Status";
  public static final String BFB_BASE_PRICE_ID = "BfB Harga dasar";
  public static final String BFB_MANAGED_INFO_ID = "BfB Diatur";
  public static final String VARIANT_IMAGE_EN = "Variant photo";
  public static final String VARIANT_IMAGE_ID = "Foto varian";
  public static final String VARIANT_IMAGE_INFO_EN = "You can add photo for each variant.";
  public static final String VARIANT_IMAGE_INFO_ID = "Anda bisa menambahkan foto untuk tiap varian.";
  public static final String INSTORE = "Instore Status*";
  public static final String ERROR_COLUMN = "Error";


  public static final List<String> HEADER_LIST_MPP_SWITCH_ENABLED_CNC =
      ImmutableList.<String>builder().add(CATEGORY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(PRODUCT_NAME)
          .add(UPC).add(SELLER_SKU).add(DESCRIPTION).add(USP).add(BRAND).add(WARNA).add(FAMILY_COLOUR).add(UKURAN)
          .add(VARANSI).add(PARENT).add(VARIANT_IMAGE_ID).add(IMAGE_1).add(IMAGE_2).add(IMAGE_3).add(IMAGE_4)
          .add(IMAGE_5).add(IMAGE6).add(IMAGE_7).add(URL_VIDEO).add(PRODUCT_TYPE).add(PICKUP_POINT).add(LENGTH)
          .add(WIDTH).add(HEIGTH).add(WEIGHT).add(SALE_PRICE).add(STOCK).add(DELIVERY_STATUS).add(CNC)
          .add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME)
          .add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE)
          .build();


  public static final List<String> HEADER_LIST_MPP_SWITCH_ENABLED_NON_CNC =
      ImmutableList.<String>builder().add(CATEGORY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(PRODUCT_NAME)
          .add(UPC).add(SELLER_SKU).add(DESCRIPTION).add(USP).add(BRAND).add(WARNA).add(FAMILY_COLOUR).add(UKURAN)
          .add(VARANSI).add(PARENT).add(VARIANT_IMAGE_ID).add(IMAGE_1).add(IMAGE_2).add(IMAGE_3).add(IMAGE_4).add(IMAGE_5).add(IMAGE6)
          .add(IMAGE_7).add(URL_VIDEO).add(PRODUCT_TYPE).add(PICKUP_POINT).add(LENGTH).add(WIDTH)
          .add(HEIGTH).add(WEIGHT).add(SALE_PRICE).add(STOCK).add(DELIVERY_STATUS).add(ATTRIBUTE_NAME)
          .add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE)
          .add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).build();

  public static final List<String> HEADER_LIST_CNC_BFB =
      ImmutableList.<String>builder().add(CATEGORY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(PRODUCT_NAME)
          .add(UPC).add(SELLER_SKU).add(DESCRIPTION).add(USP).add(BRAND).add(WARNA).add(FAMILY_COLOUR).add(UKURAN)
          .add(VARANSI).add(PARENT).add(VARIANT_IMAGE_ID).add(IMAGE_1).add(IMAGE_2).add(IMAGE_3).add(IMAGE_4)
          .add(IMAGE_5).add(IMAGE6).add(IMAGE_7).add(URL_VIDEO).add(PRODUCT_TYPE).add(PICKUP_POINT).add(LENGTH)
          .add(WIDTH).add(HEIGTH).add(WEIGHT).add(SALE_PRICE).add(STOCK).add(BFB_BASE_PRICE_ID).add(BFB_MANAGED_INFO_ID)
          .add(BFB_STATUS).add(DELIVERY_STATUS).add(CNC).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME)
          .add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE)
          .add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).build();


  public static final List<String> HEADER_LIST_NON_CNC_BFB =
      ImmutableList.<String>builder().add(CATEGORY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(PRODUCT_NAME)
          .add(UPC).add(SELLER_SKU).add(DESCRIPTION).add(USP).add(BRAND).add(WARNA).add(FAMILY_COLOUR).add(UKURAN)
          .add(VARANSI).add(PARENT).add(VARIANT_IMAGE_ID).add(IMAGE_1).add(IMAGE_2).add(IMAGE_3).add(IMAGE_4)
          .add(IMAGE_5).add(IMAGE6).add(IMAGE_7).add(URL_VIDEO).add(PRODUCT_TYPE).add(PICKUP_POINT).add(LENGTH)
          .add(WIDTH).add(HEIGTH).add(WEIGHT).add(SALE_PRICE).add(STOCK).add(BFB_BASE_PRICE_ID).add(BFB_MANAGED_INFO_ID)
          .add(BFB_STATUS).add(DELIVERY_STATUS).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME)
          .add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE)
          .add(ATTRIBUTE_NAME).add(ATTRIBUTE_VALUE).build();

  public static final String CATEGORY_EN = "Category";
  public static final String PRODUCT_NAME_EN = "Product name*";
  public static final String UPC_EN = "Model/EAN/UPC";
  public static final String SELLER_SKU_EN = "Seller SKU";
  public static final String DESCRIPTION_EN = "Description*";
  public static final String USP_EN = "Unique Selling Point";
  public static final String BRAND_EN = "Brand*";
  public static final String WARNA_EN = "Colour";
  public static final String FAMILY_COLOUR_EN = "Family Colour";
  public static final String UKURAN_EN = "Size";
  public static final String VARANSI_EN = "Variant";
  public static final String PARENT_EN = "Parent";
  public static final String IMAGE_EN = "Image";
  public static final String IMAGE_1_EN = "Image-1*";
  public static final String IMAGE_2_EN = "Image-2";
  public static final String IMAGE_3_EN = "Image-3";
  public static final String IMAGE_4_EN = "Image-4";
  public static final String IMAGE_5_EN = "Image-5";
  public static final String IMAGE_6_EN = "Image-6";
  public static final String IMAGE_7_EN = "Image-7";
  public static final String IMAGE_8_EN = "Image-8";
  public static final String URL_VIDEO_EN = "Url Video";
  public static final String PRODUCT_TYPE_EN = "Handling Type*";
  public static final String PRODUCT_TYPE_NAME_EN = "Handling Type";
  public static final String PICKUP_POINT_EN = "Shop / Warehouse Code *";
  public static final String LENGTH_EN = "Length (cm) *";
  public static final String WIDTH_EN = "Width (cm) *";
  public static final String HEIGTH_EN = "Height (cm) *";
  public static final String WEIGHT_EN = "Weight (gram) *";
  public static final String SALE_PRICE_EN = "Sales Price (Rp)*";
  public static final String STOCK_EN = "Available Stock*";
  public static final String DELIVERY_STATUS_EN = "Delivery Status*";
  public static final String CNC_EN = "CNC*";
  public static final String ATTRIBUTE_NAME_EN = "Select Attribute ";
  public static final String ATTRIBUTE_VALUE_EN = "Select Value ";
  public static final String BFB_MANAGED_HEADER_ID = "BfB Diatur*";
  public static final String BFB_MANAGED_HEADER_EN = "BfB Managed*";
  public static final String BFB_STATUS_HEADER = "BfB Status*";
  public static final String BFB_BASE_PRICE_HEADER_EN = "Bfb Base Price*";
  public static final String BFB_BASE_PRICE_HEADER_ID = "BfB Harga dasar*";
  public static final String CHILD_SKU_EN = "Child SKU";
  public static final String QUANTITY_EN = "Quantity";
  public static final String CHILD_SKU_ID = "SKU child";
  public static final String QUANTITY_ID = "Jumlah";
  public static final String FOTO_PREFIX = "Foto-";
  public static final String IMAGE_PREFIX = "Image-";
  public static final String GENERATED_ITEM_NAME = "generatedItemName";
  public static final String ROW_NUMBER = "rowNumber";
  public static final String CATEGORY_TREE_HIERARCHY = "categoryTreeHierarchy";
  public static final String CN_CATEGORY_ID= "cnCategoryId";
  public static final String CN_CATEGORY_CODE_PARENT_CODE_IDENTIFIER = "cnCategoryCodeAndParentCodeIdentifier";
  public static final String BULK_PROCESS_NOTES = "bulkProcessNotes";

  public static final List<String> HEADER_LIST_EN_MPP_SWITCH_ENABLED_CNC =
      ImmutableList.<String>builder().add(CATEGORY_EN).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(PRODUCT_NAME_EN).add(UPC_EN).add(SELLER_SKU_EN).add(DESCRIPTION_EN).add(USP_EN).add(BRAND_EN)
          .add(WARNA_EN).add(FAMILY_COLOUR_EN).add(UKURAN_EN).add(VARANSI_EN).add(PARENT_EN).add(VARIANT_IMAGE_EN)
          .add(IMAGE_1_EN).add(IMAGE_2_EN).add(IMAGE_3_EN).add(IMAGE_4_EN).add(IMAGE_5_EN).add(IMAGE_6_EN)
          .add(IMAGE_7_EN).add(URL_VIDEO_EN).add(PRODUCT_TYPE_EN).add(PICKUP_POINT_EN).add(LENGTH_EN).add(WIDTH_EN)
          .add(HEIGTH_EN).add(WEIGHT_EN).add(SALE_PRICE_EN).add(STOCK_EN).add(DELIVERY_STATUS_EN).add(CNC_EN)
          .add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN)
          .add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN)
          .add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).build();

  public static final List<String> HEADER_LIST_EN_MPP_SWITCH_ENABLED_NON_CNC =
      ImmutableList.<String>builder().add(CATEGORY_EN).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(PRODUCT_NAME_EN).add(UPC_EN).add(SELLER_SKU_EN).add(DESCRIPTION_EN).add(USP_EN).add(BRAND_EN)
          .add(WARNA_EN).add(FAMILY_COLOUR_EN).add(UKURAN_EN).add(VARANSI_EN).add(PARENT_EN).add(VARIANT_IMAGE_EN)
          .add(IMAGE_1_EN).add(IMAGE_2_EN).add(IMAGE_3_EN).add(IMAGE_4_EN).add(IMAGE_5_EN).add(IMAGE_6_EN)
          .add(IMAGE_7_EN).add(URL_VIDEO_EN).add(PRODUCT_TYPE_EN).add(PICKUP_POINT_EN).add(LENGTH_EN).add(WIDTH_EN)
          .add(HEIGTH_EN).add(WEIGHT_EN).add(SALE_PRICE_EN).add(STOCK_EN).add(DELIVERY_STATUS_EN).add(ATTRIBUTE_NAME_EN)
          .add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN)
          .add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN)
          .add(ATTRIBUTE_VALUE_EN).build();

  public static final List<String> HEADER_LIST_EN_NON_CNC_BFB =
      ImmutableList.<String>builder().add(CATEGORY_EN).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(PRODUCT_NAME_EN).add(UPC_EN).add(SELLER_SKU_EN).add(DESCRIPTION_EN).add(USP_EN).add(BRAND_EN)
          .add(WARNA_EN).add(FAMILY_COLOUR_EN).add(UKURAN_EN).add(VARANSI_EN).add(PARENT_EN).add(VARIANT_IMAGE_EN)
          .add(IMAGE_1_EN).add(IMAGE_2_EN).add(IMAGE_3_EN).add(IMAGE_4_EN).add(IMAGE_5_EN).add(IMAGE_6_EN)
          .add(IMAGE_7_EN).add(URL_VIDEO_EN).add(PRODUCT_TYPE_EN).add(PICKUP_POINT_EN).add(LENGTH_EN).add(WIDTH_EN)
          .add(HEIGTH_EN).add(WEIGHT_EN).add(SALE_PRICE_EN).add(STOCK_EN).add(BFB_BASE_PRICE).add(BFB_MANAGED_INFO)
          .add(BFB_STATUS).add(DELIVERY_STATUS_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN)
          .add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN)
          .add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).build();

  public static final List<String> HEADER_LIST_EN_CNC_BFB =
      ImmutableList.<String>builder().add(CATEGORY_EN).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(PRODUCT_NAME_EN).add(UPC_EN).add(SELLER_SKU_EN).add(DESCRIPTION_EN).add(USP_EN).add(BRAND_EN)
          .add(WARNA_EN).add(FAMILY_COLOUR_EN).add(UKURAN_EN).add(VARANSI_EN).add(PARENT_EN).add(VARIANT_IMAGE_EN)
          .add(IMAGE_1_EN).add(IMAGE_2_EN).add(IMAGE_3_EN).add(IMAGE_4_EN).add(IMAGE_5_EN).add(IMAGE_6_EN)
          .add(IMAGE_7_EN).add(URL_VIDEO_EN).add(PRODUCT_TYPE_EN).add(PICKUP_POINT_EN).add(LENGTH_EN).add(WIDTH_EN)
          .add(HEIGTH_EN).add(WEIGHT_EN).add(SALE_PRICE_EN).add(STOCK_EN).add(BFB_BASE_PRICE).add(BFB_MANAGED_INFO)
          .add(BFB_STATUS).add(DELIVERY_STATUS_EN).add(CNC_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN)
          .add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN)
          .add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).add(ATTRIBUTE_NAME_EN).add(ATTRIBUTE_VALUE_EN).build();

  public static final String SECONDARY_HEADER_CATEGORY_EN = "Select the main category from the dropdown";
  public static final String SECONDARY_HEADER_CATEGORY_2_EN = "Select the Sub category from the dropdown";
  public static final String SECONDARY_HEADER_CATEGORY_3_EN =
          "Select the category in which you want to create a product";
  public static final String SECONDARY_HEADER_PRODUCT_NAME_EN = "Enter the product name. Maximum 150 characters";
  public static final String SECONDARY_HEADER_UPC_EN = "Enter the Unique Product Code (number on the barcode)";
  public static final String SECONDARY_HEADER_SELLER_SKU_EN = "Enter SKU, max. 50 characters";
  public static final String SECONDARY_HEADER_DESCRIPTION_EN = "Enter the product description";
  public static final String SECONDARY_HEADER_USP_EN = "Enter the unique points of your product";
  public static final String SECONDARY_HEADER_BRAND_EN = "Enter the brand of the product";
  public static final String SECONDARY_HEADER_WARNA_EN = "Enter the color of the product (if applicable) ";
  public static final String SECONDARY_HEADER_FAMILY_COLOUR_EN = "Enter the family color	";
  public static final String SECONDARY_HEADER_UKURAN_EN = "Enter the Size of the product(if Applicalble)";
  public static final String SECONDARY_HEADER_VARANSI_EN = "Additional info for Products";
  public static final String SECONDARY_HEADER_PARENT_EN =
          "Enter the same code here to link all variants)";
  public static final String SECONDARY_HEADER_IMAGE_1_EN =
          "Enter the images of your product (min 1 image mandatory, Image format: JPG, JPEG and PNG, image name should not have special characters)";
  public static final String SECONDARY_HEADER_URL_VIDEO_EN =
          "You can add product videos to explain, use and demonstrate the product";
  public static final String SECONDARY_HEADER_PRODUCT_TYPE_EN =
          "Enter the shipping type ( Regular; Shipment by Seller Partner; Bopis)";
  public static final String SECONDARY_HEADER_PICKUP_POINT_EN =
          "Enter the store/warehouse location from the \"Toko\" sheet";
  public static final String SECONDARY_HEADER_LENGTH_EN = "Enter the length of the product in cms";
  public static final String SECONDARY_HEADER_WIDTH_EN = "Enter the breadth of the product in cms";
  public static final String SECONDARY_HEADER_HEIGTH_EN = "Enter the height of the product in cms";
  public static final String SECONDARY_HEADER_WEIGHT_EN = "Enter the weight of the product in grams";
  public static final String SECONDARY_HEADER_SALE_PRICE_EN = "Enter the selling price";
  public static final String SECONDARY_HEADER_STOCK_EN =
          "Please fill with your available stock, fill with 0 if you don't have stock for now";
  public static final String SECONDARY_HEADER_DELIVERY_STATUS_EN = "Online = 1, Offline = 0";
  public static final String SECONDARY_HEADER_BFB_BASE_PRICE = "Please fill in BfB base price";
  public static final String SECONDARY_HEADER_BFB_MANAGED_INFO = "Yes=1, No=0";
  public static final String SECONDARY_HEADER_BFB_STATUS = "Online=1, Offline=0";
  public static final String SECONDARY_HEADER_CNC_STATUS_EN = "On = 1, Off = 0";
  public static final String SECONDARY_HEADER_ATTRIBUTE_NAME_EN = "Select Attribute from the List";
  public static final String SECONDARY_HEADER_ATTRIBUTE_VALUE_EN = "Enter the value for the selected attribute";
  public static final String SECONDARY_HEADER_BFB_BASE_PRICE_IN = "Silakan isi BfB Harga dasar";

  public static final List<String> SECONDARY_HEADER_LIST_EN_MPP_SWITCH_ENABLED_CNC =
      ImmutableList.<String>builder().add(SECONDARY_HEADER_CATEGORY_EN).add(SECONDARY_HEADER_CATEGORY_2_EN)
          .add(SECONDARY_HEADER_CATEGORY_3_EN).add(SECONDARY_HEADER_PRODUCT_NAME_EN).add(SECONDARY_HEADER_UPC_EN)
          .add(SECONDARY_HEADER_SELLER_SKU_EN).add(SECONDARY_HEADER_DESCRIPTION_EN).add(SECONDARY_HEADER_USP_EN)
          .add(SECONDARY_HEADER_BRAND_EN).add(SECONDARY_HEADER_WARNA_EN).add(SECONDARY_HEADER_FAMILY_COLOUR_EN)
          .add(SECONDARY_HEADER_UKURAN_EN).add(SECONDARY_HEADER_VARANSI_EN).add(SECONDARY_HEADER_PARENT_EN)
          .add(VARIANT_IMAGE_INFO_EN).add(SECONDARY_HEADER_IMAGE_1_EN).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(SECONDARY_HEADER_URL_VIDEO_EN).add(SECONDARY_HEADER_PRODUCT_TYPE_EN)
          .add(SECONDARY_HEADER_PICKUP_POINT_EN).add(SECONDARY_HEADER_LENGTH_EN).add(SECONDARY_HEADER_WIDTH_EN)
          .add(SECONDARY_HEADER_HEIGTH_EN).add(SECONDARY_HEADER_WEIGHT_EN).add(SECONDARY_HEADER_SALE_PRICE_EN)
          .add(SECONDARY_HEADER_STOCK_EN).add(SECONDARY_HEADER_DELIVERY_STATUS_EN).add(SECONDARY_HEADER_CNC_STATUS_EN)
          .add(SECONDARY_HEADER_ATTRIBUTE_NAME_EN).add(SECONDARY_HEADER_ATTRIBUTE_VALUE_EN).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).build();

  public static final List<String> SECONDARY_HEADER_LIST_EN_MPP_SWITCH_ENABLED_NON_CNC =
      ImmutableList.<String>builder().add(SECONDARY_HEADER_CATEGORY_EN).add(SECONDARY_HEADER_CATEGORY_2_EN)
          .add(SECONDARY_HEADER_CATEGORY_3_EN).add(SECONDARY_HEADER_PRODUCT_NAME_EN).add(SECONDARY_HEADER_UPC_EN)
          .add(SECONDARY_HEADER_SELLER_SKU_EN).add(SECONDARY_HEADER_DESCRIPTION_EN).add(SECONDARY_HEADER_USP_EN)
          .add(SECONDARY_HEADER_BRAND_EN).add(SECONDARY_HEADER_WARNA_EN).add(SECONDARY_HEADER_FAMILY_COLOUR_EN)
          .add(SECONDARY_HEADER_UKURAN_EN).add(SECONDARY_HEADER_VARANSI_EN).add(SECONDARY_HEADER_PARENT_EN)
          .add(VARIANT_IMAGE_INFO_EN).add(SECONDARY_HEADER_IMAGE_1_EN).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(SECONDARY_HEADER_URL_VIDEO_EN).add(SECONDARY_HEADER_PRODUCT_TYPE_EN)
          .add(SECONDARY_HEADER_PICKUP_POINT_EN).add(SECONDARY_HEADER_LENGTH_EN).add(SECONDARY_HEADER_WIDTH_EN)
          .add(SECONDARY_HEADER_HEIGTH_EN).add(SECONDARY_HEADER_WEIGHT_EN).add(SECONDARY_HEADER_SALE_PRICE_EN)
          .add(SECONDARY_HEADER_STOCK_EN).add(SECONDARY_HEADER_DELIVERY_STATUS_EN)
          .add(SECONDARY_HEADER_ATTRIBUTE_NAME_EN).add(SECONDARY_HEADER_ATTRIBUTE_VALUE_EN).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).build();

  public static final List<String> SECONDARY_HEADER_LIST_EN_NON_CNC_BFB =
      ImmutableList.<String>builder().add(SECONDARY_HEADER_CATEGORY_EN).add(SECONDARY_HEADER_CATEGORY_2_EN)
          .add(SECONDARY_HEADER_CATEGORY_3_EN).add(SECONDARY_HEADER_PRODUCT_NAME_EN).add(SECONDARY_HEADER_UPC_EN)
          .add(SECONDARY_HEADER_SELLER_SKU_EN).add(SECONDARY_HEADER_DESCRIPTION_EN).add(SECONDARY_HEADER_USP_EN)
          .add(SECONDARY_HEADER_BRAND_EN).add(SECONDARY_HEADER_WARNA_EN).add(SECONDARY_HEADER_FAMILY_COLOUR_EN)
          .add(SECONDARY_HEADER_UKURAN_EN).add(SECONDARY_HEADER_VARANSI_EN).add(SECONDARY_HEADER_PARENT_EN)
          .add(VARIANT_IMAGE_INFO_EN).add(SECONDARY_HEADER_IMAGE_1_EN).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(SECONDARY_HEADER_URL_VIDEO_EN).add(SECONDARY_HEADER_PRODUCT_TYPE_EN)
          .add(SECONDARY_HEADER_PICKUP_POINT_EN).add(SECONDARY_HEADER_LENGTH_EN).add(SECONDARY_HEADER_WIDTH_EN)
          .add(SECONDARY_HEADER_HEIGTH_EN).add(SECONDARY_HEADER_WEIGHT_EN).add(SECONDARY_HEADER_SALE_PRICE_EN)
          .add(SECONDARY_HEADER_STOCK_EN).add(SECONDARY_HEADER_BFB_BASE_PRICE).add(SECONDARY_HEADER_BFB_MANAGED_INFO)
          .add(SECONDARY_HEADER_BFB_STATUS).add(SECONDARY_HEADER_DELIVERY_STATUS_EN)
          .add(SECONDARY_HEADER_ATTRIBUTE_NAME_EN).add(SECONDARY_HEADER_ATTRIBUTE_VALUE_EN).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).build();

  public static final List<String> SECONDARY_HEADER_LIST_EN_CNC_BFB =
      ImmutableList.<String>builder().add(SECONDARY_HEADER_CATEGORY_EN).add(SECONDARY_HEADER_CATEGORY_2_EN)
          .add(SECONDARY_HEADER_CATEGORY_3_EN).add(SECONDARY_HEADER_PRODUCT_NAME_EN).add(SECONDARY_HEADER_UPC_EN)
          .add(SECONDARY_HEADER_SELLER_SKU_EN).add(SECONDARY_HEADER_DESCRIPTION_EN).add(SECONDARY_HEADER_USP_EN)
          .add(SECONDARY_HEADER_BRAND_EN).add(SECONDARY_HEADER_WARNA_EN).add(SECONDARY_HEADER_FAMILY_COLOUR_EN)
          .add(SECONDARY_HEADER_UKURAN_EN).add(SECONDARY_HEADER_VARANSI_EN).add(SECONDARY_HEADER_PARENT_EN)
          .add(VARIANT_IMAGE_INFO_EN).add(SECONDARY_HEADER_IMAGE_1_EN).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(SECONDARY_HEADER_URL_VIDEO_EN).add(SECONDARY_HEADER_PRODUCT_TYPE_EN)
          .add(SECONDARY_HEADER_PICKUP_POINT_EN).add(SECONDARY_HEADER_LENGTH_EN).add(SECONDARY_HEADER_WIDTH_EN)
          .add(SECONDARY_HEADER_HEIGTH_EN).add(SECONDARY_HEADER_WEIGHT_EN).add(SECONDARY_HEADER_SALE_PRICE_EN)
          .add(SECONDARY_HEADER_STOCK_EN).add(SECONDARY_HEADER_BFB_BASE_PRICE).add(SECONDARY_HEADER_BFB_MANAGED_INFO)
          .add(SECONDARY_HEADER_BFB_STATUS).add(SECONDARY_HEADER_DELIVERY_STATUS_EN)
          .add(SECONDARY_HEADER_ATTRIBUTE_NAME_EN).add(SECONDARY_HEADER_ATTRIBUTE_VALUE_EN).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).build();



  public static final String SECONDARY_HEADER_CATEGORY = "Pilih kategori dari pilihan yang tersedia.";
  public static final String SECONDARY_HEADER_CATEGORY_2 = "Pilih subkategori dari pilihan yang tersedia.";
  public static final String SECONDARY_HEADER_CATEGORY_3 = "Pilih kategori dari produk yang ingin dijual.";
  public static final String SECONDARY_HEADER_PRODUCT_NAME = "Masukkan nama produk. Maksimum 150 karakter.";
  public static final String SECONDARY_HEADER_UPC = "Masukkan Unique Product Code (nomor pada barcode).";
  public static final String SECONDARY_HEADER_SELLER_SKU = "Masukkan detail SKU seller.";
  public static final String SECONDARY_HEADER_DESCRIPTION = "Buat deskripsi produk yang menarik supaya banyak pelanggan melirik.";
  public static final String SECONDARY_HEADER_USP = "Ceritakan keunggulan produk Anda yang membedakan dengan produk lainnya.";
  public static final String SECONDARY_HEADER_BRAND = "Masukkan merek produk Anda.";
  public static final String SECONDARY_HEADER_WARNA = "Masukkan warna produk (jika ada). ";
  public static final String SECONDARY_HEADER_FAMILY_COLOUR = "Masukkan family color (jika ada). ";
  public static final String SECONDARY_HEADER_UKURAN = "Masukkan ukuran produk Anda (jika ada). ";
  public static final String SECONDARY_HEADER_VARANSI = "Informasi varian lainnya (akan digunakan untuk membuat varian, Anda dapat memasukkan jenis varian apa pun di sini)";
  public static final String SECONDARY_HEADER_PARENT = "Masukkan kode yang sama di sini untuk menghubungkan semua varian.";
  public static final String SECONDARY_HEADER_IMAGE_1 = "Masukkan foto produk Anda (Wajib upload min.1 foto, format foto: JPG, JPEG dan PNG, nama foto tidak boleh memiliki karakter khusus).";
  public static final String SECONDARY_HEADER_URL_VIDEO = "Anda bisa menambahkan video produk untuk menjelaskan, menggunakan, dan demo produk.";
  public static final String SECONDARY_HEADER_PRODUCT_TYPE = "Masukkan jenis pengiriman (Reguler; pengiriman oleh seller, BOPIS).";
  public static final String SECONDARY_HEADER_PICKUP_POINT = "Masukkan lokasi toko / gudang dari sheet \"Toko\".";
  public static final String SECONDARY_HEADER_LENGTH = "Panjang produk dalam satuan cm.";
  public static final String SECONDARY_HEADER_WIDTH = "Lebar produk dalam satuan cm.";
  public static final String SECONDARY_HEADER_HEIGTH = "Tinggi produk dalam satuan cm.";
  public static final String SECONDARY_HEADER_WEIGHT = "Berat produk dalam satuan gram.";
  public static final String SECONDARY_HEADER_SALE_PRICE = "Tentukan harga jual.";
  public static final String SECONDARY_HEADER_STOCK = "Silakan isi stok produk. Isi dengan 0 jika tidak tersedia.";
  public static final String SECONDARY_HEADER_DELIVERY_STATUS = "";
  public static final String SECONDARY_HEADER_CNC_STATUS = "";
  public static final String SECONDARY_HEADER_ATTRIBUTE_NAME = "Pilih atribut dari daftar yang ada.";
  public static final String SECONDARY_HEADER_ATTRIBUTE_VALUE = "Pilih value untuk atribut terpilih.";

  private GenericBulkHeaders() {
  }

  public static final List<String> SECONDARY_HEADER_LIST_MPP_SWITCH_ENABLED_CNC =
      ImmutableList.<String>builder().add(SECONDARY_HEADER_CATEGORY).add(SECONDARY_HEADER_CATEGORY_2)
          .add(SECONDARY_HEADER_CATEGORY_3).add(SECONDARY_HEADER_PRODUCT_NAME).add(SECONDARY_HEADER_UPC)
          .add(SECONDARY_HEADER_SELLER_SKU).add(SECONDARY_HEADER_DESCRIPTION).add(SECONDARY_HEADER_USP)
          .add(SECONDARY_HEADER_BRAND).add(SECONDARY_HEADER_WARNA).add(SECONDARY_HEADER_FAMILY_COLOUR)
          .add(SECONDARY_HEADER_UKURAN).add(SECONDARY_HEADER_VARANSI).add(SECONDARY_HEADER_PARENT)
          .add(VARIANT_IMAGE_INFO_ID).add(SECONDARY_HEADER_IMAGE_1).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(SECONDARY_HEADER_URL_VIDEO).add(SECONDARY_HEADER_PRODUCT_TYPE).add(SECONDARY_HEADER_PICKUP_POINT)
          .add(SECONDARY_HEADER_LENGTH).add(SECONDARY_HEADER_WIDTH).add(SECONDARY_HEADER_HEIGTH)
          .add(SECONDARY_HEADER_WEIGHT).add(SECONDARY_HEADER_SALE_PRICE).add(SECONDARY_HEADER_STOCK)
          .add(SECONDARY_HEADER_DELIVERY_STATUS).add(SECONDARY_HEADER_CNC_STATUS).add(SECONDARY_HEADER_ATTRIBUTE_NAME)
          .add(SECONDARY_HEADER_ATTRIBUTE_VALUE).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).build();

  public static final List<String> SECONDARY_HEADER_LIST_MPP_SWITCH_ENABLED_NON_CNC =
      ImmutableList.<String>builder().add(SECONDARY_HEADER_CATEGORY).add(SECONDARY_HEADER_CATEGORY_2)
          .add(SECONDARY_HEADER_CATEGORY_3).add(SECONDARY_HEADER_PRODUCT_NAME).add(SECONDARY_HEADER_UPC)
          .add(SECONDARY_HEADER_SELLER_SKU).add(SECONDARY_HEADER_DESCRIPTION).add(SECONDARY_HEADER_USP)
          .add(SECONDARY_HEADER_BRAND).add(SECONDARY_HEADER_WARNA).add(SECONDARY_HEADER_FAMILY_COLOUR)
          .add(SECONDARY_HEADER_UKURAN).add(SECONDARY_HEADER_VARANSI).add(SECONDARY_HEADER_PARENT)
          .add(VARIANT_IMAGE_INFO_ID).add(SECONDARY_HEADER_IMAGE_1).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(SECONDARY_HEADER_URL_VIDEO).add(SECONDARY_HEADER_PRODUCT_TYPE).add(SECONDARY_HEADER_PICKUP_POINT)
          .add(SECONDARY_HEADER_LENGTH).add(SECONDARY_HEADER_WIDTH).add(SECONDARY_HEADER_HEIGTH)
          .add(SECONDARY_HEADER_WEIGHT).add(SECONDARY_HEADER_SALE_PRICE).add(SECONDARY_HEADER_STOCK)
          .add(SECONDARY_HEADER_DELIVERY_STATUS).add(SECONDARY_HEADER_ATTRIBUTE_NAME)
          .add(SECONDARY_HEADER_ATTRIBUTE_VALUE).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).build();

  public static final List<String> SECONDARY_HEADER_LIST_CNC_BFB =
      ImmutableList.<String>builder().add(SECONDARY_HEADER_CATEGORY).add(SECONDARY_HEADER_CATEGORY_2)
          .add(SECONDARY_HEADER_CATEGORY_3).add(SECONDARY_HEADER_PRODUCT_NAME).add(SECONDARY_HEADER_UPC)
          .add(SECONDARY_HEADER_SELLER_SKU).add(SECONDARY_HEADER_DESCRIPTION).add(SECONDARY_HEADER_USP)
          .add(SECONDARY_HEADER_BRAND).add(SECONDARY_HEADER_WARNA).add(SECONDARY_HEADER_FAMILY_COLOUR)
          .add(SECONDARY_HEADER_UKURAN).add(SECONDARY_HEADER_VARANSI).add(SECONDARY_HEADER_PARENT)
          .add(VARIANT_IMAGE_INFO_ID).add(SECONDARY_HEADER_IMAGE_1).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(SECONDARY_HEADER_URL_VIDEO).add(SECONDARY_HEADER_PRODUCT_TYPE).add(SECONDARY_HEADER_PICKUP_POINT)
          .add(SECONDARY_HEADER_LENGTH).add(SECONDARY_HEADER_WIDTH).add(SECONDARY_HEADER_HEIGTH)
          .add(SECONDARY_HEADER_WEIGHT).add(SECONDARY_HEADER_SALE_PRICE).add(SECONDARY_HEADER_STOCK)
          .add(SECONDARY_HEADER_BFB_BASE_PRICE_IN).add(SECONDARY_HEADER_BFB_MANAGED_INFO)
          .add(SECONDARY_HEADER_BFB_STATUS).add(SECONDARY_HEADER_DELIVERY_STATUS).add(SECONDARY_HEADER_CNC_STATUS)
          .add(SECONDARY_HEADER_ATTRIBUTE_NAME).add(SECONDARY_HEADER_ATTRIBUTE_VALUE).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).build();

  public static final List<String> SECONDARY_HEADER_LIST_NON_CNC_BFB =
      ImmutableList.<String>builder().add(SECONDARY_HEADER_CATEGORY).add(SECONDARY_HEADER_CATEGORY_2)
          .add(SECONDARY_HEADER_CATEGORY_3).add(SECONDARY_HEADER_PRODUCT_NAME).add(SECONDARY_HEADER_UPC)
          .add(SECONDARY_HEADER_SELLER_SKU).add(SECONDARY_HEADER_DESCRIPTION).add(SECONDARY_HEADER_USP)
          .add(SECONDARY_HEADER_BRAND).add(SECONDARY_HEADER_WARNA).add(SECONDARY_HEADER_FAMILY_COLOUR)
          .add(SECONDARY_HEADER_UKURAN).add(SECONDARY_HEADER_VARANSI).add(SECONDARY_HEADER_PARENT)
          .add(VARIANT_IMAGE_INFO_ID).add(SECONDARY_HEADER_IMAGE_1).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(SECONDARY_HEADER_URL_VIDEO).add(SECONDARY_HEADER_PRODUCT_TYPE).add(SECONDARY_HEADER_PICKUP_POINT)
          .add(SECONDARY_HEADER_LENGTH).add(SECONDARY_HEADER_WIDTH).add(SECONDARY_HEADER_HEIGTH)
          .add(SECONDARY_HEADER_WEIGHT).add(SECONDARY_HEADER_SALE_PRICE).add(SECONDARY_HEADER_STOCK)
          .add(SECONDARY_HEADER_BFB_BASE_PRICE_IN).add(SECONDARY_HEADER_BFB_MANAGED_INFO)
          .add(SECONDARY_HEADER_BFB_STATUS).add(SECONDARY_HEADER_DELIVERY_STATUS).add(SECONDARY_HEADER_ATTRIBUTE_NAME)
          .add(SECONDARY_HEADER_ATTRIBUTE_VALUE).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY).add(StringUtils.EMPTY)
          .add(StringUtils.EMPTY).build();

  public static final String C2 = "C2";
  public static final String CN = "CN";

  public static final List<String> HEADER_DATA_LIST_EN_CNC_MERCHANT =
      ImmutableList.<String>builder().add(CATEGORY_EN).add(C2).add(CN).add(PRODUCT_NAME_EN).add(UPC_EN)
          .add(SELLER_SKU_EN).add(DESCRIPTION_EN).add(USP_EN).add(BRAND_EN).add(WARNA_EN).add(FAMILY_COLOUR_EN)
          .add(UKURAN_EN).add(VARANSI_EN).add(PARENT_EN).add(VARIANT_IMAGE_EN).add(IMAGE_1_EN).add(IMAGE_2_EN)
          .add(IMAGE_3_EN).add(IMAGE_4_EN).add(IMAGE_5_EN).add(IMAGE_6_EN).add(IMAGE_7_EN).add(URL_VIDEO_EN)
          .add(PRODUCT_TYPE_EN).add(PICKUP_POINT_EN).add(LENGTH_EN).add(WIDTH_EN).add(HEIGTH_EN).add(WEIGHT_EN)
          .add(SALE_PRICE_EN).add(STOCK_EN).add(DELIVERY_STATUS_EN).add(CNC_EN).add(ATTRIBUTE_NAME_EN + "1")
          .add(ATTRIBUTE_VALUE_EN + "1").add(ATTRIBUTE_NAME_EN + "2").add(ATTRIBUTE_VALUE_EN + "2")
          .add(ATTRIBUTE_NAME_EN + "3").add(ATTRIBUTE_VALUE_EN + "3").add(ATTRIBUTE_NAME_EN + "4")
          .add(ATTRIBUTE_VALUE_EN + "4").add(ATTRIBUTE_NAME_EN + "5").add(ATTRIBUTE_VALUE_EN + "5").build();

  public static final List<String> HEADER_DATA_LIST_EN_NON_CNC_MERCHANT =
      ImmutableList.<String>builder().add(CATEGORY_EN).add(C2).add(CN).add(PRODUCT_NAME_EN).add(UPC_EN)
          .add(SELLER_SKU_EN).add(DESCRIPTION_EN).add(USP_EN).add(BRAND_EN).add(WARNA_EN).add(FAMILY_COLOUR_EN)
          .add(UKURAN_EN).add(VARANSI_EN).add(PARENT_EN).add(VARIANT_IMAGE_EN).add(IMAGE_1_EN).add(IMAGE_2_EN)
          .add(IMAGE_3_EN).add(IMAGE_4_EN).add(IMAGE_5_EN).add(IMAGE_6_EN).add(IMAGE_7_EN).add(URL_VIDEO_EN)
          .add(PRODUCT_TYPE_EN).add(PICKUP_POINT_EN).add(LENGTH_EN).add(WIDTH_EN).add(HEIGTH_EN).add(WEIGHT_EN)
          .add(SALE_PRICE_EN).add(STOCK_EN).add(DELIVERY_STATUS_EN).add(ATTRIBUTE_NAME_EN + "1")
          .add(ATTRIBUTE_VALUE_EN + "1").add(ATTRIBUTE_NAME_EN + "2").add(ATTRIBUTE_VALUE_EN + "2")
          .add(ATTRIBUTE_NAME_EN + "3").add(ATTRIBUTE_VALUE_EN + "3").add(ATTRIBUTE_NAME_EN + "4")
          .add(ATTRIBUTE_VALUE_EN + "4").add(ATTRIBUTE_NAME_EN + "5").add(ATTRIBUTE_VALUE_EN + "5").build();


  public static final List<String> HEADER_DATA_LIST_CNC_MERCHANT =
      ImmutableList.<String>builder().add(CATEGORY).add(C2).add(CN).add(PRODUCT_NAME).add(UPC).add(SELLER_SKU)
          .add(DESCRIPTION).add(USP).add(BRAND).add(WARNA).add(FAMILY_COLOUR).add(UKURAN).add(VARANSI).add(PARENT)
          .add(VARIANT_IMAGE_ID).add(IMAGE_1).add(IMAGE_2).add(IMAGE_3).add(IMAGE_4).add(IMAGE_5).add(IMAGE6)
          .add(IMAGE_7).add(URL_VIDEO).add(PRODUCT_TYPE).add(PICKUP_POINT).add(LENGTH).add(WIDTH).add(HEIGTH)
          .add(WEIGHT).add(SALE_PRICE).add(STOCK).add(DELIVERY_STATUS).add(CNC).add(ATTRIBUTE_NAME + " 1")
          .add(ATTRIBUTE_VALUE + " 1").add(ATTRIBUTE_NAME + " 2").add(ATTRIBUTE_VALUE + " 2").add(ATTRIBUTE_NAME + " 3")
          .add(ATTRIBUTE_VALUE + " 3").add(ATTRIBUTE_NAME + " 4").add(ATTRIBUTE_VALUE + " 4").add(ATTRIBUTE_NAME + " 5")
          .add(ATTRIBUTE_VALUE + " 5").build();

  public static final List<String> HEADER_DATA_LIST_NON_CNC_MERCHANT =
      ImmutableList.<String>builder().add(CATEGORY).add(C2).add(CN).add(PRODUCT_NAME).add(UPC).add(SELLER_SKU)
          .add(DESCRIPTION).add(USP).add(BRAND).add(WARNA).add(FAMILY_COLOUR).add(UKURAN).add(VARANSI).add(PARENT)
          .add(VARIANT_IMAGE_ID).add(IMAGE_1).add(IMAGE_2).add(IMAGE_3).add(IMAGE_4).add(IMAGE_5).add(IMAGE6)
          .add(IMAGE_7).add(URL_VIDEO).add(PRODUCT_TYPE).add(PICKUP_POINT).add(LENGTH).add(WIDTH).add(HEIGTH)
          .add(WEIGHT).add(SALE_PRICE).add(STOCK).add(DELIVERY_STATUS).add(ATTRIBUTE_NAME + " 1")
          .add(ATTRIBUTE_VALUE + " 1").add(ATTRIBUTE_NAME + " 2").add(ATTRIBUTE_VALUE + " 2").add(ATTRIBUTE_NAME + " 3")
          .add(ATTRIBUTE_VALUE + " 3").add(ATTRIBUTE_NAME + " 4").add(ATTRIBUTE_VALUE + " 4").add(ATTRIBUTE_NAME + " 5")
          .add(ATTRIBUTE_VALUE + " 5").build();

  public static final List<String> HEADER_DATA_LIST_EN_BFB_NON_CNC_MERCHANT =
      ImmutableList.<String>builder().add(CATEGORY_EN).add(C2).add(CN).add(PRODUCT_NAME_EN).add(UPC_EN)
          .add(SELLER_SKU_EN).add(DESCRIPTION_EN).add(USP_EN).add(BRAND_EN).add(WARNA_EN).add(FAMILY_COLOUR_EN)
          .add(UKURAN_EN).add(VARANSI_EN).add(PARENT_EN).add(VARIANT_IMAGE_EN).add(IMAGE_1_EN).add(IMAGE_2_EN)
          .add(IMAGE_3_EN).add(IMAGE_4_EN).add(IMAGE_5_EN).add(IMAGE_6_EN).add(IMAGE_7_EN).add(URL_VIDEO_EN)
          .add(PRODUCT_TYPE_EN).add(PICKUP_POINT_EN).add(LENGTH_EN).add(WIDTH_EN).add(HEIGTH_EN).add(WEIGHT_EN)
          .add(SALE_PRICE_EN).add(STOCK_EN).add(BFB_BASE_PRICE_HEADER_EN).add(BFB_MANAGED_HEADER_EN)
          .add(BFB_STATUS_HEADER).add(DELIVERY_STATUS_EN).add(ATTRIBUTE_NAME_EN + "1").add(ATTRIBUTE_VALUE_EN + "1")
          .add(ATTRIBUTE_NAME_EN + "2").add(ATTRIBUTE_VALUE_EN + "2").add(ATTRIBUTE_NAME_EN + "3")
          .add(ATTRIBUTE_VALUE_EN + "3").add(ATTRIBUTE_NAME_EN + "4").add(ATTRIBUTE_VALUE_EN + "4")
          .add(ATTRIBUTE_NAME_EN + "5").add(ATTRIBUTE_VALUE_EN + "5").build();

  public static final List<String> HEADER_DATA_LIST_ID_BFB_NON_CNC_MERCHANT =
      ImmutableList.<String>builder().add(CATEGORY).add(C2).add(CN).add(PRODUCT_NAME).add(UPC).add(SELLER_SKU)
          .add(DESCRIPTION).add(USP).add(BRAND).add(WARNA).add(FAMILY_COLOUR).add(UKURAN).add(VARANSI).add(PARENT)
          .add(VARIANT_IMAGE_ID).add(IMAGE_1).add(IMAGE_2).add(IMAGE_3).add(IMAGE_4).add(IMAGE_5).add(IMAGE6)
          .add(IMAGE_7).add(URL_VIDEO).add(PRODUCT_TYPE).add(PICKUP_POINT).add(LENGTH).add(WIDTH).add(HEIGTH)
          .add(WEIGHT).add(SALE_PRICE).add(STOCK).add(BFB_BASE_PRICE_HEADER_ID).add(BFB_MANAGED_HEADER_ID)
          .add(BFB_STATUS_HEADER).add(DELIVERY_STATUS).add(ATTRIBUTE_NAME + " 1").add(ATTRIBUTE_VALUE + " 1")
          .add(ATTRIBUTE_NAME + " 2").add(ATTRIBUTE_VALUE + " 2").add(ATTRIBUTE_NAME + " 3").add(ATTRIBUTE_VALUE + " 3")
          .add(ATTRIBUTE_NAME + " 4").add(ATTRIBUTE_VALUE + " 4").add(ATTRIBUTE_NAME + " 5").add(ATTRIBUTE_VALUE + " 5")
          .build();

  public static final List<String> HEADER_DATA_LIST_EN_BFB_CNC_MERCHANT =
      ImmutableList.<String>builder().add(CATEGORY_EN).add(C2).add(CN).add(PRODUCT_NAME_EN).add(UPC_EN)
          .add(SELLER_SKU_EN).add(DESCRIPTION_EN).add(USP_EN).add(BRAND_EN).add(WARNA_EN).add(FAMILY_COLOUR_EN)
          .add(UKURAN_EN).add(VARANSI_EN).add(PARENT_EN).add(VARIANT_IMAGE_EN).add(IMAGE_1_EN).add(IMAGE_2_EN)
          .add(IMAGE_3_EN).add(IMAGE_4_EN).add(IMAGE_5_EN).add(IMAGE_6_EN).add(IMAGE_7_EN).add(URL_VIDEO_EN)
          .add(PRODUCT_TYPE_EN).add(PICKUP_POINT_EN).add(LENGTH_EN).add(WIDTH_EN).add(HEIGTH_EN).add(WEIGHT_EN)
          .add(SALE_PRICE_EN).add(STOCK_EN).add(BFB_BASE_PRICE_HEADER_EN).add(BFB_MANAGED_HEADER_EN)
          .add(BFB_STATUS_HEADER).add(DELIVERY_STATUS_EN).add(CNC_EN).add(ATTRIBUTE_NAME_EN + "1")
          .add(ATTRIBUTE_VALUE_EN + "1").add(ATTRIBUTE_NAME_EN + "2").add(ATTRIBUTE_VALUE_EN + "2")
          .add(ATTRIBUTE_NAME_EN + "3").add(ATTRIBUTE_VALUE_EN + "3").add(ATTRIBUTE_NAME_EN + "4")
          .add(ATTRIBUTE_VALUE_EN + "4").add(ATTRIBUTE_NAME_EN + "5").add(ATTRIBUTE_VALUE_EN + "5").build();

  public static final List<String> HEADER_DATA_LIST_ID_BFB_CNC_MERCHANT =
      ImmutableList.<String>builder().add(CATEGORY).add(C2).add(CN).add(PRODUCT_NAME).add(UPC).add(SELLER_SKU)
          .add(DESCRIPTION).add(USP).add(BRAND).add(WARNA).add(FAMILY_COLOUR).add(UKURAN).add(VARANSI).add(PARENT)
          .add(VARIANT_IMAGE_ID).add(IMAGE_1).add(IMAGE_2).add(IMAGE_3).add(IMAGE_4).add(IMAGE_5).add(IMAGE6)
          .add(IMAGE_7).add(URL_VIDEO).add(PRODUCT_TYPE).add(PICKUP_POINT).add(LENGTH).add(WIDTH).add(HEIGTH)
          .add(WEIGHT).add(SALE_PRICE).add(STOCK).add(BFB_BASE_PRICE_HEADER_ID).add(BFB_MANAGED_HEADER_ID)
          .add(BFB_STATUS_HEADER).add(DELIVERY_STATUS).add(CNC).add(ATTRIBUTE_NAME + " 1").add(ATTRIBUTE_VALUE + " 1")
          .add(ATTRIBUTE_NAME + " 2").add(ATTRIBUTE_VALUE + " 2").add(ATTRIBUTE_NAME + " 3").add(ATTRIBUTE_VALUE + " 3")
          .add(ATTRIBUTE_NAME + " 4").add(ATTRIBUTE_VALUE + " 4").add(ATTRIBUTE_NAME + " 5").add(ATTRIBUTE_VALUE + " 5")
          .build();

  public static final List<String> IMAGE_EN_HEADER_LIST =
      Arrays.asList(IMAGE_1_EN, IMAGE_2_EN, IMAGE_3_EN, IMAGE_4_EN, IMAGE_5_EN, IMAGE_6_EN, IMAGE_7_EN);
  public static final List<String> IMAGE_ID_HEADER_LIST =
      Arrays.asList(IMAGE_1, IMAGE_2, IMAGE_3, IMAGE_4, IMAGE_5, IMAGE6, IMAGE_7);

  public static final Map<Boolean, String> CHILD_SKU = ImmutableMap.of(true, CHILD_SKU_EN, false, CHILD_SKU_ID);
  public static final Map<Boolean, String> QUANTITY = ImmutableMap.of(true, QUANTITY_EN, false, QUANTITY_ID);

  public static final int NUMBER_OF_IMAGES = 7;

  public static final Map<Boolean, String> VARIANT_IMAGE =
      ImmutableMap.of(true, VARIANT_IMAGE_EN, false, VARIANT_IMAGE_ID);

  public static final Set<String> IMAGE_PREFIX_SET =
      ImmutableSet.of(FOTO_PREFIX, IMAGE_PREFIX, VARIANT_IMAGE_EN, VARIANT_IMAGE_ID);

}