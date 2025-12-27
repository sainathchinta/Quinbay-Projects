package com.gdn.mta.bulk.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import com.gdn.mta.bulk.BulkUploadOption;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;

/**
 * Created by virajjasani on 01/09/16.
 */
public class BulkParameters {

  public static final String PRICE_HEADER = "Harga (Rp)";
  public static final String PRICE_HEADER_DESC =
      "Diisi dengan harga normal sebelum diskon. Hanya boleh diisi angka.Harga produk promo tidak bisa diubah.";
  public static final String SELLING_PRICE_HEADER = "Harga Penjualan (Rp)";
  public static final String STOCK_HEADER = "Stok";
  public static final String PO_QUOTA = "PO Quota";
  public static final String STOCK_REMINDER_COLUMN_ID = "Pengingat stok";
  public static final String TYPE_HANDLING_HEADER = "Tipe Penanganan";
  public static final String PICKUP_POINT_HEADER = "Toko/Gudang";
  public static final String PICKUP_POINT_HEADER_DESC =
      "Pilih alamat pengambilan dari opsi yang ada. Contoh: PP-3212345.Kode yang sudah ada tidak "
          + "bisa diubah, hanya bisa menambahkan kode alamat pengambilan baru.";
  public static final String PUBLISHED_HEADER = "Ditampilkan";
  public static final String PURCHASED_HEADER = "Dapat Dibeli";
  public static final String OFFLINE_TO_ONLINE_HEADER = "Offline To Online";
  public static final String SKU_CODE_HEADER = "SKU Code";
  public static final String UPC_CODE_NAME_DESC ="Diisi dengan EAN/UPC. Bisa diedit sesuai varian.";
  public static final String SKU_CODE_HEADER_DESC = "Diisi dengan kode produk. Contoh: MTA-123456789-00001";
  public static final String CNC_STATUS_HEADER = "Click & Collect(0= Off, 1 = On)";
  public static final String DELIVERY_STATUS_HEADER = "Pengiriman(0 = Off, 1 = On)";
  public static final String DELIVERY_STATUS_HEADER_DESC =
      "Diisi dengan angka sesuai status pengiriman: 0 = Offline, 1 = Online";
  public static final String AMPHI_SKU_STATUS_CNC_1P =
      "Status(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)";
  public static final String EXTERNAL_SKU_STATUS_CNC_1P = "Status(0 = Offline, 1 = Online)";

  public static final String EXTERNAL_CNC_STATUS_HEADER = "Click & Collect(0= Offline, 1 = Online)";
  public static final String CNC_HEADER_DESC =
      "Diisi dengan angka sesuai status Click & Collect: 0 = Offline, 1 = Online";
  public static final String AMPHI_CNC_STATUS_HEADER = "Click & Collect(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)";
  public static final String WAREHOUSE_STOCK_HEADER = "Warehouse Stock";
  public static final String WAREHOUSE_STOCK_HEADER_DESC =
      "Stok gudang saat ini. Tidak akan mengubah data yang ada saat ini.";
  public static final String PRODUCT_NAME = "Nama Produk";
  public static final String PRODUCT_NAME_DESC =
      "Diisi dengan nama varian produk. Tidak akan mengubah data yang ada saat ini";
  public static final String PRODUCT_NAME_NON_EDITABLE = "Nama Produk (non-editable)";
  public static final String BLIBLI_SKU = "Blibli SKU";
  public static final String BLIBLI_SKU_DESC =
      "Diisi dengan kode SKU varian dari sistem Blibli. Contoh: BEE-12345-00001-00001";
  public static final String BLIBLI_PRODUCT_SKU = "Blibli Product SKU";
  public static final String BLIBLI_PRODUCT_SKU_DESC = "Diisi dengan kode SKU produk dari sistem Blibli. Contoh: BEE-12345-00001";
  public static final String BLIBLI_SKU_WITH_EXAMPLE = "Blibli SKU (contoh: BLI-10000-00011-00001)";
  public static final String SUBJECT_TO_VAT  = "Subject to VAT (0= VAT off, 1 = VAT on)";
  public static final String PARENT_PRODUCT_NAME = "Parent Product name";
  public static final String PARENT_PRODUCT_NAME_DESC =
      "Diisi dengan nama produk parent. Tidak akan mengubah data yang ada saat ini.";

  public static final String PICKUP_POINT_NAME_HEADER = "Nama alamat pengambilan";
  public static final String PICKUP_POINT_NAME_HEADER_DESC =
      "Diisi dengan nama alamat pengambilan sesuai dengan kode alamat pengambilan. ";
  public static final String OFF2ON_VALUE = "Off2OnFlag";
  public static final String ROW_NUMBER = "RowNumber";
  public static final String SELLER_SKU = "Seller SKU";
  public static final String EAN_OR_UPC = "EAN / UPC";
  public static final String SELLER_SKU_DESC = "Diisi dengan SKU seller. Bisa diedit sesuai varian.";
  public static final String ENABLE_VALUE = "1";
  public static final String DISABLE_VALUE = "0";
  public static final String ENABLE_DECIMAL_VALUE = "1.0";
  public static final String PRIVILEGE_EDIT_STOCK = "isPrivilegedToEditAvailableStock";
  public static final String PRIVILEGE_EDIT_CNC_STATUS = "isPrivilegedToEditCncStatus";
  public static final String PRIVILEGE_EDIT_PRICE = "isPrivilegedToEditPrice";
  public static final String PRIVILEGE_EDIT_PRODUCT_TYPE = "isPrivilegedToEditProductType";
  public static final String PRIVILEGE_EDIT_DISPLAY_BUYABLE = "isPrivilegedToEditDisplayBuyable";
  public static final String PRIVILEGE_EDIT_PICKUP_POINT = "isPrivilegedToEditPickupPoint";
  public static final String PRIVILEGE_EDIT_O2O = "isPrivilegedToEditO2O";
  public static final String PRIVILEGE_READ_PRODUCT_TYPE = "isPrivilegedToReadProductType";
  public static final String PRIVILEGE_READ_PICKUP_POINT = "isPrivilegedToReadPickupPoint";
  public static final String PRIVILEGE_READ_PRICE = "isPrivilegedToReadPrice";
  public static final String PRIVILEGE_READ_STOCK = "isPrivilegedToReadAvailableStock";
  public static final String PRIVILEGE_READ_DISPLAY_BUYABLE = "isPrivilegedToReadDisplayBuyable";
  public static final String PRIVILEGE_READ_O2O = "isPrivilegedToReadO2O";
  public static final String PRIVILEGE_READ_WAREHOUSE_STOCK = "isPrivilegedToReadWarehouseStock";
  public static final String DATA_SHEET = "Data";
  public static final String CATEGORY_SHEET = "Category";
  public static final String PICKUP_POINT_SHEET = "Toko";
  public static final String PRODUCT_CODE = "Kode Produk";
  public static final String BRAND = "Brand";
  public static final String WARNA = "Warna";
  public static final String UKURAN = "Ukuran";
  public static final String FAMILY_COLOUR = "Family Colour";
  public static final String WIDTH = "Lebar";
  public static final String LENGTH = "Panjang";
  public static final String HEIGHT = "Tinggi";
  public static final String WEIGHT = "Berat";
  public static final String DG_LEVEL = "Dangerous goods level";
  public static final String DG_LEVEL1 = "0";
  public static final String DG_LEVEL2 = "1";
  public static final String DG_LEVEL3 = "2";
  public static final String CATEGORY = "Kategori";
  public static final String CATEGORY_CN = "Kategori cN";
  public static final String CATEGORY_C1 = "Kategori c1";
  public static final String PRODUCT_SKU = "Produk SKU";
  public static final String SKU_NAME = "Nama SKU";
  public static final String STORE_NAME = "Nama Toko";
  public static final String INITIATOR = "Initiator";
  public static final String DATE_ADDED = "Waktu Ditambahkan";
  public static final String STATUS = "Status";
  public static final String CONTENT = "Konten";
  public static final String IMAGE = "Gambar";
  public static final String CONTENT_ASSIGNEE = "Konten Assignee";
  public static final String ASSIGNEE = "Produk Assignee";
  public static final String IMAGE_ASSIGNEE = "Gambar Assignee";
  public static final String CONTENT_ASSIGNEE_DATE_ADDED = "Konten Waktu Ditambahkan";
  public static final String IMAGE_ASSIGNEE_DATE_ADDED = "Gambar Waktu Ditambahkan";
  public static final String ASSIGNEE_DATE_ADDED = "Terbaru Waktu Ditambahkan";
  public static final String CATEGORY_CODE = "Category Code";
  public static final String CATEGORY_NAME = "Category Name";
  public static final String BUSINESS_PARTNER_CODE = "Business Partner Code";
  public static final String BUSINESS_PARTNER_NAME = "Business Partner Name";
  public static final String ASSIGNED_TO = "Assigned To";
  public static final String SUBMITTED_DATE = "Submitted Date";
  public static final String STATE = "State";
  public static final String ERROR_HEADER = "Notes";
  public static final String MERCHANT_CODE = "Merchant Code";
  public static final String CONFIG_CATEGORY_CODE = "categoryCode";
  public static final String CONFIG_CATEGORY_NAME = "categoryName";
  public static final String REVIEW_CONFIG = "status";
  public static final String ERROR_CODE = "Error Code";
  public static final String PICKUP_POINT_CODE = "Kode Lokasi";
  public static final String PICKUP_POINT_NAME = "Lokasi";
  public static final String PICKUP_POINT_NAME_NON_EDITABLE = "Nama Toko/Gudang (non-editable)";
  public static final String LIST_PRICE = "Harga Normal";
  public static final String OFFER_PRICE = "Harga Jual";
  public static final String SELLING_PRICE_DESC =
      "Diisi dengan harga jual after discount. Hanya boleh diisi angka.Harga produk promo tidak bisa diubah.";

  public static final String STOCK = "Stok";
  public static final String STOCK_DESC = "Diisi dengan stok saat ini. Hanya boleh diisi angka.";
  public static final String PO_QUOTA_DESCRIPTION = "Diisi dengan quota saat ini. Hanya boleh diisi angka.";
  public static final String STOCK_REMINDER_ID = "Pengingat stok";
  public static final String STOCK_REMINDER_DESC = "Diisi dengan angka 0, 1, 2 dst untuk mendapatkan pengingat stok.";
  public static final String WARNA_CN_CATEGORY_MAPPING = "WARNA_CN_CATEGORY_MAPPING";
  public static final String DESCRIPTIVE_ATTR_CN_CATEGORY_MAPPING = "DESCRIPTIVE_ATTR_CN_CATEGORY_MAPPING";
  public static final String NAMA_PRODUK = "Nama Produk";
  public static final String HARGA_NORMAL = "Harga Normal";
  public static final String HARGA_JUAL = "Harga Jual";
  public static final String POTONGAN_HARGA = "Potongan Harga";
  public static final String PERSENTASE_DISKON = "Persentase Diskon";
  public static final String STOK_TERSEDIA = "Stok Tersedia";
  public static final String HARGA_REKOMENDASI = "Harga Rekomendasi";
  public static final String REKOMENDASI = "Rekomendasi";
  public static final String HARGA_AKHIR = "Harga Akhir";
  public static final String KUOTA = "Kuota";
  public static final String BULK_CAMPAIGN_TITLE = "Blibli Campaign Bulk Upload";
  public static final String DASHBOARD_ROW_1 = "Anda memberikan diskon lebih dari 80% untuk produk tertentu (berwarna"
      + " merah di kolom \"Persentase Diskon\"). Silakan cek kembali dan lanjutkan jika memang tiap jumlah diskon"
      + " sudah benar";
  public static final String DASHBOARD_ROW_2 = "Anda menentukan harga jual terlalu rendah untuk produk tertentu "
      + "(berwarna oranye di kolom \"Harga Akhir\"). Silakan cek kembali dan lanjutkan jika memang tiap harga jual "
      + "sudah benar.";
  public static final String BULK_CAMPAIGN_INSTRUCTION = "Info yang ditampilkan merupakan info saat Anda men-download "
      + "file ini. Pastikan detail produk dan harga jual sudah sesuai dengan harga jual di web. Harga akhir yang "
      + "dimasukkan akan digunakan sebagai harga selama periode promo.";
  public static final String HIGH_DISCOUNT = "High Discount";
  public static final String LOW_PRICE = "Low Price";
  public static final String SALE_PRICE = "salePrice";
  public static final String NORMAL_SELLING_PRICE = "NormalSellingPrice";
  public static final String KOMENTAR = "Komentar";
  public static final String CONFIG_SELLER_NAME = "sellerName";
  public static final String CONFIG_SELLER_CODE = "sellerCode";
  public static final String VARIASI = "Variasi";
  public static final String VARIASI_CN_CATEGORY_MAPPING = "VARIASI_CN_CATEGORY_MAPPING";
  public static final String SHIPPING_TYPE_LIST = "SHIPPING_TYPE_LIST";
  public static final String ITEM_SKU = "Sku";
  public static final String REASON = "REASON";
  public static final String ALASAN_KEGAGALAN = "Alasan Kegagalan";
  public static final String NO_BRAND = "no brand";
  public static final String OEM = "OEM";
  public static final List<String> BULK_OPTION_UPLOAD_SUPPORT = Arrays
      .asList(BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription(),
          BulkUploadOption.SHIPPING_TYPE_SPECIAL_SHIPPING.getdescription(),
          BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
  public static final List<String> BULK_OPTION_UPLOAD_SUPPORT_EN = Arrays
      .asList(BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI_EN.getdescription(),
          BulkUploadOption.SHIPPING_TYPE_SPECIAL_SHIPPING_EN.getdescription(),
          BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription());
  public static final List<String> DEFAULT_BRANDS = Arrays.asList(NO_BRAND, OEM);

  public static final int HEADER_START_ROW = 0;
  public static final int HEADER_END_ROW = 4;
  public static final int USER_DATA_START_ROW = 4;
  public static final String IS_ONLY_EXTERNAL_USER = "isOnlyExternalUser";
  public static final String IN_STORE_HEADER = "Instore";
  public static final String IN_STORE_HEADER_DESC =
      "Status Instore saat ini: 0 = Offline, 1 = Online. Tidak akan mengubah data yang ada saat "
          + "ini.";
  public static final String AMPHI_SKU_STATUS = "Status/pengiriman(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)";
  public static final String EXTERNAL_SKU_STATUS = "Status/pengiriman(0 = Offline, 1 = Online)";
  public static final String SKU_STATUS_OFFLINE = "0";
  public static final String SKU_STATUS_ONLINE = "1";
  public static final String SKU_STATUS_TEASER = "2";
  public static final String SKU_STATUS_B2B = "3";
  public static final int PICKUP_POINT_DEFAULT_COLUMN_INDEX = 3;
  public static final int HEADER_ROW_HEIGHT = 48;
  public static final String AMPHI_SKU_STATUS_NEW = "Status(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)";
  public static final String EXTERNAL_SKU_STATUS_NEW = "Status(0 = Offline, 1 = Online)";
  public static final String AMPHI_STATUS_HEADER_DESC =
      "Diisi dengan angka sesuai status produk: 0 = Offline, 1 = Online,2 = Akan datang, 3 = "
          + "Dibeli via link Untuk status 1, 2, dan 3, aktifkan pengiriman atau Click and Collect";

  public static final String PURE_EXTERNAL_STATUS_HEADER_DESC =
      "Diisi dengan angka sesuai status produk: 0 = Offline, 1 = Online";



  public static final String OFFLINE_VALUE = "0.0";
  public static final String ONLINE_VALUE = "1.0";
  public static final String TEASER_VALUE = "2.0";
  public static final String B2B_VALUE = "3.0";
  public static final Set<String> POSSIBLE_ONLINE_VALUES = ImmutableSet.of("1","1.0");
  public static final Set<String> POSSIBLE_OFFLINE_VALUES = ImmutableSet.of("0","0.0");
  public static final String MANDATORY = "(Wajib)";
  public static final String OPTIONAL = "(Opsional)";
  public static final String NOT_EDITABLE = "Tidak bisa diedit";
  public static final String EDITABLE = "Bisa diedit";

  public static final String PRODUCT_CODE_EN = "ProductCode";
  public static final String PRODUCT_NAME_EN = "ProductName";
  public static final String MASTER_CATEGORY_CODE = "MasterCategoryCode";
  public static final String MASTER_CATEGORY_NAME = "MasterCategoryName";
  public static final String NEW_MASTER_CATEGORY_CODE = "NewMasterCategoryCode";
  public static final String NEW_MASTER_CATEGORY_NAME = "NewMasterCategoryName";
  public static final String COPY_PRODUCT_NAME = "Copied Product name (Please use same name for all variants)";
  public static final String MINIMUM_STOCK_HEADER = "Minimum Stock";
  public static final String SHIPPING_TYPE_HEADER = "Shipping Type";
  public static final String ITEM_SKU_PP_CODE = "item-sku-pp-code";
  public static final String ITEM_SKU_PP_CODE_EN = "ITEM-SKU-PP-CODE";

  public static final int PICKUP_POINT_DEFAULT_COLUMN_INDEX_IN_COPY_STORE = 11;
  public static final String PRODUCT_SKU_REQUEST = "productSku";
  public static final String ITEM_SKU_REQUEST = "itemSku";
  public static final String STATE_REQUEST = "state";
  public static final String PICKUP_POINT_CODE_REQUEST = "pickupPointCode";
  public static final String NEW_PICKUP_POINT_CODE_REQUEST = "newPickupPointCode";
  public static final String BUSINESS_PARTNER_CODE_REQUEST = "businessPartnerCode";
  public static final String IN_PROGRESS = "IN_PROGRESS";
  public static final String ACTIVE = "ACTIVE";
  public static final String FAILURE_REASON = "reason";
  public static final String SYSTEM_ERROR = "System error";

  public static final String AMPHI_BFB_STATUS = "Bfb Status(0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli via link)";
  public static final String EXTERNAL_BFB_STATUS = "Bfb Status(0 = Offline, 1 = Online)";
  public static final String BFB_STATUS_AND_MANAGED_HEADER_DESC =
      "Diisi dengan angka sesuai status BFB: 0 = Offline, 1 = Online";
  public static final String AMPHI_BFB_STATUS_AND_MANAGED_HEADER_DESC =
      "Diisi dengan angka sesuai status BFB: 0 = Offline, 1 = Online, 2 = Akan datang, 3 = Dibeli"
          + " via link)";
  public static final String BFB_MANAGED = "Bfb Diatur(1 = Yes, 0=No)";
  public static final String BFB_BASE_PRICE = "Bfb Harga dasar";
  public static final String BFB_BASE_PRICE_DESC =
      "Diisi dengan harga jual sebelum diskon. Hanya boleh diisi angka.";
  public static final String BFB_MANAGED_TRUE = "1";
  public static final String BFB_MANAGED_FALSE = "0";
  public static final String AMPHI_BFB_STATUS_COLUMN_WIDTH = "7168";
  public static final Double BFB_MANAGED_TRUE_VALUE = 1.0;
  public static final String FIRST_ANCHOR_SKU_CODE = "Kode master SKU 1";
  public static final String SECOND_ANCHOR_SKU_CODE = "Kode master SKU 2";
  public static final String FIRST_ANCHOR_SKU_NAME = "Nama master SKU 1";
  public static final String SECOND_ANCHOR_SKU_NAME = "Nama master SKU 2";
  public static final String SELLER_CODE = "Seller Code";
  public static final String SKU_ID = "SKU Id";
  public static final String PICKUP_POINT_CODE_FOR_TAGGING = "PP Code";
  public static final String CATEGORY_FOR_TAGGING = "Category";
  public static final String STORE_NAME_FOR_TAGGING = "Store Name";
  public static final String PRODUCT_TYPE_FOR_TAGGING = "Product Type";
  public static final String LAST_UPDATED = "Last Updated On";
  public static final Collection<String> BULK_INTERNAL_PRICE_UPDATE_HEADER =
      Set.of(SELLER_CODE, BLIBLI_PRODUCT_SKU, PARENT_PRODUCT_NAME, BLIBLI_SKU, PICKUP_POINT_HEADER, PRODUCT_NAME,
          SKU_CODE_HEADER, SELLER_SKU, PRICE_HEADER, SELLING_PRICE_HEADER, STOCK_HEADER, AMPHI_SKU_STATUS_NEW, DELIVERY_STATUS_HEADER,
          CNC_STATUS_HEADER, IN_STORE_HEADER, WAREHOUSE_STOCK_HEADER);

  public static final String MONTH = "Month";
  public static final String YEAR = "Year";
  public static final String STORE_ID = "Store ID";
  public static final String MAIN_CATEGORY_CODE = "Main Category Code";
  public static final String PROJECTED_REBATE = "Projected Rebate";

  public static final Collection<String> BULK_REBATE_UPLOAD_HEADERS =
      Set.of(MONTH, YEAR, MAIN_CATEGORY_CODE, CATEGORY_CODE, STORE_ID, BRAND, PROJECTED_REBATE);

  public static final String ITEM_SKU_PRODUCT_TAGGING = "SKU Id";
  public static final String PICKUP_POINT_CODE_PRODUCT_TAGGING = "PP Code";
  public static final String PRODUCT_TYPE_PRODUCT_TAGGING = "Product Type";
  public static final String DELETE_PRODUCT_TAGGING = "Delete Tagging";
  public static final Collection<String> BULK_PRICING_PRODUCT_TYPE_UPLOAD_HEADERS =
    Set.of(ITEM_SKU_PRODUCT_TAGGING, PICKUP_POINT_CODE_PRODUCT_TAGGING, PRODUCT_TYPE_PRODUCT_TAGGING, DELETE_PRODUCT_TAGGING);

  public static final String D_1_DAY_DATA = "\n(D-1 day data)";
  public static final String LAST_CALENDAR_MONTH = "\n(Last calendar month)";
  public static final String SKU_ID_PRICE_RECOMMENDATION = "SKU Id";
  public static final String PP_CODE = "PP Code";
  public static final String PP_NAME = "PP Name";
  public static final String SKU_NAME_PRICE_RECOMMENDATION = "SKU Name";
  public static final String STORE_NAME_PRICE_RECOMMENDATION = "Store Name";
  public static final String MAIN_CATEGORY = "Main Category";
  public static final String CATEGORY_PRICE_RECOMMENDATION = "Category";
  public static final String BRAND_PRICE_RECOMMENDATION = "Brand";
  public static final String PRICING_PRODUCT_TYPE = "Pricing Product Type";
  public static final String NORMAL_PRICE = "Normal Price";
  public static final String NEW_NORMAL_PRICE = "New Normal Price";
  public static final String MIN_DESIRED_MARGIN = "Min Desired Margin";
  public static final String MAX_DESIRED_MARGIN = "Max Desired Margin";
  public static final String COGS_SKU = "COGS/qty" + D_1_DAY_DATA;
  public static final String INCLUSIVE_OF_VAT = "Inclusive of VAT?";
  public static final String REBATE_SKU = "Rebate/qty";
  public static final String REBATE_PROJECTION_BREAKDOWN = "Rebate Projection Breakdown";
  public static final String COGS_REBATE = "COGS-Rebate";
  public static final String SELLING_PRICE = "Selling Price";
  public static final String NEW_SELLING_PRICE = "New Selling Price";
  public static final String CURRENT_MARGIN = "Current Margin";
  public static final String NEW_MARGIN = "New Margin";
  public static final String SRP_MIN = "SRP Min";
  public static final String SRP_MAX = "SRP Max";
  public static final String PRICE_COMPLIANT = "Price Compliant?";
  public static final String BML = "BML Best Price" + D_1_DAY_DATA;
  public static final String BML_BEST_PRICE = "BML Best Price";
  public static final String IS_SKU_PART_OF_CAMPAIGN = "Is SKU part of campaign/FS?";
  public static final String SELLING_PRICE_RANGE = "Campaign Period Allowed Selling Price Range";
  public static final String TOTAL_ORDERS = "Total Orders" + LAST_CALENDAR_MONTH;
  public static final String TOTAL_TPV = "Total TPV" + LAST_CALENDAR_MONTH;
  public static final String L5_STOCK = "L5 Stock" + D_1_DAY_DATA;
  public static final String L2_STOCK = "L2 Stock" + D_1_DAY_DATA;
  public static final String L2_DAYS_OF_INVENTORY = "L2 Days of Inventory" + D_1_DAY_DATA;
  public static final String L2_TARGET_DAYS_OF_INVENTORY = "L2 Target Days of Inventory" + D_1_DAY_DATA;
  public static final String IPR_PRODUCT_SKU = "SKU produk";
  public static final String IPR_SOURCE = "Sumber";
  public static final String CAMPAIGN_ID = "Campaign Id";
  public static final String CAMPAIGN_NAME = "Campaign Name";
  public static final String CAMPAIGN_PRICE = "Campaign Price";
  public static final String CURRENT_CAMPAIGN_MARGIN = "Current Campaign Margin";
  public static final String NEW_CAMPAIGN_PRICE = "New Campaign Price";
  public static final String NEW_CAMPAIGN_MARGIN = "New Campaign margin";
  public static final String CAMPAIGN_PRICE_RANGE = "Campaign Period Allowed Campaign Price Range";
  public static final String CLOSEST_EXPIRATION_DATE = "Closest Expiration Date";
  public static final String EXPIRING_STOCK = "Expiring Stock";

  public static final List<String> PRICE_RECOMMENDATION_HEADERS =
      ImmutableList.<String>builder().add(SKU_ID_PRICE_RECOMMENDATION).add(PP_CODE).add(PP_NAME)
          .add(SKU_NAME_PRICE_RECOMMENDATION).add(STORE_NAME_PRICE_RECOMMENDATION)
          .add(MAIN_CATEGORY).add(CATEGORY_PRICE_RECOMMENDATION).add(BRAND_PRICE_RECOMMENDATION)
          .add(PRICING_PRODUCT_TYPE).add(NORMAL_PRICE).add(NEW_NORMAL_PRICE).add(MIN_DESIRED_MARGIN)
          .add(MAX_DESIRED_MARGIN).add(COGS_SKU).add(INCLUSIVE_OF_VAT).add(REBATE_SKU).add(REBATE_PROJECTION_BREAKDOWN)
          .add(COGS_REBATE).add(SELLING_PRICE).add(CURRENT_MARGIN).add(NEW_SELLING_PRICE)
          .add(NEW_MARGIN).add(SRP_MIN).add(SRP_MAX)
          .add(PRICE_COMPLIANT).add(BML).add(BML_BEST_PRICE).add(IS_SKU_PART_OF_CAMPAIGN)
          .add(CAMPAIGN_ID).add(CAMPAIGN_NAME).add(CAMPAIGN_PRICE).add(CURRENT_CAMPAIGN_MARGIN).add(NEW_CAMPAIGN_PRICE)
          .add(NEW_CAMPAIGN_MARGIN).add(SELLING_PRICE_RANGE)
          .add(CAMPAIGN_PRICE_RANGE).add(TOTAL_ORDERS).add(TOTAL_TPV).add(L5_STOCK).add(CLOSEST_EXPIRATION_DATE)
          .add(EXPIRING_STOCK).add(L2_STOCK).add(L2_DAYS_OF_INVENTORY).add(L2_TARGET_DAYS_OF_INVENTORY).build();

  public static final String SKU_LEVEL_REBATE_SKU_ID = "SKU Id";
  public static final String SKU_LEVEL_REBATE_PICKUP_POINT_CODE = "PP Code";
  public static final String SKU_LEVEL_REBATE_NEW_REBATE_VALUE = "New Rebate Value";
  public static final Collection<String> BULK_SKU_LEVEL_REBATE_UPLOAD_HEADERS =
    Set.of(SKU_LEVEL_REBATE_SKU_ID, SKU_LEVEL_REBATE_PICKUP_POINT_CODE, SKU_LEVEL_REBATE_NEW_REBATE_VALUE);

  public static final String BULK_PRICE_UPDATE_SKU_ID = "SKU Id";
  public static final String BULK_PRICE_UPDATE_PICKUP_POINT_CODE = "PP Code";
  public static final String BULK_PRICE_UPDATE_NEW_NORMAL_PRICE = "New Normal Price";
  public static final String BULK_PRICE_UPDATE_NEW_SELLING_PRICE = "New Selling Price";
  public static final String BULK_PRICE_UPDATE_CAMPAIGN_ID = "Campaign Id";
  public static final String BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE = "New Campaign Price";
  public static final String BULK_PRICE_UPDATE_RESULT = "Result";
  public static final String PICKUP_POINT_NAME_COLUMN_ID = "Nama alamat pengambilan";
  public static final List<String> BULK_PRICE_UPDATE_NEW_UPLOAD_HEADERS = List.of(BULK_PRICE_UPDATE_SKU_ID,
      BULK_PRICE_UPDATE_PICKUP_POINT_CODE,
      BULK_PRICE_UPDATE_NEW_NORMAL_PRICE,
      BULK_PRICE_UPDATE_NEW_SELLING_PRICE,
      BULK_PRICE_UPDATE_CAMPAIGN_ID,
      BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE);

  public static final Set<String> BULK_PRICE_UPDATE_NEW_MANDATORY_FIELD_HEADERS =
      Set.of(BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, BULK_PRICE_UPDATE_NEW_SELLING_PRICE,
          BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE);
  public static final String OLD_EAN_UPC_CODE = "OLD_EAN_UPC_CODE";

  private BulkParameters() {
  }
  public static final Set<String> POSSIBLE_BOOLEAN_HEADER_VALUES = ImmutableSet.of("1", "1.0", "0", "0.0");

  // Bulk basic info headers
  public static final String BLIBLI_PRODUCT_SKU_HEADER = "SKU produk";
  public static final String PARENT_PRODUCT_NAME_HEADER = "Nama produk parent";
  public static final String INSTORE = "Instore";
  public static final String BRAND_HEADER = "Brand";
  public static final String CATEGORY_HEADER = "Kategori";
  public static final String DESCRIPTIONS = "Deskripsi";
  public static final String MAIN_PHOTO = "Foto utama";
  public static final String COMMON_PHOTO_2 = "Foto umum 2";
  public static final String COMMON_PHOTO_3 = "Foto umum 3";
  public static final String COMMON_PHOTO_4 = "Foto umum 4";
  public static final String COMMON_PHOTO_5 = "Foto umum 5";
  public static final String COMMON_PHOTO_6 = "Foto umum 6";
  public static final String COMMON_PHOTO_7 = "Foto umum 7";
  public static final String COMMON_PHOTO_8 = "Foto umum 8";
  public static final String VIDEO_URL = "Video";
  public static final String SHIPPING_TYPE = "Tipe pengiriman";
  public static final String LENGTH_HEADER = "Panjang";
  public static final String WIDTH_HEADER = "Lebar";
  public static final String HEIGHT_HEADER = "Tinggi";
  public static final String ACTUAL_WEIGHT = "Berat aktual";
  public static final String SHIPPING_WEIGHT = "Berat pengiriman";
  public static final String SIZE_CHART = "Panduan ukuran";
  public static final String VOLUME_FACTOR = "Volume factor";
  public static final String ERROR_COLUMN = "Penyebab Error";
  public static final String SHIPPED_BY_BLIBLI = "Dikirim oleh blibli";
  public static final String SHIPPED_BY_SELLER = "Dikirim oleh seller";
  public static final String BOPIS = "Produk non-fisik";

  public static final List<String> BASIC_INFO_BASE_HEADER_LIST =
      ImmutableList.<String>builder().add(BLIBLI_PRODUCT_SKU_HEADER).add(PARENT_PRODUCT_NAME_HEADER).add(BRAND_HEADER)
          .add(CATEGORY_HEADER).add(DESCRIPTIONS).add(MAIN_PHOTO).add(COMMON_PHOTO_2).add(COMMON_PHOTO_3)
          .add(COMMON_PHOTO_4).add(COMMON_PHOTO_5).add(COMMON_PHOTO_6).add(COMMON_PHOTO_7).add(COMMON_PHOTO_8)
          .add(VIDEO_URL).add(SHIPPING_TYPE).add(LENGTH_HEADER).add(WIDTH_HEADER).add(HEIGHT_HEADER).add(ACTUAL_WEIGHT)
          .add(SHIPPING_WEIGHT).add(SIZE_CHART).add(VOLUME_FACTOR).build();

  public static List<String> getBasicInfoHeaderList(boolean isInstore, boolean includeError) {
    List<String> headers = new ArrayList<>(BASIC_INFO_BASE_HEADER_LIST);
    if (isInstore) {
      headers.add(2, INSTORE);
    }
    if (includeError) {
      headers.add(ERROR_COLUMN);
    }
    return ImmutableList.copyOf(headers);
  }

  public static final List<String> ALLOWED_SHIPPING_HEADERS =
      Arrays.asList(SHIPPED_BY_BLIBLI, SHIPPED_BY_SELLER, BOPIS);

  public static final List<String> SHIPPING_HEADERS =
      ImmutableList.<String>builder().add(BulkParameters.LENGTH_HEADER).add(BulkParameters.WIDTH_HEADER)
          .add(BulkParameters.HEIGHT_HEADER).add(BulkParameters.ACTUAL_WEIGHT).add(BulkParameters.SHIPPING_WEIGHT)
          .add(BulkParameters.VOLUME_FACTOR).build();

  public static final List<String> BASIC_INFO_IMAGE_HEADERS =
      ImmutableList.<String>builder().add(BulkParameters.MAIN_PHOTO).add(BulkParameters.COMMON_PHOTO_2)
          .add(BulkParameters.COMMON_PHOTO_3).add(BulkParameters.COMMON_PHOTO_4).add(BulkParameters.COMMON_PHOTO_5)
          .add(BulkParameters.COMMON_PHOTO_6).add(BulkParameters.COMMON_PHOTO_7).add(BulkParameters.COMMON_PHOTO_8)
          .build();

  public static final String BASIC_INFO_UPDATE_ERROR_MESSAGE = "Berisi informasi mengapa produk di baris tersebut gagal di proses. Abaikan kolom ini saat upload pertama kali.";

}
