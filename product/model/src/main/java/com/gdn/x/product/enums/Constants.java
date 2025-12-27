package com.gdn.x.product.enums;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public interface Constants {

  String DEFAULT_UPDATED_BY = "System";
  String CREATED_DATE = "createdDate";
  String PRODUCT_CODE = "productCode";
  String STORE_ID = "storeId";
  String DATE_FORMAT = "dd/MM/yyyy";
  String NOT_APPLICABLE = "NA";

  String DEFAULT_CLIENT_ID = "pbp";
  String DEFAULT_CLIENT_ID_X_PRODUCT = "x-product";
  String DEFAULT_CHANNEL_ID = "web";
  String DEFAULT_USERNAME = "system";
  String DEFAULT_STORE_ID = "10001";
  String DEFAULT_REQUEST_ID = "requestId";
  String CATEGORY_CACHE_MANAGER = "cacheManagerCategoryCombination";
  String UNIQUE_ID_CHECK_CACHE_MANAGER = "cacheManagerUniqueIdCheckCombination";
  String SYSTEM_PARAMETER_CACHE_MANAGER = "cacheManagerSystemParameterCombination";
  String PRODUCT_SKU_CACHE_MANAGER = "cacheManagerProductSkuCombination";
  String MASTER_DATA_CACHE_MANAGER = "cacheManagerMasterDataCombination";
  String PRODUCT_COUNT_CACHE_MANAGER = "cacheManagerProductCountsCombination";
  String SIZE_CHART_DETAIL_CACHE_MANAGER = "cacheManagerSizeChartDetailCombination";
  String ITEM_PICKUP_POINT_CACHE_MANAGER = "cacheManagerItemPickupPointCombination";
  String HYPHEN = "-";
  String COLON = ":";
  String CATEGORY_CODE_VARIABLE = "categoryCodeForDisableUnSync";
  String COMMA_DELIMITER = ",";
  String WHOLESALE_PRICE = "WHOLESALE_PRICE";
  String WHOLESALE = "WHOLESALE";
  String COMBO = "COMBO";
  String PROMO_SCHEDULER = "PROMO_SCHEDULER";
  String IGNORE_SYMBOLS_VARIABLE_NAME = "ignore_symbol_list";
  String IGNORE_ATTRIBUTE_VARIABLE_NAME = "ignore_attribute_list";
  double ROUND_OFF_FACTOR = 100D;
  String DELIMETER = "#_#";
  String SUSPENDED = "SUSPENDED";
  String ARCHIVED = "ARCHIVED";
  String ACTIVE = "ACTIVE";
  String COMMA_SPLIT_REGEX = "\\s*,\\s*";
  String MASTER_CATALOG = "10001";
  String AVAILABLE = "AVAILABLE";
  int SINGLE_ROW = 1;
  String ONLINE = "ONLINE";
  String OFFLINE = "OFFLINE";
  String TEASER = "TEASER";
  String B2B = "B2B";
  String ACTION_KEY = "OFFLINE_TO_ONLINE";
  String ACCESS_CHANNEL = "accesschannel";
  double DEFAULT_MAX_PRICE = -1;
  String DEFAULT = "DEFAULT";
  String CNC = "CNC";
  String DAYS = "DAYS";
  String WEEK = "WEEK";
  String DATE = "DATE";
  int NUM_OF_DAYS_IN_WEEK = 7;
  Long LONG_ZERO = 0L;
  Double DOUBLE_ZERO = 0.0;

  String REGULAR_MERCHANT_CLIENT_ID = "Regular-Merchant-Client-Id";
  String ALL = "ALL";
  String SALES = "SALES";
  String MASTER = "MASTER";
  String NONE = "NONE";
  String PICKUP_POINT_CHANGE = "pickup-point";

  String OXFORD_CLIENT_ID = "oxford";
  String COMPLEMENTARY_FREE_SAMPLE_PRODUCT = "COMPLEMENTARY_FREE_SAMPLE_PRODUCT";

  String OFF_2ON_ACTIVE_CHANGED = "off2OnActiveChanged";
  String FREE_SAMPLE_CHANGED = "freeSampleChanged";
  String SIZE_CHART_CHANGED = "sizeChartChanged";

  String PREORDER_DETAILS_CHANGED = "preOrderDetailsChanged";
  String CATEGORY_HIERARCHY_DELIMITER = " < ";
  String PRODUCT_DETAIL_PAGE_LINK = "/p/%s/ps--%s";
  String SPACE = " ";

  List<String> GUARANTEE_ATTRIBUTE_NAME =
      Collections.unmodifiableList(new ArrayList<String>(Arrays.asList("garansi", "guarantee")));
  List<String> GUARANTEE_DURATION_ATTRIBUTE_NAME =
      Collections.unmodifiableList(new ArrayList<String>(Arrays.asList("lama garansi")));

  String DESC = "DESC";
  String ITEM_SKU = "itemSku";
  String OFFLINE_ITEM_ID = "offlineItemId";
  String DOT = ".";
  String DEFAULT_CHANNEL = "DEFAULT";
  String DASH ="-" ;

  String FETCH_ONLY_IMAGES_DETAILS = "fetchOnlyImagesDetails";
  double DEFAULT_PRICE = 0;

  String DEFINING_ATTRIBUTE = "DEFINING_ATTRIBUTE";
  String CNC_FLAG_UPDATE = "CNC";
  String BUYABLE_FALG_UPDATE = "BUYABLE";
  String DISPLAYABLE_FLAG_UPDATE = "DISPLAYABLE";
  String PICKUP_POINT_UPSERT = "PICKUP_POINT_ADDED";
  String SELLING_PRICE_UPDATE = "SELLING_PRICE";
  String NORMAL_PRICE_UPDATE = "NORMAL_PRICE";
  String PRODUCT_ARCHIVE = "ARCHIVE";
  String PICKUP_POINT_DELETED = "PICKUP_POINT_DELETED";
  String PICKUP_POINT_UPDATED = "PICK_POINT_CODE";
  String PICKUP_POINT_ADDED = "PICKUP_POINT_ADDED";
  String PICKUP_POINT_AUTO_CREATED = "New pickup point added";
  String B2B_PRICE = "B2B_PRICE";
  String B2B_MANAGED = "B2B_MANAGED";
  String B2B_BUYABLE = "B2B_BUYABLE";
  String B2B_DISCOVERABLE = "B2B_DISCOVERABLE";

  String B2C_ACTIVATED = "B2C_ACTIVATED";
  String B2B_ACTIVATED = "B2B_ACTIVATED";
  String APPROVE_HALAL_PRODUCT = "Approve product";
  String REJECT_HALAL_PRODUCT = "Reject product";
  String EDIT_HALAL_PRODUCT = "Edit halal flag";

  String DATE_TIME_FORMAT = "dd/MM/yyyy HH:mm:ss";
  String LENGTH_CHANGE = "LENGTH_CHANGE";
  String WIDTH_CHANGE = "WIDTH_CHANGE";
  String HEIGHT_CHANGE = "HEIGHT_CHANGE";
  String WEIGHT_CHANGE = "WEIGHT_CHANGE";
  String SHIPPING_WEIGHT_CHANGE = "SHIPPING_WEIGHT_CHANGE";
  String UPC_CODE_CHANGE = "Ubah upc code";

  String FBB_ACTIVATED = "fbbActivated";
  int SINGLE_VARIANT_L4_COUNT = 1;

  String UPC_MIGRATION = "UPC_MIGRATION";
  String SALES_CATEGORY_CATALOG_CODE = "12051";
  String RETAIL = "RETAIL";
  String B2C_SELLER_CHANNEL = "BLIBLI";
  String B2B_SELLER_CHANNEL = "BLIBLI FOR BUSINESS";
  String LENGTH = "Ubah panjang";
  String WIDTH = "Ubah lebar";
  String HEIGHT = "Ubah tinggi";
  String WEIGHT = "Ubah berat";
  String WAREHOUSE_USERNAME = "warehouse-system";
  String MERCHANT_STATUS_ACTIVE = "ACTIVE";
  int ONE = 1;
  int INT_ZERO = 0;
  int TWO = 2;
  String LOCALE_LANGUAGE = "in";
  String LOCALE_COUNTRY = "ID";
  String COMMA = ",";
  String BRAND = "brand";
  String PRODUCT_CACHE_MANAGER = "cacheManagerProductCombination";
  String BUYABLE_SCHEDULE_ACTION = "BUYABLE_SCHEDULE";
  String DISCOVERABLE_SCHEDULE_ACTION = "DISCOVERABLE_SCHEDULE";
  String SOURCE_PRODUCT_PUBLISH = "PRODUCT_PUBLISH";
  String SOURCE_SUSPENSION_FLOW = "suspendProduct";
  String SOURCE_PRODUCT_ARCHIVAL = "archiveProduct";
  String SOURCE_PRODUCT_REJECTION = "rejectProduct";
  String DIMENSIONS_MISSING = "dimensionsMissing";
  String DESCRIPTION_MISSING = "descriptionMissing";
  String ZERO = "0";

  String YES = "YES";

  String VIDEO_ADD_ACTION_KEY = "Upload video";
  String VIDEO_UPDATE_ACTION_KEY = "Edit video";
  String VIDEO_DELETE_ACTION_KEY = "Hapus video";
  String REGULAR = "Dikirim oleh blibli";
  String BIG_PRODUCT = "Dikirim oleh seller";
  String BOPIS = "Produk non-fisik";
  String PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT = "DELETE_PICKUP_POINT";
  String PRODUCT_SKU = "productSku";

  String PWP_MAIN_PENDING = "PWP_MAIN_PENDING";
  String PWP_MAIN_ACTIVE = "PWP_MAIN_ACTIVE";
  String PWP_ADDITIONAL_PENDING = "PWP_ADDITIONAL_PENDING";
  String PWP_ADDITIONAL_ACTIVE = "PWP_ADDITIONAL_ACTIVE";
  String PRODUCT_TYPE_CHANGED = "Ubah tipe produk";

  String SIZE_CHART_DELETED = "Size Chart has been Deleted";
  String SIZE_CHART_ADDED = "Size Chart has been Added";
  String SIZE_CHART_UPDATED = "ubah size chart";
  String OFF_2ON_CHANNEL_CHANGED = "Ubah offline to online";
  String UNDERSCORE = "_";

  // Ranch history Activities
  String OMNI_CHANNEL_SKU_UPDATED = "Omnichannel SKU updated";
  String NON_DISTRIBUTION = "NON_DISTRIBUTION";
}
