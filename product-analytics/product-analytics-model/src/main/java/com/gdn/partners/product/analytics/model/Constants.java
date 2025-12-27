package com.gdn.partners.product.analytics.model;

import java.util.Arrays;
import java.util.List;

import com.google.common.collect.ImmutableList;

public interface Constants {


  String JOB_NOT_EXISTS = "Requested job no longer exists";

  String SELLER_INFO_BQ_JOB = "SELLER_INFO_BQ_JOB";

  String STATUS_NOT_EXISTS = "Status of requested job is null";

  String SLASH_SEPARATOR = "/";

  String UNDERSCORE_SEPARATOR = "_";

  String PROCESS_LOCKED = "Lock is acquired by another process";

  String WILDCARD = "*";

  String GOOGLE_CLOUD_STORAGE_URI_PREFIX = "gs://";

  String PROJECT_ID = "projectId";
  String DATASCIENCE_PROJECT_ID = "datascienceProjectId";

  String PRODUCT_ATTRIBUTE_EXTRACTIONS_BQ_TABLE = "productAttributeExtractionsBqTable";

  String PRD_PRODUCT_BUSINESS_PARTNER_BQ_TABLE = "prdProductBusinessPartnerBqTable";

  String COMMA = ",";

  List<String> DIAMOND_OR_GOLD_SELLERS_LIST = ImmutableList.of("Diamond Merchant","Gold Merchant");

  List<String> BRONZE_OR_SILVER_SELLERS = Arrays.asList("Bronze Merchant", "Silver Merchant");

  String NO_BADGE_SELLERS = "None Merchant";
  String OFFICIAL_STORES = "Official Stores";
  String MARK_FOR_DELETE = "markForDelete";
  String VERSION = "version";
  String CREATED_DATE = "createdDate";
  String UPDATED_DATE = "updatedDate";
  String CREATED_BY = "createdBy";
  String UPDATED_BY = "updatedBy";
  String STORE_ID = "storeId";
  String FULL_FETCH_TEMPLATE = "templates/fullFetch.vm";
  String FULL_FETCH_SELLER_SPECIFIC_TEMPLATE = "templates/sellerSpecificFullFetch.vm";
  String FULL_FETCH_SELLER_ANALYTICS_TEMPLATE = "templates/sellerAnalyticsFullFetch.vm";
  String AUTO_APPROVED_PRODUCTS_TEMPLATE = "templates/autoApprovedProducts.vm";
  String IPR_PRODUCTS_TEMPLATE = "templates/iprProducts.vm";
  String PRODUCT_OPTIMISATION_TEMPLATE = "templates/productOptimisation.vm";
  String PRODUCT_ATTRIBUTE_EXTRACTIONS_DELTA_TEMPLATE =
      "templates/productAttributeExtractionsDelta.vm";
  String PRODUCT_ATTRIBUTE_EXTRACTIONS_BY_ATTRIBUTE_NAME =
      "templates/productAttributeExtractionsByAttributeName.vm";
  String RECORD_FETCH_SIZE = "recordFetchSize";
  String GENEVA_PROJECT_ID = "genevaProjectId";
  String USERNAME = "System";
  String UNASSIGNED_FILTER = "UNASSIGNED";
  String STORE_ID_VALUE = "10001";
  String THUMBNAIL_IMAGE_PREFIX = "/thumbnail/";
  String FULL_IMAGE_PREFIX = "/full/";
  String MEDIUM_IMAGE_PREFIX = "/medium/";
  String RESIZE = "/resize/";
  Integer THOUSAND = 1000;
  String FETCH_HOUR_THRESHOLD = "fetchHourThreshold";
  String PRODUCT_OPTIMISATION_DS_TABLE = "productOptimisationDsTable";
  String SUGGESTED_DATE_FETCH = "suggestedDateFetch";
  String SUSPEND_FLAG_CHANGE = "SUSPEND_FLAG_CHANGE";
  String ARCHIVE_FLAG_CHANGE = "ARCHIVE_FLAG_CHANGE";
  String FORCE_REVIEW_FLAG_CHANGE = "FORCE_REVIEW_FLAG_CHANGE";
  String PRODUCT_REJECTED = "PRODUCT_REJECTED";
  String UTILITY_CLASS_ERROR = "This is a utility class and cannot be instantiated";
  String IPR_PRODUCT_FETCH_TABLE = "iprProductFetchTable";
  String IPR_BIG_QUERY_PROJECT_ID = "iprBigQueryProjectId";
  Long ZERO = 0l;
  String DS_EXTRACTION = "DS_EXTRACTION";
  String ATTRIBUTE_NAME = "attributeName";
  String AUTO_APPROVAL_FETCH_HOUR = "autoApprovalFetchHour";
}
