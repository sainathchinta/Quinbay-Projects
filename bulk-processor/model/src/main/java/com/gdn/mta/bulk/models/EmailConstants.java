package com.gdn.mta.bulk.models;

import com.gdn.mta.bulk.ProcessStatus;
import com.google.common.collect.ImmutableMap;

/**
 * Created by keshashah on 14/11/16.
 */
public final class EmailConstants {
  public static final String TEMPLATE_ID_PARAM = "templateId";
  public static final String MAIL_SUBJECT_PARAM = "subject";
  public static final String MAIL_SENDER_PARAM = "mailSender";
  public static final String NAME = "name";
  public static final String REQUEST_CODE = "requestCode";
  public static final String SELLER_CODE = "sellerCode";
  public static final String RECAT_PROCESS_LIST = "recatProcessList";
  public static final String TOTAL_COUNT = "totalCount";
  public static final String FILE_PREFIX = "filePrefix";
  public static final String DATE = "date";
  public static final String SUCCESS_COUNT = "successCount";
  public static final String FAILURE_COUNT = "failureCount";
  public static final String ERROR_FILE_PATH = "errorFilePath";
  public static final String REBATE_LISTING_URL = "rebateListingURL";
  public static final String SKU_LEVEL_REBATE_LISTING_URL = "skuLevelRebateListingURL";
  public static final String FILE_NAME = "fileName";
  public static final String MAX_LIMIT = "maxLimit";
  public static final String REQ_ID = "reqId";
  public static final String PRODUCT_DOWNLOAD_TEMPLATE_ID = "BULK_PRODUCT_DOWNLOAD";
  public static final String STORE_COPY_DOWNLOAD_TEMPLATE_ID = "BULK_STORE_COPY_DOWNLOAD";
  public static final String ORDER_DOWNLOAD_TEMPLATE_ID = "BULK_ORDER_DOWNLOAD";
  public static final String BULK_REVIEW_PRODUCT_DOWNLOAD_TEMPLATE_ID = "BULK_REVIEW_PRODUCT_DOWNLOAD";
  public static final String BULK_CAMPAIGN_PRODUCT_DOWNLOAD_TEMPLATE_ID = "BULK_CAMPAIGN_PRODUCT_DOWNLOAD";
  public static final String BULK_INSTANT_PICKUP_PRODUCT_DOWNLOAD_TEMPLATE_ID =
      "BULK_INSTANT_PICKUP_PRODUCT_DOWNLOAD";
  public static final String BULK_VENDOR_FILTERED_PRODUCT_DOWNLOAD_TEMPLATE_ID = "BULK_VENDOR_FILTERED_PRODUCT_DOWNLOAD";
  public static final String BULK_MERCHANT_CONFIGURATION_DOWNLOAD_TEMPLATE_ID = "BULK_MERCHANT_CONFIGURATION_DOWNLOAD";
  public static final String BULK_CATEGORY_CONFIGURATION_DOWNLOAD_TEMPLATE_ID = "BULK_CATEGORY_CONFIGURATION_DOWNLOAD";
  public static final String BULK_TAGGING_PRODUCTS_DOWNLOAD_TEMPLATE_ID = "BULK_TAGGING_PRODUCTS_DOWNLOAD_TEMPLATE_ID";
  public static final String RECAT_CREATED = "RECATEGORIZATION_REQUEST_CREATION";
  public static final String RECAT_SUCCESS = "RECATEGORIZATION_REQUEST_DONE";
  public static final String RECAT_PARTIAL_SUCCESS = "RECATEGORIZATION_PARTIAL_SUCCESS";
  public static final String RECAT_FAILED = "RECATEGORIZATION_FAILED";
  public static final String RECAT_DOWNLOAD = "RECATEGORIZATION_DOWNLOAD";
  public static final String RECAT_BULK_FAILURE =
      "RECATEGORIZATION_FAILED_BULK";
  public static final String RECAT_CREATED_SUBJECT = "[Recategorization] requestCode has created";
  public static final String RECAT_SUCCESS_SUBJECT = "[Recategorization] requestCode has completed";
  public static final String RECAT_PARTIAL_SUCCESS_SUBJECT = "[Recategorization] requestCode has partially succeed";
  public static final String RECAT_FAILED_SUBJECT = "[Recategorization] requestCode has failed";
  public static final String RECAT_DOWNLOAD_SUBJECT = "[Recategorization] requestCode file is ready";
  public static final String RECAT_BULK_FAILURE_SUBJECT =
      "[Recategorization] Some products are failed to recategorized";
  public static final String STORE_COPY_CREATION = "STORE_COPY_REQUEST_CREATION";
  public static final String STORE_COPY_SUCCESS = "STORE_COPY_REQUEST_DONE";
  public static final String STORE_COPY_PARTIAL_SUCCESS = "STORE_COPY_PARTIAL_SUCCESS";
  public static final String STORE_COPY_FAILED = "STORE_COPY_FAILED";
  public static final String DORMANT_SELLER_NOTIFICATION = "DORMANT_SELLER_STUCK";

  public static final String STORE_COPY_FILE_PREFIX = "x-bulk/storeCopy";
  public static final String STORE_COPY_GCS_FILE_PREFIX = "xbulk/download/store-copy";
  public static final String UPDATE_SALES_CATEGORY_CREATION = "UPDATE_SALES_CATEGORY_REQUEST_CREATION";
  public static final String UPDATE_SALES_CATEGORY_SUCCESS = "UPDATE_SALES_CATEGORY_REQUEST_DONE";
  public static final String UPDATE_SALES_CATEGORY_PARTIAL_SUCCESS = "UPDATE_SALES_CATEGORY_PARTIAL_SUCCESS";
  public static final String UPDATE_SALES_CATEGORY_FAILED = "UPDATE_SALES_CATEGORY_FAILED";
  public static final String STORE_COPY_CREATED_SUBJECT = "[Copy Product] requestCode has created";
  public static final String STORE_COPY_SUCCESS_SUBJECT = "[Copy Product] requestCode has completed";
  public static final String STORE_COPY_PARTIAL_SUCCESS_SUBJECT = "[Copy Product] requestCode has partially succeed";
  public static final String STORE_COPY_FAILED_SUBJECT = "[Copy Product] requestCode has failed";
  public static final String STORE_COPY_DOWNLOAD_SUBJECT = "[Copy Product] The file from [sellerCode] ready";
  public static final String UPDATE_SALES_CATEGORY_CREATED_SUBJECT = "[Sales category bulk update] requestCode has created";
  public static final String UPDATE_SALES_CATEGORY_SUCCESS_SUBJECT = "[Sales category bulk update] requestCode has completed";
  public static final String UPDATE_SALES_CATEGORY_PARTIAL_SUCCESS_SUBJECT = "[Sales category bulk update] requestCode has partially succeed";
  public static final String UPDATE_SALES_CATEGORY_FAILED_SUBJECT = "[Sales category bulk update]requestCode has failed";
  public static final String DORMANT_SELLER_STUCK_PROCESS = "Dormant Seller Process Stuck!!";
  public static final String PRODUCT_MAIL_SUBJECT = "PRODUCT_SUBJECT";
  public static final String ORDER_SUBJECT = "Order Download";
  public static final String MASTER_PRODUCT_SUBJECT = "Master product Download";
  public static final String MASTER_PRODUCT_DOWNLOAD_FILE_PREFIX = "x-bulk/bulk-download";
  public static final String UNCATEGORISED_SKU_DOWNLOAD_FILE_PREFIX = "x-bulk/downloadProducts";
  public static final String MASTER_PRODUCT_DOWNLOAD_GCS_FILE_PREFIX = "xbulk/download/bulk-download";

  public static final String REVIEW_PRODUCT_SUBJECT = "Review product Download";
  public static final String VENDOR_PRODUCT_SUBJECT = "Vendor product Download";
  public static final String CAMPAIGN_PRODUCT_SUBJECT = "Campaign Product Download";
  public static final String INSTANT_PICKUP_PRODUCT_SUBJECT = "Instant Pickup Product Download";
  public static final String MAIL_SENDER = "no-reply@blibli.com";
  public static final String SUSPENSION_PRODUCT__PARTIAL_SUBJECT = "Berhasil suspend sebagian produk";
  public static final String SUSPENSION_PRODUCT_SUBJECT = "Berhasil suspend semi produk";
  public static final String REACTIVATE_PRODUCT_PARTIAL_SUBJECT = "Berhasil mengatifkan kembali sebagian produk";
  public static final String REACTIVATE_PRODUCT_SUBJECT = "Berhasil mengatifkan kembali semua produk";
  public static final String BULK_PRODUCT_SUSPENSION_PARTIAL_ID = "BULK_PRODUCT_PARTIAL_SUSPENSION";
  public static final String BULK_PRODUCT_SUSPENSION_FILE_PREFIX = "x-bulk/product-suspension";
  public static final String BULK_CONFIGURATION_ERROR_FILE_PREFIX = "x-bulk/configuration-upload";
  public static final String BULK_PRODUCT_SUSPENSION_GCS_FILE_PREFIX = "xbulk/download/product-suspension";

  public static final String BULK_PRODUCT_SUSPENSION_ID = "BULK_PRODUCT_SUSPENSION";
  public static final String BULK_PRODUCT_REACTIVATE_PARTIAL_ID = "BULK_PRODUCT_PARTIAL_REACTIVATE";
  public static final String BULK_PRODUCT_REACTIVATE_ID = "BULK_PRODUCT_REACTIVATE";
  public static final String BULK_DOWNLOAD_ERROR = "Bulk Download Error";
  public static final String BULK_DOWNLOAD_ERROR_TEMPLATE = "BULK_DOWNLOAD_ERROR";
  public static final String CONFIGURATION_CATEGORY_SUBJECT = "Berhasil menambahkan konfirgurasi ke kategori";
  public static final String CONFIGURATION_CATEGORY_PARTIAL_SUBJECT = "Berhasil menambahkan konfirgurasi ke beberapa kategori";
  public static final String BULK_CONFIG_MERCHANT_ID = "BULK_CONFIG_MERCHANT";
  public static final String BULK_CONFIG_MERCHANT_PARTIAL_ID = "BULK_CONFIG_MERCHANT_PARTIAL";
  public static final String BULK_CONFIG_CATEGORY_ID = "BULK_CONFIG_CATEGORY";
  public static final String BULK_CONFIG_CATEGORY_PARTIAL_ID = "BULK_CONFIG_CATEGORY_PARTIAL";
  public static final String CONFIGURATION_SUMMARY_SELLER_SUBJECT = "Daftar status seller";
  public static final String CONFIGURATION_SUMMARY_CATEGORY_SUBJECT = "Daftar status kategori";
  public static final String CONFIGURATION_SELLER_SUBJECT = "Berhasil menambahkan konfirgurasi ke seller";
  public static final String CONFIGURATION_SELLER_PARTIAL_SUBJECT = "Berhasil menambahkan konfirgurasi ke beberapa seller";
    public static final String PRODUCT_CENTER_DOWNLOAD_EN = "PRODUCT_CENTER_DOWNLOAD_EN";
  public static final String PRODUCT_CENTER_DOWNLOAD_ID = "PRODUCT_CENTER_DOWNLOAD_ID";
  public static final String CATEGORY_NAME = "categoryName";
  public static final String PENDING_REQUESTS_TEMPLATE_ID = "BULK_PENDING_REQUESTS";
  public static final String PENDING_REQUESTS_SUBJECT = "Bulk Pending and Aborted Requests";

  public static final String RESTRICTED_KEYWORD_COMPLETED_TEMPLATE_ID = "RESTRICTED_KEYWORD_COMPLETED_TEMPLATE_ID";
  public static final String RESTRICTED_KEYWORD_PARTIALLY_COMPLETED_TEMPLATE_ID = "RESTRICTED_KEYWORD_PARTIALLY_COMPLETED_TEMPLATE_ID";
  public static final String RESTRICTED_KEYWORD_FAILED_TEMPLATE_ID = "RESTRICTED_KEYWORD_FAILED_TEMPLATE_ID";
  public static final String RESTRICTED_KEYWORD_COMPLETED_TEMPLATE_SUBJECT = "Mass keyword upload has been completed";
  public static final String RESTRICTED_KEYWORD_PARTIALLY_COMPLETED_TEMPLATE_SUBJECT = "Mass keyword upload as partially succeed";
  public static final String RESTRICTED_KEYWORD_FAILED_TEMPLATE_SUBJECT = "Mass keyword upload has been failed";
  //Brand Auth Templates Ids
  public static final String BRAND_AUTH_ADD_COMPLETED_TEMPLATE_ID = "BRAND_AUTH_ADD_COMPLETED_TEMPLATE_ID";
  public static final String BRAND_AUTH_ADD_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BRAND_AUTH_ADD_PARTIALLY_COMPLETED_TEMPLATE_ID";
  public static final String BRAND_AUTH_ADD_FAILED_TEMPLATE_ID = "BRAND_AUTH_ADD_FAILED_TEMPLATE_ID";
  public static final String BRAND_AUTH_DELETE_COMPLETED_TEMPLATE_ID = "BRAND_AUTH_DELETE_COMPLETED_TEMPLATE_ID";
  public static final String BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE_ID";
  public static final String BRAND_AUTH_DELETE_FAILED_TEMPLATE_ID = "BRAND_AUTH_DELETE_FAILED_TEMPLATE_ID";
  //Brand Auth Template Subjects
  public static final String BRAND_AUTH_ADD_COMPLETED_TEMPLATE = "Brand authorization addition has completed";
  public static final String BRAND_AUTH_ADD_PARTIALLY_COMPLETED_TEMPLATE =
      "Brand authorization addition has partially succeed";
  public static final String BRAND_AUTH_ADD_FAILED_TEMPLATE = "Brand authorization addition has failed";
  public static final String BRAND_AUTH_DELETE_COMPLETED_TEMPLATE = "Brand authorization has been deleted";
  public static final String BRAND_AUTH_DELETE_PARTIALLY_COMPLETED_TEMPLATE =
      "Brand authorization has partially deleted";
  public static final String BRAND_AUTH_DELETE_FAILED_TEMPLATE = "Failed to delete brand authorization";

  public static final String BRAND_AUTH_DOWNLOAD_TEMPLATE_ID = "BRAND_AUTH_DOWNLOAD_ALL_TEMPLATE_ID";
  public static final String BRAND_AUTH_DOWNLOAD_TEMPLATE_SUBJECT = "Brand authorization file is ready";
  public static final String DELETE_UPDATE_PICKUP_POINTS_TEMPLATE_ID = "DELETE_UPDATE_PICKUP_POINTS";
  public static final String DELETE_UPDATE_PICKUP_POINTS_TEMPLATE_SUBJECT = "updated pickup points are ready";
  public static final String MASTER_SKU_REVIEW_ITEMS_DOWNLOAD_TEMPLATE_ID = "MASTER_SKU_REVIEW_ITEMS_DOWNLOAD";
  public static final String MASTER_SKU_REVIEW_ITEMS_DOWNLOAD_SUBJECT = "Master Sku review items file is ready";
  // MASTER SKU REVIEW EMAIL TEMPLATES
  public static final String MASTER_SKU_IN_REVIEW_DOWNLOAD_TEMPLATE_ID =
    "MASTER_SKU_IN_REVIEW_DOWNLOAD";
  public static final String MASTER_SKU_IN_REVIEW_DOWNLOAD_TEMPLATE_SUBJECT = "Master sku "
    + "in-review file is ready";

  public static final String AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_ID =
      "AUTO_APPROVED_PRODUCT_DOWNLOAD";
  public static final String AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_SUBJECT =
      "Auto Approved Products file is ready";
  public static final String MASTER_SKU_BULK_ASSIGNEE_SUCCESS_TEMPLATE_ID =
    "MASTER_SKU_BULK_ASSIGNEE_SUCCESS_TEMPLATE_ID";
  public static final String MASTER_SKU_BULK_ASSIGNEE_SUCCESS_TEMPLATE_SUBJECT =
    "Master sku bulk assignee upload is success";
  public static final String MASTER_SKU_BULK_ASSIGNEE_PARTIAL_SUCCESS_TEMPLATE_ID =
    "MASTER_SKU_BULK_ASSIGNEE_PARTIAL_SUCCESS_TEMPLATE_ID";
  public static final String MASTER_SKU_BULK_ASSIGNEE_PARTIAL_SUCCESS_TEMPLATE_SUBJECT =
    "Master sku bulk assignee upload is partially success";
  public static final String MASTER_SKU_BULK_ASSIGNEE_FAILED_TEMPLATE_ID =
    "MASTER_SKU_BULK_ASSIGNEE_FAILED_TEMPLATE_ID";
  public static final String MASTER_SKU_BULK_ASSIGNEE_FAILED_TEMPLATE_SUBJECT =
    "Master sku bulk assignee upload has failed";

  //VENDOR APPROVAL ACTION EMAIL IDs

  public static final String VENDOR_APPROVAL_TEMPLATE_ID = "VENDOR_APPROVAL_TEMPLATE_ID";
  public static final String VENDOR_APPROVAL_FAILED_TEMPLATE_ID =
    "VENDOR_APPROVAL_FAILED_TEMPLATE_ID";
  public static final String VENDOR_APPROVAL_PARTIALLY_COMPLETED_TEMPLATE =
    "VENDOR_APPROVAL_PARTIALLY_COMPLETED_TEMPLATE";

  //VENDOR REJECTION ACTION EMAIL IDs
  public static final String VENDOR_REJECTION_COMPLETED_TEMPLATE_ID =
    "VENDOR_REJECTION_TEMPLATE_ID";
  public static final String VENDOR_REJECTION_FAILED_TEMPLATE_ID =
    "VENDOR_REJECTION_FAILED_TEMPLATE_ID";
  public static final String VENDOR_REJECTION_PARTIALLY_COMPLETED_TEMPLATE_ID =
    "VENDOR_REJECTION_PARTIALLY_COMPLETED_TEMPLATE";


  //Vendor Approval Template Subjects
  public static final String VENDOR_APPROVAL_COMPLETED_TEMPLATE =
    "Product approval has been completed";
  public static final String VENDOR_APPROVAL_FAILED_TEMPLATE = "Product approval has failed";
  public static final String VENDOR_APPROVAL_PARTIALLY_SUCCEED_TEMPLATE =
    "Product approval has partially succeed";

  //Vendor Rejection Template Subjects
  public static final String VENDOR_REJECTION_COMPLETED_TEMPLATE =
    "Product rejection has been completed";
  public static final String VENDOR_REJECTION_FAILED_TEMPLATE = "Product rejection has failed";
  public static final String VENDOR_REJECTION_PARTIALLY_COMPLETED_TEMPLATE =
    "Product rejection has partially succeed";

  //Bulk Master Sku review  template ids

  public static final String BULK_MASTER_SKU_REVIEW_TEMPLATE_ID = "BULK_MASTER_SKU_REVIEW_TEMPLATE_ID";
  public static final String BULK_MASTER_SKU_REVIEW_FAILED_TEMPLATE_ID = "BULK_MASTER_SKU_REVIEW_FAILED_TEMPLATE_ID";
  public static final String BULK_MASTER_SKU_REVIEW_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BULK_MASTER_SKU_REVIEW_PARTIALLY_COMPLETED_TEMPLATE_ID";

  //Bulk master sku review Template Subjects
  public static final String BULK_MASTER_SKU_REVIEW_COMPLETED_TEMPLATE = "Master sku review has been completed";
  public static final String BULK_MASTER_SKU_REVIEW_FAILED_TEMPLATE = "Master sku review has failed";
  public static final String BULK_MASTER_SKU_REVIEW_PARTIALLY_COMPLETED_TEMPLATE =
      "Master sku review has partially succeed";

  //Bulk IPR Products add review  template ids

  public static final String BULK_IPR_PRODUCT_ADD_REVIEW_COMPLETED_TEMPLATE_ID =
      "BULK_IPR_PRODUCT_ADD_REVIEW_COMPLETED_TEMPLATE_ID";
  public static final String BULK_IPR_PRODUCT_ADD_REVIEW_FAILED_TEMPLATE_ID =
      "BULK_IPR_PRODUCT_ADD_REVIEW_FAILED_TEMPLATE_ID";
  public static final String BULK_IPR_PRODUCT_ADD_REVIEW_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BULK_IPR_PRODUCT_ADD_REVIEW_PARTIALLY_COMPLETED_TEMPLATE_ID";

  //Bulk IPR Products Template Subjects
  public static final String BULK_IPR_PRODUCT_ADD_REVIEW_COMPLETED_TEMPLATE =
      "IPR Product add or review has been completed";
  public static final String BULK_IPR_PRODUCT_ADD_REVIEW_FAILED_TEMPLATE =
      "IPR Product add or review has failed";
  public static final String BULK_IPR_PRODUCT_ADD_REVIEW_PARTIALLY_COMPLETED_TEMPLATE =
      "IPR Product add or review has partially succeed";
  public static final String BULK_IPR_PRODUCT_DOWNLOAD_TEMPLATE_SUBJECT =
      "IPR Product file is ready";

  public static final String BULK_IPR_PRODUCT_DOWNLOAD_TEMPLATE_ID =
      "BULK_IPR_PRODUCT_DOWNLOAD_TEMPLATE_ID";

  //Bulk Price update Templates Ids
  public static final String BULK_PRICE_UPDATE_COMPLETED_TEMPLATE_ID = "BULK_PRICE_UPDATE_COMPLETED_TEMPLATE_ID";
  public static final String BULK_PRICE_UPDATE_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BULK_PRICE_UPDATE_PARTIALLY_COMPLETED_TEMPLATE_ID";
  public static final String BULK_PRICE_UPDATE_FAILED_TEMPLATE_ID = "BULK_PRICE_UPDATE_FAILED_TEMPLATE_ID";
  public static final String BULK_PRICE_UPDATE_MAX_ROWS_FAILURE_ID = "BULK_PRICE_UPDATE_MAX_ROWS_FAILURE_ID";

  //Bulk Rebate update Templates Ids
  public static final String BULK_REBATE_UPDATE_COMPLETED_TEMPLATE_ID = "BULK_REBATE_UPDATE_COMPLETED_TEMPLATE_ID";
  public static final String BULK_REBATE_UPDATE_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BULK_REBATE_UPDATE_PARTIALLY_COMPLETED_TEMPLATE_ID";
  public static final String BULK_REBATE_UPDATE_FAILED_TEMPLATE_ID = "BULK_REBATE_UPDATE_FAILED_TEMPLATE_ID";
  public static final String BULK_REBATE_UPDATE_MAX_ROWS_FAILURE_ID = "BULK_REBATE_UPDATE_MAX_ROWS_FAILURE_ID";

  //Bulk Sku Level Rebate Templates Ids
  public static final String BULK_SKU_LEVEL_REBATE_COMPLETED_TEMPLATE_ID =
      "BULK_SKU_LEVEL_REBATE_COMPLETED_TEMPLATE_ID";
  public static final String BULK_SKU_LEVEL_REBATE_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BULK_SKU_LEVEL_REBATE_PARTIALLY_COMPLETED_TEMPLATE_ID";
  public static final String BULK_SKU_LEVEL_REBATE_FAILED_TEMPLATE_ID = "BULK_SKU_LEVEL_REBATE_FAILED_TEMPLATE_ID";
  public static final String BULK_SKU_LEVEL_REBATE_MAX_ROWS_FAILURE_TEMPLATE_ID =
      "BULK_SKU_LEVEL_REBATE_MAX_ROWS_FAILURE_TEMPLATE_ID";

  //Bulk New Price Update Templates Ids
  public static final String BULK_NEW_PRICE_UPDATE_COMPLETED_TEMPLATE_ID =
      "BULK_NEW_PRICE_UPDATE_COMPLETED_TEMPLATE_ID";
  public static final String BULK_NEW_PRICE_UPDATE_FAILED_PARTIALLY_COMPLETED_TEMPLATE_ID =
      "BULK_NEW_PRICE_UPDATE_FAILED_PARTIALLY_COMPLETED_TEMPLATE_ID";
  public static final String BULK_NEW_PRICE_UPDATE_NO_PRICE_CHANGE_TEMPLATE_ID = "BULK_NEW_PRICE_UPDATE_NO_PRICE_CHANGE_TEMPLATE_ID";
  public static final String BULK_NEW_PRICE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE_ID =
      "BULK_NEW_PRICE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE_ID";

  //Bulk price update Template Subjects
  public static final String BULK_PRICE_UPDATE_COMPLETED_TEMPLATE = "Price & Stock mass update has been completed";

  public static final String BULK_PRODUCT_TYPE_TAGGING_COMPLETED_TEMPLATE_ID =
    "BULK_PRODUCT_TYPE_TAGGING_COMPLETED_TEMPLATE_ID";
  public static final String BULK_PRODUCT_TYPE_TAGGING_PARTIALLY_COMPLETED_TEMPLATE_ID =
    "BULK_PRODUCT_TYPE_TAGGING_PARTIALLY_COMPLETED_TEMPLATE_ID";
  public static final String BULK_PRODUCT_TYPE_TAGGING_MAX_ROWS_FAILURE_ID = "BULK_PRODUCT_TYPE_TAGGING_MAX_ROWS_FAILURE_ID";

  public static final String BULK_PRODUCT_TYPE_TAGGING_MAX_ROWS_FAILURE_TEMPLATE =
    "Bulk Product Type tagging update has been failed";
  public static final String BULK_PRODUCT_TYPE_TAGGING_COMPLETED_TEMPLATE =
    "Bulk Product type tagging update process is now complete .";
  public static final String BULK_PRICE_UPDATE_PARTIALLY_COMPLETED_TEMPLATE =
      "Price & Stock mass update has partially succeed";
  public static final String BULK_PRODUCT_TYPE_TAGGING_PARTIALLY_COMPLETED_TEMPLATE =
    "Some Product type tagging update process failed to upload";
  public static final String BULK_PRICE_UPDATE_FAILED_TEMPLATE = "Price & Stock mass update has been failed";
  public static final String BULK_PRICE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE = "Price & Stock mass update has been failed";

  //Bulk REBATE update Template Subjects
  public static final String BULK_REBATE_UPDATE_COMPLETED_TEMPLATE = "Rebate projection in mass has been uploaded";
  public static final String BULK_REBATE_UPDATE_PARTIALLY_COMPLETED_TEMPLATE =
      "Some rebate projection in mass failed to upload";
  public static final String BULK_REBATE_UPDATE_FAILED_TEMPLATE = "Rebate projection in mass failed to upload";
  public static final String BULK_REBATE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE = "Rebate mass update has been failed";

  //Bulk Sku Level Rebate Template Subjects
  public static final String BULK_SKU_LEVEL_REBATE_COMPLETED_TEMPLATE =
      "Success to update rebate on SKU level using excel.";
  public static final String BULK_SKU_LEVEL_REBATE_PARTIALLY_COMPLETED_TEMPLATE =
      "Some rebate on SKU level failed to upload.";
  public static final String BULK_SKU_LEVEL_REBATE_FAILED_TEMPLATE =
      "Failed to update rebate on SKU level using excel.";
  public static final String BULK_SKU_LEVEL_REBATE_MAX_ROWS_FAILURE_TEMPLATE =
      "Failed to upload file for SKU level rebate.";

  //Bulk New Price Update Template Subjects
  public static final String BULK_NEW_PRICE_UPDATE_COMPLETED_TEMPLATE =
      "Pricing update has been successful";
  public static final String BULK_NEW_PRICE_UPDATE_FAILED_PARTIALLY_COMPLETED_TEMPLATE =
      "Some pricing update has been failed";
  public static final String BULK_NEW_PRICE_UPDATE_NO_PRICE_CHANGE_TEMPLATE =
      "No price change is detected";
  public static final String BULK_NEW_PRICE_UPDATE_MAX_ROWS_FAILURE_TEMPLATE =
      "Failed to upload file for pricing update";

  public static final ImmutableMap<String, String> COPY_STORE_MESSAGE_MAP =
      new ImmutableMap.Builder<String, String>()
          .put(ProcessStatus.PENDING.name(), STORE_COPY_CREATION)
          .put(ProcessStatus.COMPLETED.name(), STORE_COPY_SUCCESS)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), STORE_COPY_PARTIAL_SUCCESS)
          .put(ProcessStatus.FAILED.name(), STORE_COPY_FAILED)
          .build();

  public static final ImmutableMap<String, String> COPY_STORE_SUBJECT_MAP =
      new ImmutableMap.Builder<String, String>()
          .put(ProcessStatus.PENDING.name(), STORE_COPY_CREATED_SUBJECT)
          .put(ProcessStatus.COMPLETED.name(), STORE_COPY_SUCCESS_SUBJECT)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), STORE_COPY_PARTIAL_SUCCESS_SUBJECT)
          .put(ProcessStatus.FAILED.name(), STORE_COPY_FAILED_SUBJECT)
          .build();

  public static final ImmutableMap<String, String> UPDATE_SALES_CATEGORY_MESSAGE_MAP =
      new ImmutableMap.Builder<String, String>().put(ProcessStatus.PENDING.name(), UPDATE_SALES_CATEGORY_CREATION)
          .put(ProcessStatus.COMPLETED.name(), UPDATE_SALES_CATEGORY_SUCCESS)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), UPDATE_SALES_CATEGORY_PARTIAL_SUCCESS)
          .put(ProcessStatus.FAILED.name(), UPDATE_SALES_CATEGORY_FAILED).build();

  public static final ImmutableMap<String, String> UPDATE_SALES_CATEGORY_SUBJECT_MAP =
      new ImmutableMap.Builder<String, String>()
          .put(ProcessStatus.PENDING.name(), UPDATE_SALES_CATEGORY_CREATED_SUBJECT)
          .put(ProcessStatus.COMPLETED.name(), UPDATE_SALES_CATEGORY_SUCCESS_SUBJECT)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), UPDATE_SALES_CATEGORY_PARTIAL_SUCCESS_SUBJECT)
          .put(ProcessStatus.FAILED.name(), UPDATE_SALES_CATEGORY_FAILED_SUBJECT).build();

  public static final ImmutableMap<String, String> BULK_MASTER_SKU_REVIEW_TEMPLATE_MAP =
      new ImmutableMap.Builder<String, String>().put(ProcessStatus.COMPLETED.name(), BULK_MASTER_SKU_REVIEW_TEMPLATE_ID)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), BULK_MASTER_SKU_REVIEW_PARTIALLY_COMPLETED_TEMPLATE_ID)
          .put(ProcessStatus.FAILED.name(), BULK_MASTER_SKU_REVIEW_FAILED_TEMPLATE_ID).build();

  public static final ImmutableMap<String, String> BULK_MASTER_SKU_REVIEW_SUBJECT_MAP =
      new ImmutableMap.Builder<String, String>().put(ProcessStatus.COMPLETED.name(),
              BULK_MASTER_SKU_REVIEW_COMPLETED_TEMPLATE)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), BULK_MASTER_SKU_REVIEW_PARTIALLY_COMPLETED_TEMPLATE)
          .put(ProcessStatus.FAILED.name(), BULK_MASTER_SKU_REVIEW_FAILED_TEMPLATE).build();

  public static final ImmutableMap<String, String> BULK_MASTER_SKU_ASSIGNEE_TEMPLATE_MAP =
      new ImmutableMap.Builder<String, String>().put(ProcessStatus.COMPLETED.name(),
              MASTER_SKU_BULK_ASSIGNEE_SUCCESS_TEMPLATE_ID)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), MASTER_SKU_BULK_ASSIGNEE_PARTIAL_SUCCESS_TEMPLATE_ID)
          .put(ProcessStatus.FAILED.name(), MASTER_SKU_BULK_ASSIGNEE_FAILED_TEMPLATE_ID).build();

  public static final ImmutableMap<String, String> BULK_MASTER_SKU_ASSIGNEE_SUBJECT_MAP =
      new ImmutableMap.Builder<String, String>().put(ProcessStatus.COMPLETED.name(),
              MASTER_SKU_BULK_ASSIGNEE_SUCCESS_TEMPLATE_SUBJECT)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), MASTER_SKU_BULK_ASSIGNEE_PARTIAL_SUCCESS_TEMPLATE_SUBJECT)
          .put(ProcessStatus.FAILED.name(), MASTER_SKU_BULK_ASSIGNEE_FAILED_TEMPLATE_SUBJECT).build();


  public static final ImmutableMap<String, String> BULK_IPR_PRODUCT_ADD_REVIEW_TEMPLATE_MAP =
      new ImmutableMap.Builder<String, String>().put(ProcessStatus.COMPLETED.name(), BULK_IPR_PRODUCT_ADD_REVIEW_COMPLETED_TEMPLATE_ID)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), BULK_IPR_PRODUCT_ADD_REVIEW_PARTIALLY_COMPLETED_TEMPLATE_ID)
          .put(ProcessStatus.FAILED.name(), BULK_IPR_PRODUCT_ADD_REVIEW_FAILED_TEMPLATE_ID).build();

  public static final ImmutableMap<String, String> BULK_IPR_PRODUCT_ADD_REVIEW_SUBJECT_MAP =
      new ImmutableMap.Builder<String, String>().put(ProcessStatus.COMPLETED.name(),
              BULK_IPR_PRODUCT_ADD_REVIEW_COMPLETED_TEMPLATE)
          .put(ProcessStatus.PARTIAL_COMPLETED.name(), BULK_IPR_PRODUCT_ADD_REVIEW_PARTIALLY_COMPLETED_TEMPLATE)
          .put(ProcessStatus.FAILED.name(), BULK_IPR_PRODUCT_ADD_REVIEW_FAILED_TEMPLATE).build();
}
