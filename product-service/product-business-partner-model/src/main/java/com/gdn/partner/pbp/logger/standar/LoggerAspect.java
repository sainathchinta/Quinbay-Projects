package com.gdn.partner.pbp.logger.standar;

public enum LoggerAspect {
  
  DEFAULT(""),
  PRODUCT_FETCH("product-fetch"),
  
  PRODUCT_CREATE("product-create"),
  PRODUCT_UPDATE("product-update"),
  MASTER_DATA_BULK_UPDATE("master-data-bulk-update"),
  PRODUCT_UPDATE_PUBLISH_PDT("product-update-publish-pdt"),
  PRODUCT_PUBLISH_PDT("product-publish-pdt"),
  PRODUCT_MERGE("product-merge"),

  RECATEGORIZATION_CREATE("recategorization-create"),

  PRODUCT_VALIDATE_BARCODE("product-validate-barcode"),
  
  PRODUCT_PROCESS_IMAGE("product-process-image"),
  PRODUCT_REJECT_PROCESS_IMAGE("product-reject-process-image"),
  
  PRODUCT_IMAGE_UPDATE("product-image-update"),
  PRODUCT_IMAGE_ACTIVATE("product-image-activate"),
  
  PRODUCT_RETURN_FOR_CORRECTION("product-return-for-correction"),
  
  PRODUCT_APPROVE_CONTENT("product-approve-content"),
  PRODUCT_APPROVE_CONTENT_ACTIVATE("product-approve-image-activate"),
  PRODUCT_APPROVE_IMAGE("product-approve-image"),
  PRODUCT_APPROVE_DRAFT("product-approve-draft"),
  PRODUCT_APPROVE_QC("product-approve-qc"),
  PRODUCT_RESUBMIT("product-resubmit"),
  
  PRODUCT_NOTIFY_EMAIL("product-notify-email"),
  
  PRODUCT_SUBMIT_HISTORY("product-submit-history"),
  
  PRODUCT_GENERATE_BARCODE("product-generate-narcode"),
  PRODUCT_GENERATE_SHIPPING_WEIGHT("product-generate-shipping-weight"),
  
  PRODUCT_DELETE("product-delete"),
  PRODUCT_CODE_SEQUENCE("product-code-sequence"),
  
  PRODUCT_ACTIVE_INTERNAL("product-active-internal"),
  
  PRODUCT_BP_FETCH("product-bp-fetch"),
  PRODUCT_BP_DELETE("product-bp-delete"),
  PRODUCT_BP_CREATE("product-bp-create"),
  PRODUCT_BP_COPY("product-bp-copy"),
  PRODUCT_BP_COPY_NOTIFICATION("product-bp-copy-notification"),
  PRODUCT_BP_COPY_UPDATE_RETRY("product-bp-copy-update-retry"),
  PRODUCT_MAPPED_TO_MERCHANT_CODE("product-mapped-to-merchant-code"),
  PRODUCT_BP_ACTIVATE_FALSE("product-bp-activate-false"),
  PRODUCT_BP_UPDATE("product-bp-update"),
  PRODUCT_BP_RETRY_CREATE("product-bp-retry-create"),
  
  PRODUCT_LV3_FETCH("product-lv3-fetch"),
  PRODUCT_LV3_UPDATE("product-lv3-update"),
  PRODUCT_LV3_BULK_DOWNLOAD("product-lv3-bulk-download"),
  PRODUCT_LV3_UPDATE_SYNC("product-lv3-update-sync"),
  PRODUCT_LV3_ARCHIVE("product-lv3-archive"),
  PRODUCT_LV3_SEND_EMAIL("product-lv3-send-email"),
  PRODUCT_LV3_COUNT_BRAND("product-lv3-count-brand"),
  PRODUCT_LV3_ESTIMATE_PRICE("product-lv3-estimate-price"),
  PRODUCT_LV3_PRISTINE_CATEGORY("product-lv3-pristine-category"),
  
  PROMO_CREATE("promo_create"),
  PROMO_DEACTIVATE("promo_deactivate"),
  PROMO_COUNT("promo_count"),
  PROMO_DETAIL("promo_detail"),
  PROMO_HISTORY("promo_history"),
  PROMO_SUMMARY("promo_summary"),
  PROMO_UPDATE("promo_update"),
  
  AUDIT_SAVE("audit-save"),
  AUDIT_FETCH("audit-fetch"),
  AUDIT_CHECK_PRICE_CHANGE("audit-check-price-change"),
  AUDIT_DELETE("audit-delete"),

  OFFLINE_ITEM_FETCH("offline-item-fetch"),
  OFFLINE_ITEM_FETCH_BULK_DOWNLOAD("offline-item-fetch-bulk-download"),
  OFFLINE_ITEM_DETAIL("offline-item-detail"),
  OFFLINE_ITEM_DELETE("offline-item-delete"),

  BUSINESS_PARTNER_CONFIG("business-partner-config"),

  BULK_ACTIONS("bulk-actions"),
  REVISION_HISTORY("revision-history"),
  PRODUCT_SUSPENSION("product-suspension"),
  GET_SUSPENDED_ITEMS("get-suspended-items"),
  GET_ALL_PRODUCTS("get-all-products"),
  BULK_PRODUCT_SUSPENSION("bulk-product-suspension"),
  SUSPENSION_HISTORY("suspension-history"),
  PRODUCT_SKU_FILTER("product-sku-filter-summary");

  private String value;

  LoggerAspect(String value) {
    this.value = value;
  }

  public String getValue() {
    return value;
  }
  
}
