package com.gdn.partners.pcu.internal.streaming.model.bulk;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 28/01/2019 AD.
 */

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "bulkProcessEntity")
@JsonSubTypes({
    @JsonSubTypes.Type(value = ReviewProductDownloadRequest.class, name = "REVIEW_PRODUCTS"),
    @JsonSubTypes.Type(value = CategoryConfigurationDownloadRequest.class, name = "CONFIGURATION_CATEGORY_SUMMARY"),
    @JsonSubTypes.Type(value = MerchantConfigurationDownloadRequest.class, name = "CONFIGURATION_MERCHANT_SUMMARY"),
    @JsonSubTypes.Type(value = SelectedMasterProductDownloadRequest.class, name = "SELECTED_MASTER_PRODUCTS"),
    @JsonSubTypes.Type(value = MasterProductDownloadRequest.class, name = "MASTER_PRODUCT"),
    @JsonSubTypes.Type(value = MasterSkuInReviewDownloadRequest.class, name = "MASTER_SKU_IN_REVIEW_DOWNLOAD"),
    @JsonSubTypes.Type(value = VendorSummaryDownloadRequest.class, name = "VENDOR_FILTERED_PRODUCT"),
    @JsonSubTypes.Type(value = StoreCopyDownloadRequest.class, name = "STORE_COPY_PRODUCTS"),
    @JsonSubTypes.Type(value = BulkBrandAuthDownloadRequest.class, name = "BRAND_AUTHORIZATION_DOWNLOAD"),
    @JsonSubTypes.Type(value = MasterSkuReviewAllItemsDownloadRequest.class, name = "MASTER_SKU_ALL_ITEMS_DOWNLOAD"),
    @JsonSubTypes.Type(value = MasterSkuReviewSelectedItemsDownloadRequest.class, name = "MASTER_SKU_SELECTED_ITEMS_DOWNLOAD"),
    @JsonSubTypes.Type(value = AutoApprovedAllProductsDownloadRequest.class, name = "AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD"),
    @JsonSubTypes.Type(value = AutoApprovedSelectedProductsDownloadRequest.class, name = "AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD"),
    @JsonSubTypes.Type(value = IPRProductsDownloadRequest.class, name = "IPR_PRODUCTS_DOWNLOAD_ALL"),
    @JsonSubTypes.Type(value = IPRProductsDownloadRequest.class, name = "IPR_PRODUCTS_DOWNLOAD_SELECTED")
})
public class BulkDownloadRequest {

  private String requestId;
  private DownloadType downloadType;
  private FileType fileType;
  private BulkProcessEntity bulkProcessEntity;
  private String emailCc;
  private String emailTo;
  private String filename;
  private String username;
  private String merchantId;
  private String language;
  private boolean directDownload;
  private long timestamp;

}
