package com.gdn.mta.bulk.models.download;

import java.io.Serializable;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.bulk.dto.product.TaggedProductFilterDTO;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.FileType;
import lombok.Data;

/**
 * Created by keshashah on 20/10/16.
 */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY,
    property = "bulkProcessEntity", visible = true)
@JsonSubTypes({@JsonSubTypes.Type(value = OrderDownloadRequest.class, name = "ORDER"),
    @JsonSubTypes.Type(value = ProductUPCDownloadRequest.class,name="PRODUCT_EAN"),
    @JsonSubTypes.Type(value = ProductDownloadRequest.class, name = "PRODUCT"),
    @JsonSubTypes.Type(value = ProductVendorDownloadRequest.class, name = "PRODUCT_VENDOR"),
    @JsonSubTypes.Type(value = MasterProductDownloadRequest.class, name = "MASTER_PRODUCT"),
    @JsonSubTypes.Type(value = MasterSelectedProductDownloadRequest.class, name = "SELECTED_MASTER_PRODUCTS"),
    @JsonSubTypes.Type(value = MasterSkuInReviewDownloadRequest.class, name = "MASTER_SKU_IN_REVIEW_DOWNLOAD"),
    @JsonSubTypes.Type(value = CampaignProductDownloadRequest.class, name = "CAMPAIGN_PRODUCT"),
    @JsonSubTypes.Type(value = ReviewProductDownloadRequest.class, name = "REVIEW_PRODUCTS"),
    @JsonSubTypes.Type(value = VendorSummaryDownloadRequest.class, name = "VENDOR_FILTERED_PRODUCT"),
    @JsonSubTypes.Type(value = MerchantConfigurationDownloadRequest.class, name = "CONFIGURATION_MERCHANT_SUMMARY"),
    @JsonSubTypes.Type(value = CategoryConfigurationDownloadRequest.class, name = "CONFIGURATION_CATEGORY_SUMMARY"),
    @JsonSubTypes.Type(value = InstantPickupProductDownloadRequest.class, name = "INSTANT_PICKUP_PRODUCT"),
    @JsonSubTypes.Type(value = RecatFailedProductsDownloadRequest.class, name = "RECAT_FAILED_PRODUCTS"),
    @JsonSubTypes.Type(value = StoreCopyDownloadRequest.class, name = "STORE_COPY_PRODUCTS"),
    @JsonSubTypes.Type(value = BrandAuthDownloadRequest.class, name = "BRAND_AUTHORIZATION_DOWNLOAD"),
    @JsonSubTypes.Type(value = MasterSkuReviewSelectedItemsRequest.class, name = "MASTER_SKU_SELECTED_ITEMS_DOWNLOAD"),
    @JsonSubTypes.Type(value = MasterSkuReviewAllItemsRequest.class, name = "MASTER_SKU_ALL_ITEMS_DOWNLOAD"),
    @JsonSubTypes.Type(value = AutoApprovedProductsAllItemsRequest.class, name = "AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD"),
    @JsonSubTypes.Type(value = AutoApprovedProductsSelectedItemsRequest.class, name = "AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD"),
    @JsonSubTypes.Type(value = BulkPriceRecommendationDownloadRequest.class, name = "BULK_PRICE_RECOMMENDATION"),
    @JsonSubTypes.Type(value = AutoApprovedProductsSelectedItemsRequest.class, name = "AUTO_APPROVED_PRODUCTS_SELECTED_DOWNLOAD"),
    @JsonSubTypes.Type(value = TaggedProductFilterDTO.class, name ="BULK_DOWNLOAD_TAGGED_PRODUCTS"),
    @JsonSubTypes.Type(value = IPRProductsDownloadRequest.class, name = "IPR_PRODUCTS_DOWNLOAD_ALL"),
    @JsonSubTypes.Type(value = ProductBasicInfoDownloadRequest.class, name = "PRODUCT_BASIC_INFO")
})
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadRequest extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -5571789085339424670L;

  private String requestId;
  private DownloadType downloadType;
  private FileType fileType;
  private BulkProcessEntity bulkProcessEntity;
  private String emailCc;
  private String emailTo;
  private boolean directDownload;
  private String filename;
  private String username;
  private String merchantId;
  private String language;
  private String exceptionMsg;


  public BulkDownloadRequest() {
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public DownloadType getDownloadType() {
    return downloadType;
  }

  public FileType getFileType() {
    return fileType;
  }

  public BulkProcessEntity getBulkProcessEntity() {
    return bulkProcessEntity;
  }

  public String getEmailCc() {
    return emailCc;
  }

  public String getEmailTo() {
    return emailTo;
  }

  public String getFilename() {
    return filename;
  }

  public String getUsername() {
    return username;
  }

  public String getMerchantId() {
    return merchantId;
  }

  public String getLanguage() {
    return language;
  }

  public String getExceptionMsg() {
    return exceptionMsg;
  }

  public void setExceptionMsg(String exceptionMsg) {
    this.exceptionMsg = exceptionMsg;
  }

  public boolean isDirectDownload() {
    return directDownload;
  }

  public void setDirectDownload(boolean directDownload) {
    this.directDownload = directDownload;
  }

  public static class BulkRequestBuilder {
    private String requestId;
    private DownloadType downloadType;
    private FileType fileType;
    private BulkProcessEntity bulkProcessEntity;
    private boolean directDownload;
    private String emailCc;
    private String emailTo;
    private String filename;
    private String username;
    private String merchantId;
    private String language;
    private String exceptionMsg;

    public BulkRequestBuilder request(String requestId) {
      this.requestId = requestId;
      return this;
    }

    public BulkRequestBuilder downloadType(DownloadType downloadType) {
      this.downloadType = downloadType;
      return this;
    }

    public BulkRequestBuilder directDownload(boolean directDownload) {
        this.directDownload = directDownload;
        return this;
    }

    public BulkRequestBuilder language(String language) {
      this.language = language;
      return this;
    }

    public BulkRequestBuilder fileType(FileType fileType) {
      this.fileType = fileType;
      return this;
    }

    public BulkRequestBuilder bulkProcessType(BulkProcessEntity bulkProcessEntity) {
      this.bulkProcessEntity = bulkProcessEntity;
      return this;
    }

    public BulkRequestBuilder emailCC(String emailCc) {
      this.emailCc = emailCc;
      return this;
    }

    public BulkRequestBuilder emailTo(String emailTo) {
      this.emailTo = emailTo;
      return this;
    }

    public BulkRequestBuilder filename(String filename) {
      this.filename = filename;
      return this;
    }

    public BulkRequestBuilder username(String username) {
      this.username = username;
      return this;
    }

    public BulkRequestBuilder merchant(String merchantId) {
      this.merchantId = merchantId;
      return this;
    }

    public BulkRequestBuilder exceptionMsg(String exceptionMsg) {
      this.exceptionMsg = exceptionMsg;
      return this;
    }

    public BulkDownloadRequest build() {
      return new BulkDownloadRequest(this);
    }
  }

  protected BulkDownloadRequest(BulkRequestBuilder bulkRequestBuilder) {
    requestId = bulkRequestBuilder.requestId;
    downloadType = bulkRequestBuilder.downloadType;
    fileType = bulkRequestBuilder.fileType;
    bulkProcessEntity = bulkRequestBuilder.bulkProcessEntity;
    emailCc = bulkRequestBuilder.emailCc;
    emailTo = bulkRequestBuilder.emailTo;
    filename = bulkRequestBuilder.filename;
    username = bulkRequestBuilder.username;
    merchantId = bulkRequestBuilder.merchantId;
    language = bulkRequestBuilder.language;
    exceptionMsg = bulkRequestBuilder.exceptionMsg;
    directDownload = bulkRequestBuilder.directDownload;
  }

  @Override public String toString() {
    return new ToStringBuilder(this).append("requestId", requestId)
        .append("downloadType", downloadType).append("fileType", fileType)
        .append("bulkProcessEntity", bulkProcessEntity).append("emailCc", emailCc)
        .append("emailTo", emailTo).append("filename", filename).append("username", username)
        .append("merchantId", merchantId).append("language", language).append("directDownload", directDownload)
        .append("exceptionMsg", exceptionMsg).toString();
  }

}
