package com.gdn.partners.pcu.external.streaming.model.bulk;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, include = JsonTypeInfo.As.PROPERTY, property = "bulkProcessEntity")
@JsonSubTypes({
    @JsonSubTypes.Type(value = ProductDownloadRequest.class, name = "PRODUCT"),
    @JsonSubTypes.Type(value = ProductDownloadEANRequest.class, name = "PRODUCT_EAN"),
    @JsonSubTypes.Type(value = ProductBasicInfoDownloadRequest.class, name = "PRODUCT_BASIC_INFO")
})
public class BulkDownloadRequest extends GdnBaseDomainEventModel implements Serializable {

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
    directDownload = bulkRequestBuilder.directDownload;
  }


  public static class BulkRequestBuilder {
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

    public BulkRequestBuilder fileType(FileType fileType) {
      this.fileType = fileType;
      return this;
    }

    public BulkRequestBuilder bulkProcessType(BulkProcessEntity bulkProcessEntity) {
      this.bulkProcessEntity = bulkProcessEntity;
      return this;
    }

    public BulkRequestBuilder language(String language) {
      this.language = language;
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

    public BulkDownloadRequest build() {
      return new BulkDownloadRequest(this);
    }
  }

}