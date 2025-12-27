package com.gdn.mta.bulk.models.download;

import java.io.Serializable;
import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude()
public class MerchantConfigurationDownloadRequest extends BulkDownloadRequest implements Serializable {

  private static final long serialVersionUID = 1717639834233911674L;
  private String storeId;
  private String reviewConfig;
  private String configType;
  private String categoryCode;
  private String searchKey;
  private String sortOrder = "desc";
  private List<String> dataList;


  public static class MerchantConfigurationDownloadRequestBuilder extends BulkDownloadRequest.BulkRequestBuilder {
    private String storeId;
    private String reviewConfig;
    private String configType;
    private String categoryCode;
    private String searchKey;
    private String sortOrder = "desc";
    private List<String> dataList;

    public MerchantConfigurationDownloadRequestBuilder() {
    }

    public MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder storeId(String storeId) {
      this.storeId = storeId;
      return this;
    }

    public MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder reviewConfig(
        String reviewConfig) {
      this.reviewConfig = reviewConfig;
      return this;
    }

    public MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder configType(
        String configType) {
      this.configType = configType;
      return this;
    }

    public MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder categoryCode(
        String categoryCode) {
      this.categoryCode = categoryCode;
      return this;
    }

    public MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder searchKey(
        String searchKey) {
      this.searchKey = searchKey;
      return this;
    }

    public MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder sortOrder(
        String sortOrder) {
      this.sortOrder = sortOrder;
      return this;
    }

    public MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder dataList(
        List<String> dataList) {
      this.dataList = dataList;
      return this;
    }

    public MerchantConfigurationDownloadRequest build() {
      return new MerchantConfigurationDownloadRequest(this);
    }
  }

  public MerchantConfigurationDownloadRequest(
      MerchantConfigurationDownloadRequest.MerchantConfigurationDownloadRequestBuilder builder) {
    super(builder);
    storeId = builder.storeId;
    reviewConfig = builder.reviewConfig;
    configType = builder.configType;
    categoryCode = builder.categoryCode;
    searchKey = builder.searchKey;
    sortOrder = builder.sortOrder;
    dataList = builder.dataList;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("storeId", storeId)
        .append("reviewConfig", reviewConfig)
        .append("configType", configType)
        .append("categoryCode", categoryCode)
        .append("searchKey", searchKey)
        .append("sortOrder", sortOrder)
        .append("dataList", dataList)
        .append("requestId", getRequestId())
        .append("downloadType", getDownloadType())
        .append("fileType", getFileType())
        .append("bulkProcessEntity", getBulkProcessEntity())
        .append("emailCc", getEmailCc())
        .append("emailTo", getEmailTo())
        .append("filename", getFilename())
        .append("username", getUsername())
        .append("merchantId", getMerchantId())
        .append("language", getLanguage())
        .append("exceptionMsg", getExceptionMsg())
        .append("directDownload", isDirectDownload())
        .toString();
  }
}
