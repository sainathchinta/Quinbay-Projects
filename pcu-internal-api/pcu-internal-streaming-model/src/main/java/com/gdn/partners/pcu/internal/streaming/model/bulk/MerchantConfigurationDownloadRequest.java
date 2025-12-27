package com.gdn.partners.pcu.internal.streaming.model.bulk;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class MerchantConfigurationDownloadRequest extends BulkDownloadRequest {

  private String storeId;
  private String reviewConfig;
  private String configType;
  private String categoryCode;
  private String searchKey;
  private String sortOrder = "desc";
  private List<String> dataList;

  @Builder(builderMethodName = "MerchantConfigurationDownloadRequestBuilder")
  public MerchantConfigurationDownloadRequest(String requestId, DownloadType downloadType, FileType fileType,
      BulkProcessEntity bulkProcessEntity, String emailCc, String emailTo, String filename, String username,
      String merchantId, String language, boolean directDownload, long timestamp, String storeId, String reviewConfig,
      String configType, String categoryCode, String searchKey, String sortOrder, List<String> dataList) {
    super(requestId, downloadType, fileType, bulkProcessEntity, emailCc, emailTo, filename, username, merchantId,
        language, directDownload, timestamp);
    this.storeId = storeId;
    this.reviewConfig = reviewConfig;
    this.configType = configType;
    this.categoryCode = categoryCode;
    this.searchKey = searchKey;
    this.sortOrder = sortOrder;
    this.dataList = dataList;
  }
}
