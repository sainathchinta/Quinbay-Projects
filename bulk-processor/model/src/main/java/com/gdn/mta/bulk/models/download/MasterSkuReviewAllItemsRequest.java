package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

@Data
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterSkuReviewAllItemsRequest extends MasterSkuReviewDownloadItemsRequest {
  public MasterSkuReviewAllItemsRequest() {
    // Call the constructor of the immediate parent class
    super();
  }
}
