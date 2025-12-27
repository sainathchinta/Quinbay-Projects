package com.gdn.partners.pcu.internal.streaming.model.bulk;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterSkuReviewSelectedItemsDownloadRequest extends MasterSkuReviewDownloadItemsRequest {

  public MasterSkuReviewSelectedItemsDownloadRequest() {
    // Call the constructor of the immediate parent class
    super();
  }
}
