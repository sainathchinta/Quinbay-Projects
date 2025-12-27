package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

@Data
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoApprovedProductsSelectedItemsRequest extends AutoApprovedProductsDownloadRequest{

  public AutoApprovedProductsSelectedItemsRequest() {
    // Call the constructor of the immediate parent class
    super();
  }
}
