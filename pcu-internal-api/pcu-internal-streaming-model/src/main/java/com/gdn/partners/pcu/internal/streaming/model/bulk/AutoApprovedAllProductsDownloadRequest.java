package com.gdn.partners.pcu.internal.streaming.model.bulk;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoApprovedAllProductsDownloadRequest extends AutoApprovedProductsDownloadRequest{

  public AutoApprovedAllProductsDownloadRequest() {
    super();
  }
}
