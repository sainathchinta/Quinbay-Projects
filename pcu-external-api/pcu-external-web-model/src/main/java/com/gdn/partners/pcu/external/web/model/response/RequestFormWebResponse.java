package com.gdn.partners.pcu.external.web.model.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class RequestFormWebResponse extends RequestFormResponse {
  private String categoryName;
  private boolean bundleProduct;
  private boolean transferFromBundleProduct;
}
