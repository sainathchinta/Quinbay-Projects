package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by govind on 11/01/2019 AD.
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ProductReturnForCorrectionWebRequest {

  private String productCode;
  private String notes;
}
