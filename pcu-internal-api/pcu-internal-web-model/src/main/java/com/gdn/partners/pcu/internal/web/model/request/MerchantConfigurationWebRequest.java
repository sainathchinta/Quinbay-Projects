package com.gdn.partners.pcu.internal.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by parvej on 27/01/2020 AD.
 */

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class MerchantConfigurationWebRequest {

  String merchantCode;
  String merchantName;
  String reviewConfig;
}