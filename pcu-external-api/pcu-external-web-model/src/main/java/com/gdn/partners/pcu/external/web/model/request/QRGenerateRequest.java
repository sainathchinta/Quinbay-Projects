package com.gdn.partners.pcu.external.web.model.request;

import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class QRGenerateRequest {

  private Boolean isDarkTheme;
  private String productSkuCode;
  private String productSkuName;
  private String merchantCode;
  private String merchantName;
  private Boolean isCnC;
  private TemplateSize templateSize;
}
