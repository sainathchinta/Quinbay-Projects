package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.AllowedQRGenerationType;
import com.gdn.partners.pcu.external.web.model.enums.TemplateSize;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ManualQRCodeRequest {
  private Boolean isDarkTheme;
  private boolean printPrice;
  private Integer qrPerPage;
  private TemplateSize templateSize;
  private AllowedQRGenerationType qrGenerationType;
  private boolean allStores;
  private List<ProductDetailsRequest> productDetailsRequestList = new ArrayList<>();
  private Boolean cncActivated;
}
