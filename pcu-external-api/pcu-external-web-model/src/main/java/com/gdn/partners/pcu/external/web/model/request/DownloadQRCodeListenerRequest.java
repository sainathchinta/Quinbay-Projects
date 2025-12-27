package com.gdn.partners.pcu.external.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
public class DownloadQRCodeListenerRequest {
  private String requestId;
  private String storeId;
  private Boolean isDarkTheme;
  private boolean printPrice;
  private Integer qrPerPage;
  private String templateSize;
  private String qrGenerationType;
  private String merchantCode;
  private boolean allStores;
  private String merchantName;
  private List<ProductDetailsRequest> productDetailsRequestList= new ArrayList<>();
  private boolean cncActivated;
}
