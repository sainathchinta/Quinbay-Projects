package com.gdn.mta.bulk.dto.product;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.mta.bulk.dto.ProductDetailsRequest;
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
public class DownloadQRCodeRequest extends BaseRequest {
  private String requestId;
  private String storeId;
  private Boolean isDarkTheme;
  private Integer qrPerPage;
  private String templateSize;
  private String qrGenerationType;
  private String merchantCode;
  private String merchantName;
  private boolean allStores;
  private boolean printPrice;
  private List<ProductDetailsRequest> productDetailsRequestList = new ArrayList<>();
  private boolean cncActivated;
}
