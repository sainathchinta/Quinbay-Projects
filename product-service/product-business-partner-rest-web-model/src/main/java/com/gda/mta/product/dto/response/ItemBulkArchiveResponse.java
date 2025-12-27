package com.gda.mta.product.dto.response;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemBulkArchiveResponse extends BaseResponse {

  private static final long serialVersionUID = -6601409425413147486L;

  private List<String> failedItemSkus;
  private String errorCode = null;
  private Map<String, String> productSkuErrorMessageMap = new HashMap<>();

  public ItemBulkArchiveResponse(List<String> failedItemSkus) {
    this.failedItemSkus = failedItemSkus;
  }

  public ItemBulkArchiveResponse(List<String> failedItemSkus, String errorCode) {
    this.failedItemSkus = failedItemSkus;
    this.errorCode = errorCode;
  }
}
