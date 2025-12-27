package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude()
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemCodeBasicDetailWebResponse extends BaseResponse {
  private String itemCode;
  private String itemName;
  private boolean tdSeller;
  private String itemSku;
  private boolean suspended;
  private boolean archivedAtL3;
  private boolean archivedAtL4;
  private String productSku;
  private String merchantCode;
  private String mainImageUrl;
  private String pdpUrl;
}
