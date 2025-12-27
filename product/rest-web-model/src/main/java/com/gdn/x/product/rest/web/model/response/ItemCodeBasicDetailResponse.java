package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemCodeBasicDetailResponse extends BaseResponse {
  private static final long serialVersionUID = -4386414916596980975L;
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
}
