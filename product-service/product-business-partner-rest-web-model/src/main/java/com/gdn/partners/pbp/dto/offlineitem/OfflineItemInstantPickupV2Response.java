package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemInstantPickupV2Response extends BaseResponse {

  private static final long serialVersionUID = 5833766070622173444L;
  private String categoryName;
  private String itemName;
  private String itemSku;
  private String merchantSku;
  private String productSku;
  private List<OfflineProductResponse> offlineProducts;
}
