package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemInstantPickupBulkDownloadResponse extends BaseResponse {

  private static final long serialVersionUID = 5366809697735951449L;

  private String itemSku;
  private String itemName;
  private String pickupPointCode;
  private String pickupPointName;
  private Double listPrice;
  private Double price;
  private Integer offlineAvailableStock;

}
