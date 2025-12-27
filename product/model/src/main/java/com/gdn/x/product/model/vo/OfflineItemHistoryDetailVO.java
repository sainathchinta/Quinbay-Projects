package com.gdn.x.product.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemHistoryDetailVO implements Serializable {

  private static final long serialVersionUID = -7873003654609342724L;

  private Double oldListPrice;
  private Double oldOfferPrice;
  private Boolean syncPriceAction;
  private String clientId;
  private String requestId;

}
