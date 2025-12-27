package com.gdn.partners.pbp.dto.promo.request;

import java.util.Date;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PromoBundlingPricingSummaryRequest extends BaseRequest {
  private static final long serialVersionUID = 2325438907389375743L;

  private Set<String> statusSet;
  private String promoKeyword;
  private String promoBundlingId;
  private String promoBundlingType;
  private String promoBundlingName;
  private String merchantCode;
  private Set<String> itemSkus;
  private Set<String> itemPickupPointIds;
  private Date startDate;
  private Date endDate;
  private Boolean includeMFDTrueSkuBundlings;
  private boolean minOrder;
  private String productSku;
}
