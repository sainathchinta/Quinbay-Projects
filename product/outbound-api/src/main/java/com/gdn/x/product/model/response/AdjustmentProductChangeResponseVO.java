package com.gdn.x.product.model.response;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AdjustmentProductChangeResponseVO extends BaseResponse implements Serializable {
  private static final long serialVersionUID = -5827079810251408984L;
  private String adjustmentName;
  private String description;
  private String productSku;
  private String campaignCode;
  private String promoType;
  private Date startDate;
  private Date endDate;
  private long value;
  private boolean activated;
  private boolean exclusiveProduct;
  private Integer sessionId;
  private int priority;
  private String pickupPointCode;
  private Set<String> budgetOwners;
}

