package com.gdn.mta.product.entity;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3Logistics extends BaseResponse implements Serializable {

  private static final long serialVersionUID = -2606589124347250189L;
  private String logisticProductCode;
  private String logisticProductName;
  private boolean selected;
  private boolean requiredLongLat;
  private String highlightedInformation;
}
