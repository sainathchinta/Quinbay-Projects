package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class FbbCreatePickupPointResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 1581935239382150842L;
  private String pickupPointId;
  private String itemSku;
  private String reason;
  private String errorCode;
}
