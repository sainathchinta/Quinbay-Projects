package com.gdn.x.product.rest.web.model.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.ProductType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CogsUpdateRequest implements Serializable {

  private static final long serialVersionUID = -7403308828429407021L;
  private String itemSku;
  private String pickupPointCode;
  private double insuredAmount;
}
