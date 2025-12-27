package com.gda.mta.product.dto;

import java.io.Serial;
import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CogsUpdateDtoRequest extends BaseRequest implements Serializable {

  @Serial
  private static final long serialVersionUID = 237367098375655222L;

  private String itemSku;
  private String pickupPointCode;
  private double insuredAmount;
}
