package com.gdn.mta.domain.event.modal;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class PriceInfoDTO implements Serializable {
  private static final long serialVersionUID = 2287790234841716237L;
  private String itemId;
  private String itemSku;
  private double minPrice;
  private double maxPrice;
}