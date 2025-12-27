package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ItemSkuPickupPointRequest implements Serializable {

  private static final long serialVersionUID = -4430143987731580449L;
  private String productSku;
  private String pickupPointCode;
  private String businessPartnerCode;
  private List<String> itemSkuList;
}
