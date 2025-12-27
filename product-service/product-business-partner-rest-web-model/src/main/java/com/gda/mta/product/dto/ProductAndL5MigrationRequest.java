package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.enums.ProductType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndL5MigrationRequest implements Serializable {

  private static final long serialVersionUID = 2577375606806153016L;
  private String productSku;
  private Boolean dimensionsMissing;
  private ProductType productType;
  private boolean includeMfd;
  private boolean buyable;
  private boolean discoverable;
  private Boolean cncActiveAtL3;
  private Boolean cncActiveAtL5;
  private boolean l5Updated;
}