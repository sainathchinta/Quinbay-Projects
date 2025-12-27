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
public class ProductAndL5MigrationRequest implements Serializable {

  private static final long serialVersionUID = 780600607105133764L;
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
