package com.gdn.x.productcategorybase.domain.event.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductSalesCategoryMapping {

  private List<String> oldSalesCategoryCodes;
  private List<String> newSalesCategoryCodes;
  private List<String> newUmkmSalesCategoryCodes;
  private List<String> oldB2bSalesCategoryCodes;
  private List<String> newB2bSalesCategoryCodes;
  private boolean newCategoryBopisEligible = true;

}
