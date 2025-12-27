package com.gdn.partners.pcu.internal.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCenterListingActionWebRequest {
  private String action;
  private String newCatalogCode;
  private String newCategoryCode;
  private List<CurrentCategoryCatalogWebRequest> currentCategoryCodeSelected;
  private List<String> productSKus;
}
