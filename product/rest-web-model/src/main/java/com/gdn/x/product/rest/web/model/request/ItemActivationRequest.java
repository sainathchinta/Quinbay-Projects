package com.gdn.x.product.rest.web.model.request;

import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.vo.BundleRecipeVo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemActivationRequest {
  private String merchantCode;
  private String merchantSku;
  private String itemCode;
  private String itemSku;
  private Boolean isLateFulfillment;
  private boolean forceReview = false;
  private boolean freeSample = false;
  private boolean markForDelete;
  private Set<BundleRecipeVo> bundleRecipe;
  private List<ItemPickupPointActivationRequest> itemPickupPoints;
}