package com.gdn.x.product.model.vo;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.enums.ProductType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointUpdateRequestVo {
  private ProductType productType;
  private String productSku;
  private Boolean online;
  private Boolean cnc;
  private Boolean fbbActivated;
  private Boolean b2bActivated;
  private Boolean b2cActivated;
  private AddDeleteVariantRequestVo addDeleteVariantRequestVo;
  private List<ItemPickupPointListingUpdateRequestVo> quickEditUpdateRequests = new ArrayList<>();
  private List<ItemPickupPointListingUpdateRequestVo> addPickupPointRequests = new ArrayList<>();
  private List<ItemPickupPointDeleteRequestVo> deletePickupPointRequests = new ArrayList<>();
  private Set<BundleRecipeRequest> bundleRecipeRequests = new HashSet<>();
}
