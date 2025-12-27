package com.gdn.x.product.rest.web.model.request;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
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
public class ItemPickupPointUpdateRequest extends BaseRequest {
  private ProductType productType;
  private String productSku;
  private Boolean online;
  private Boolean cnc;
  private Boolean fbbActivated;
  private Boolean b2bActivated;
  private Boolean b2cActivated;
  private boolean dimensionsMissing;
  private AddDeleteVariantRequest addDeleteVariantRequest;
  private List<ItemPickupPointQuickEditRequest> quickEditUpdateRequests =
      new ArrayList<ItemPickupPointQuickEditRequest>();
  private List<ItemPickupPointQuickEditRequest> addPickupPointRequests =
      new ArrayList<ItemPickupPointQuickEditRequest>();
  private List<ItemPickupPointDeleteRequest> deletePickupPointRequests = new ArrayList<ItemPickupPointDeleteRequest>();
  private Set<BundleRecipeRequest> bundleRecipesRequests = new HashSet<>();
}
