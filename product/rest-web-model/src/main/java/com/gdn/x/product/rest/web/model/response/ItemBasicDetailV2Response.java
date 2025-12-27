package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemBasicDetailV2Response extends BaseResponse {

  private static final long serialVersionUID = -997869296478734544L;

  private String merchantCode;
  private String itemSku;
  private String productSku;
  private String generatedItemName;
  private String itemCode;
  private String mainImageUrl;
  private List<BundleRecipeV2Response> bundleRecipeList = new ArrayList<>();
  private List<CategoryDataResponse> categoryHierarchy;
  private String categoryCode;
  private String merchantSku;
  private boolean markForDelete;
  private boolean permanentDelete;
  private boolean archived;
  private double length;
  private double width;
  private double height;
  private double weight;
  private double shippingWeight;
  private Integer dangerousLevel;
  private ProductDataResponse productData;
  private String categoryName;
  private boolean fbbActivated;
  private String brand;
  private boolean sharedProduct;
  private boolean subscribable;
}
