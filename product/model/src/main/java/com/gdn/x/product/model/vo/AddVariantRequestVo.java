package com.gdn.x.product.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AddVariantRequestVo {
  private String itemSku;
  private String itemCode;
  private String merchantSku;
  private String generatedItemName;
  private String mainImageUrl;
  private double length;
  private double width;
  private double height;
  private double weight;
  private double shippingWeight;
  private Integer dangerousLevel;
  private boolean forceReview;
  private List<ProductAttributeDetailVo> definingAttributes = new ArrayList<>();
  private List<ItemPickupPointListingUpdateRequestVo> itemPickupPoints = new ArrayList<>();
}