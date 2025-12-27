package com.gdn.partners.pcu.internal.client.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AnchorDetailResponse extends BaseResponse {

  private static final long serialVersionUID = -1892338594542036509L;
  private String mainImage;
  private ProductDetail productDetail;
  private String keyFeatures;
  private String description;
  private double price;
  private String brand;
  private List<AttributeModel> attributeLists = new ArrayList<>();
  private ProductDimensions productDimensions;
  private double shippingWeight;
  private String masterItemSku;
}
