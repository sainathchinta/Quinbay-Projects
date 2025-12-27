package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AddVariantRequest  implements Serializable {
  private static final long serialVersionUID = -8589684850168307566L;
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
  private List<ProductAttributeDetailDTO> definingAttributes = new ArrayList<>();
  private List<ItemPickupPointQuickEditRequest> itemPickupPoints = new ArrayList<>();
}