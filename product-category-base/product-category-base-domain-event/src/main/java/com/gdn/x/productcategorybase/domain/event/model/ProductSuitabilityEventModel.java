package com.gdn.x.productcategorybase.domain.event.model;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ProductSuitabilityEventModel extends GdnBaseDomainEventModel {

  private String storeId;
  private String productId;
  private List<ProductSuitabilityAttributeModel> attributeValueExtractions = new ArrayList<>();
}
