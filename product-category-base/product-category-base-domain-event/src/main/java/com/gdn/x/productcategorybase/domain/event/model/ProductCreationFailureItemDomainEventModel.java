package com.gdn.x.productcategorybase.domain.event.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@Builder
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCreationFailureItemDomainEventModel {

  private String generatedItemName;
  private String upcCode;
  private String skuCode;
  private boolean activated;
  private boolean viewable;
  private List<ImageDomainEventModel> images;
  private List<ProductItemAttributeValueDomainEventModel> productItemAttributeValues;
  private Integer dangerousGoodsLevel;
  private boolean internalUpdate;
  private boolean contentChanged;
  private String sourceItemCode;
}
