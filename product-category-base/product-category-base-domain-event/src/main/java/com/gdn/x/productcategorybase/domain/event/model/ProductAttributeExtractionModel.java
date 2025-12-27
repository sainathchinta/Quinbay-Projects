package com.gdn.x.productcategorybase.domain.event.model;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAttributeExtractionModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String username;
  private String productCode;
  private String cnCategoryCode;
  private String cnCategoryName;
  private String extractionType;
}
