package com.gdn.x.mta.distributiontask.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAutoApprovalEventModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String productCode;
  private String categoryCode;
  private Boolean edited;
  private String reviewType;
  private boolean revised;
  private String destinationCategoryCode;
  private boolean onlyCategoryUpdate = false;

  public ProductAutoApprovalEventModel(String storeId, String productCode, String categoryCode, Boolean edited,
      String reviewType, boolean revised, String destinationCategoryCode) {
    this.storeId = storeId;
    this.productCode = productCode;
    this.categoryCode = categoryCode;
    this.edited = edited;
    this.reviewType = reviewType;
    this.revised = revised;
    this.destinationCategoryCode = destinationCategoryCode;
  }

  public ProductAutoApprovalEventModel(String storeId, String productCode, String categoryCode, Boolean edited,
      String reviewType, boolean revised) {
    this.storeId = storeId;
    this.productCode = productCode;
    this.categoryCode = categoryCode;
    this.edited = edited;
    this.reviewType = reviewType;
    this.revised = revised;
  }
}
