package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BrandApprovedOrRejectedDomainEventModel extends GdnBaseDomainEventModel {

  private String brandCode;
  private String brandRequestCode;
  private String brandName;
  private String brandApprovalStatus;
  private String businessPartnerCode;
  private boolean protectedBrand;
}
