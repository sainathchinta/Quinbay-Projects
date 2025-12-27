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
public class PDTRevisedProductVendorApprovedEventModel extends GdnBaseDomainEventModel {
  private String productCode;
  private String merchantName;
  private String merchantCode;
  private boolean forReview;
  private boolean postLive;
  private String updatedBy;
  private boolean reviewPending;
  private String approvalType;
}
