package com.gdn.mta.bulk.models;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DeletePickupPointResponseEventModel extends GdnBaseDomainEventModel {
  private String businessPartnerCode;
  private String pickupPointCode;
  private String status;
  private List<FailedItemAndReasonResponse> failedItemAndReasonResponseList;
  private String ppCodeUpdateFilePath;
}
