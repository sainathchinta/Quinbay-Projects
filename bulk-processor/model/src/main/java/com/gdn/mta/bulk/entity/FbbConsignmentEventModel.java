package com.gdn.mta.bulk.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.bulk.dto.FbbItem;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class FbbConsignmentEventModel extends GdnBaseDomainEventModel {

  private String consignmentId;
  private String businessPartnerCode;
  private List<FbbItem> fbbItems;
}
