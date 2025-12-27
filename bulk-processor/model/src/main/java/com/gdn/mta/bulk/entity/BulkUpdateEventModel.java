package com.gdn.mta.bulk.entity;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkUpdateEventModel extends GdnBaseDomainEventModel {

  private String storeId;
  private String businessPartnerCode;
  private String bulkProcessCode;
  private List<Integer> rowNumbers;
}
