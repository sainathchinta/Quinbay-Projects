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
public class BulkInternalEventModel extends GdnBaseDomainEventModel {

  private String storeId;
  private String requestId;
  private String internalProcessRequestCode;
  private List<String> processCode;
}
