package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import com.gdn.x.productcategorybase.dto.MigrationPayload;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class CommonImageBackfillingEventModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String productCode;
  private String migrationType;
  private List<MigrationPayload> migrationPayloadList;

  public CommonImageBackfillingEventModel(String storeId, String productCode) {
    this.storeId = storeId;
    this.productCode = productCode;
  }
}

