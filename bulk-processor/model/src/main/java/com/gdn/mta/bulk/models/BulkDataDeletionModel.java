package com.gdn.mta.bulk.models;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BulkDataDeletionModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String bulkProcessCode;
}
