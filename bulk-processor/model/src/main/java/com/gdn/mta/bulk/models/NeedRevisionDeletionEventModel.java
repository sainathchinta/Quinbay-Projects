package com.gdn.mta.bulk.models;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class NeedRevisionDeletionEventModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String businessPartnerCode;
  private String deletionProcessCode;
  private List<String> needRevisionDeletionDataIds;
}
