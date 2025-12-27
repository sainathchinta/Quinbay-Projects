package com.gdn.mta.bulk.dto.product;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class DownloadUnmappedSkuDomainEventModel extends GdnBaseDomainEventModel {
  private String storeId;
  private String username;
  private String emailTo;
  private String requestId;
  private String parentCategoryCode;
  private String language;
}
