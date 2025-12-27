package com.gdn.mta.bulk.entity;

import java.io.Serializable;

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
public class BulkCreateProductEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -2867881121693796845L;

  private String storeId;
  private String businessPartnerCode;
  private String bulkProcessCode;
  private String parentProduct;
}
