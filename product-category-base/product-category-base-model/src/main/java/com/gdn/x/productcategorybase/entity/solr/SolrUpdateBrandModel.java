package com.gdn.x.productcategorybase.entity.solr;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SolrUpdateBrandModel {

  private String id;
  private String brandCode;
  private String businessPartnerCode;
  private boolean brandApproved;
  private Boolean brandNameUpdated;
  private String brandName;
}
