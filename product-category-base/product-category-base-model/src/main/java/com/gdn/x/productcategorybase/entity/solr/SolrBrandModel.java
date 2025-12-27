package com.gdn.x.productcategorybase.entity.solr;


import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SolrBrandModel {

  private String id;
  private String brandCode;
  private String brandValue;
  private Date updatedDate;
  private String businessPartnerCode;
  private boolean brandApproved;
  private boolean protectedBrand;
  private Boolean brandNameUpdated;
}
