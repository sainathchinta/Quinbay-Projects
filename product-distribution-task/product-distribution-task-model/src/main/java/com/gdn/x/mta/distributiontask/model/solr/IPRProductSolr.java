package com.gdn.x.mta.distributiontask.model.solr;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class IPRProductSolr {
  private String storeId;
  private boolean markForDelete;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String productCode;
  private String productSku;
  private String productName;
  private String categoryCode;
  private String categoryName;
  private String brandName;
  private String brandCode;
  private String businessPartnerCode;
  private Date productAddedDate;
  private String state;
  private String assignedTo;
  private Date assignedDate;
  private String source;
}
