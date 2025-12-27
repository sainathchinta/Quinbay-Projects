package com.gdn.x.mta.distributiontask.domain.event.model;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class PDTProductUpdateProductToSolrEventModel {

  private String productCode;
  private String productName;
  private List<String> categoryCodes;
  private List<String> categoryNames;
  private String state;
  private Date updatedDate;
  private int reviewType;
  private Date approvedDate;
  private String brand;
  private String brandApprovalStatus;
  private boolean edited;
  private boolean postLive;
  private Integer sellerType;
  private String sellerBadge;
  private boolean appealedProduct;
  private int distributionMappingStatus;
  private String productCreationType;
}
