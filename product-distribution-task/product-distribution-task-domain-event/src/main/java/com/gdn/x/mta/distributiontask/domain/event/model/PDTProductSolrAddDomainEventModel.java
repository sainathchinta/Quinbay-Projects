package com.gdn.x.mta.distributiontask.domain.event.model;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PDTProductSolrAddDomainEventModel implements Serializable {

  private String storeId;
  private boolean markForDelete;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String productCode;
  private String productName;
  private boolean postLive;
  private List<String> categoryCodes;
  private List<String> categoryNames;
  private String brand;
  private String businessPartnerCode;
  private String businessPartnerName;
  private Date productCreatedDate;
  private int rejectedCount;
  private String state;
  private String vendorCode;
  private String productApproverAssignee;
  private Date productAssignedDate;
  private Date productApprovedDate;
  private String brandApprovalStatus;
  private int productPredictionScore;
  private String imageViolations;
  private int qcRetryCount;
  private String reviewType;
  private boolean restrictedKeywordsPresent;
  private boolean revised;
  private boolean edited;
  private boolean appealedProduct;
  private String predictedBrand;
  private Integer sellerType;
  private String sellerBadge;
  private List<String> productChannel;
  private int distributionMappingStatus;
  private String productCreationType;
}
