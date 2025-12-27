package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ReviewProductWebResponse {

  private String id;
  private String createdBy;
  private Date createdDate;
  private Date updatedDate;
  private String productId;
  private String productCode;
  private String productName;
  private String brand;
  private String categoryCode;
  private String categoryName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private String assignedTo;
  private Date submittedDate;
  private String state;
  private String commissionType;
  private boolean internationalFlag;
  private boolean isSourceDb;
}
