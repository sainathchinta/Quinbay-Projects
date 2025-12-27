package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCollectionWebResponse {

  private String id;
  private String storeId;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private Long version;
  private String productId;
  private String productCode;
  private String productName;
  private String brand;
  private String categoryCode;
  private String categoryName;
  private String businessPartnerCode;
  private String businessPartnerName;
  private boolean activated;
  private boolean viewable;
  private boolean contentApproved;
  private boolean imageApproved;
  private Date updatedStepDate;
  private boolean processImage;
  private String state;
  private Date submittedDate;
  private boolean reviewPending;
}
