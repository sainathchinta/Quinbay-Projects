package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class VendorProductDetailWebResponse {

  private String storeId;
  private String productId;
  private String businessPartnerName;
  private String vendorId;
  private String vendorName;
  private String vendorCode;
  private VendorDetailWebResponse currentVendor;
  private boolean contentApproved;
  private String contentApproverAssignee;
  private Date contentAssignedDate;
  private Date contentApprovedDate;
  private boolean imageApproved;
  private String imageApproverAssignee;
  private Date imageAssignedDate;
  private Date imageApprovedDate;
  private boolean productApproved;
  private String productApproverAssignee;
  private Date productAssignedDate;
  private Date productApprovedDate;
  private Integer rejectedCount;
  private String state;
  private int distributionMappingStatus;
}
