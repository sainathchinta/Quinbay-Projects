package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.partners.pcu.internal.web.model.enums.WorkflowWebState;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class DistributionProductWebResponse {


  private static final long serialVersionUID = 7775362863314098484L;

  private String id;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private Date productCreatedDate;
  private String productCode;
  private String productName;
  private String videoUrl;
  private String uom;
  private String categoryName;
  private String categoryCode;
  private String brand;
  private byte[] description;
  private byte[] longDescription;
  private String uniqueSellingPoint;
  private String productStory;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private String businessPartnerCode;
  private String businessPartnerName;
  private Double shippingWeight;
  private WorkflowWebState state;
  private Integer rejectedCount;
  private VendorDetailWebResponse currentVendor;
  private String vendorId;
  private String vendorName;
  private boolean productApproved;
  private String vendorCode;
  private String storeId;
  private String productId;
  private String productApproverAssignee;
  private Date productAssignedDate;
  private Date productApprovedDate;
  private boolean postLive;
  private String commissionType;
  private boolean internationalFlag;
  private boolean edited;
  private boolean revised;
  private boolean restrictedKeywordsPresent;
  private boolean autoNeedRevision;
  private String reviewType;
  private List<String> imageViolations;
  private String predictedBrand;
  private Integer sellerType;
  private boolean b2cActivated;
  private boolean b2bActivated;
  private boolean officialSeller;
  private String sellerStatus;
  private boolean appealedProduct;
  private String c1CategoryCode;
  private String c1CategoryName;
  private int distributionMappingStatus;
  private String productCreationType;
  private boolean aiGeneratedCategory;
  private boolean aiGeneratedBrand;
}
