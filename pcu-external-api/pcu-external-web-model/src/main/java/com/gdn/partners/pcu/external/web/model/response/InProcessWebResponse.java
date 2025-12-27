package com.gdn.partners.pcu.external.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.List;

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
public class InProcessWebResponse {
  private static final long serialVersionUID = 3582406834956425665L;
  private String id;
  private String storeId;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private Long version;
  private String businessPartnerCode;
  private String productLevel1Id;
  private String productSku;
  private String itemName;
  private String categoryName;
  private String brand;
  private boolean active;
  private String state;
  private String reason;
  private Date submittedDate;
  private Date expectedActivationDate;
  private String productMainImage;
  private boolean isActiveImage;
  private boolean forceReview;
  private String productCode;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private boolean bundleProduct;
  private boolean appealedProduct;
  private List<ForceReviewImageViolationWebResponse> forceReviewImageViolations;
}
