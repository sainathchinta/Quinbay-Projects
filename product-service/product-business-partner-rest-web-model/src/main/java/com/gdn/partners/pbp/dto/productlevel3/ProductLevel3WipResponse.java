package com.gdn.partners.pbp.dto.productlevel3;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
@Data
@ToString
@Builder
public class ProductLevel3WipResponse extends BaseResponse {

  private static final long serialVersionUID = -7823266558212576360L;
  
  private String businessPartnerCode;
  private String productLevel1Id;
  private String productSku;
  private String productName;
  private String categoryName;
  private String brandName;
  private boolean active = false;
  private String state;
  private String notes;
  private Date submittedDate;
  private Date expectedActivationDate;
  private String productMainImage;
  private boolean isActiveImage;
  private boolean forceReview;
  private String productCode;
  private boolean bundleProduct;
  private boolean appealedProduct;
  private List<ForceReviewImageViolationResponse> forceReviewImageViolations;
}
