package com.gdn.mta.product.valueobject;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3WipDTO implements Serializable {
  private static final long serialVersionUID = -8386105716627632807L;
  private String id;
  private String storeId;
  private Long version;
  private Date createdDate;
  private String createdBy;
  private Date updatedDate;
  private String updatedBy;
  private boolean markForDelete;
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
  private List<ForceReviewImageViolation> forceReviewImageViolations;
}
