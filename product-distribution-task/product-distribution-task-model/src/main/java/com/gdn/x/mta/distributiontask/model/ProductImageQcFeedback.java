package com.gdn.x.mta.distributiontask.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Table(name = "PDT_PRODUCT_IMAGE_QC_FEEDBACK")
@Entity
public class ProductImageQcFeedback extends GdnBaseEntity {

  @Column(name = "PRODUCT_CODE", nullable = false, unique = true)
  private String productCode;

  @Column(name = "SYSTEM_FEEDBACK")
  private String systemFeedback;

  @Column(name = "USER_FEEDBACK")
  private String userFeedback;

  @Column(name = "FEEDBACK_UPDATED")
  private boolean feedbackUpdated;

}
