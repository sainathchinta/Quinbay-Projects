package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoApprovalTypeRequest implements Serializable {
  private static final long serialVersionUID = -3522711838190504547L;

  private String categoryCode;
  private boolean edited;
  private String reviewType;
  private boolean revised;
  private String productCode;
  private String storeId;
  private String destinationCategoryCode;

  public AutoApprovalTypeRequest(String categoryCode, boolean edited, String reviewType, boolean revised) {
    this.categoryCode = categoryCode;
    this.edited = edited;
    this.reviewType = reviewType;
    this.revised = revised;
  }
}
