package com.gdn.partners.product.analytics.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class ProductOptimisationSuggestionFeedback implements Serializable {
  private static final long serialVersionUID = -6482943432259103480L;
  private String suggestionName;
  private String notes;
  private Boolean feedbackType;
  private Date updatedDate;
}
