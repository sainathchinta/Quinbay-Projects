package com.gdn.partners.product.analytics.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
public class ProductOptimisationSuggestions implements Serializable {

  @Serial
  private static final long serialVersionUID = -7209119878797952175L;
  private String suggestionType;
  private List<String> suggestionMetadata;
  private Boolean feedbackType;
}