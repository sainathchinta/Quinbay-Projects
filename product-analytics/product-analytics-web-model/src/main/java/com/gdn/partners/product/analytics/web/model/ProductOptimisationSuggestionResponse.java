package com.gdn.partners.product.analytics.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
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
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductOptimisationSuggestionResponse extends BaseResponse implements Serializable {

  @Serial
  private static final long serialVersionUID = -9168542958080120016L;
  private String suggestionType;
  private List<String> suggestionMetadata;
  private String impact;
  private String impactEn;
  private Boolean feedbackType;
  private String suggestionDisplayName;
  private String suggestionDisplayNameEn;
}