package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductOptimisationSuggestionResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 8625045774874861405L;
  private String suggestionType;
  private List<String> suggestionMetadata;
  private String impact;
  private String impactEn;
  private Boolean feedbackType;
  private String suggestionDisplayName;
  private String suggestionDisplayNameEn;
}
