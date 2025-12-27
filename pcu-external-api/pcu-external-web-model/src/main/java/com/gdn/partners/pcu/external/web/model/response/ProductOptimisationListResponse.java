package com.gdn.partners.pcu.external.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductOptimisationListResponse extends BaseResponse {

  private static final long serialVersionUID = -75110969111143792L;

  private String productSku;
  private String productName;
  private String categoryName;
  private String categoryCode;
  private float productScore;
  private Date suggestedDate;
  private String status;
  private String image;
  private List<ProductOptimisationSuggestionResponse> suggestions;
}