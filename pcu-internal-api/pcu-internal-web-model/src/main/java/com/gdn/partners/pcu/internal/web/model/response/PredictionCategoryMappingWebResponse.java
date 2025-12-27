package com.gdn.partners.pcu.internal.web.model.response;


import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
public class PredictionCategoryMappingWebResponse {
  private static final long serialVersionUID = -9104927065138499267L;
  private String categoryCode;
  private String categoryName;
}
