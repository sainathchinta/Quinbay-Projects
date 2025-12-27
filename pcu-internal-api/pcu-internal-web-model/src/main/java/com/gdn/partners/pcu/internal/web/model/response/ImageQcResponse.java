package com.gdn.partners.pcu.internal.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude()
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImageQcResponse {
  private List<ImageFeedbackResponse> images;
  private List<RestrictionModelFeedBackResponse> restrictionModels;
  private List<BrandModelFeedBackResponse> brandModels;
  private List<CategoryModelFeedbackResponse> categoryModels;
}
