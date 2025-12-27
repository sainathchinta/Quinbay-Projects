package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.SuggestedCategory;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class SuggestedCategoryResponse implements Serializable {

  private static final long serialVersionUID = -3515759613792697141L;
  private long confidence;
  private String categoryCode;
  private String categoryId;
  private String categoryLevel;
  private String categoryName;
  private String categoryNameEnglish;
  private List<SuggestedCategory> suggestedCategory = new ArrayList<>();
}
