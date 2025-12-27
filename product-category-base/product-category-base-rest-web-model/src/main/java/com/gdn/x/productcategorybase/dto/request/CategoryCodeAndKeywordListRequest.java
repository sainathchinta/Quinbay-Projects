package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
@Builder
public class CategoryCodeAndKeywordListRequest extends BaseDTORequest {

  private static final long serialVersionUID = 1986215406928901740L;

  private String categoryCode;
  private List<String> keywordIds = new ArrayList<>();
}
