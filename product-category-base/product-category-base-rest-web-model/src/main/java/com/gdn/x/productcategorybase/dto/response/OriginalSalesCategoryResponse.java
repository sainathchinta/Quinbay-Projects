package com.gdn.x.productcategorybase.dto.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
@Builder
@ToString
public class OriginalSalesCategoryResponse extends BaseDTOResponse {


  private static final long serialVersionUID = 5011111597043317432L;

  private String oscCode;
  private String oscShortText;
  private String oscLongText;
  private boolean activated = true;
  private List<CategoryResponse> masterCategories = new ArrayList<>();
}
