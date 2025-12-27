package com.gdn.x.productcategorybase.dto.request;

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
public class OriginalSalesCategoryRequest extends BaseDTORequest {

  private static final long serialVersionUID = 5011111597043317432L;

  private String oscCode;
  private String oscShortText;
  private String oscLongText;
  private boolean activated = true;
}
