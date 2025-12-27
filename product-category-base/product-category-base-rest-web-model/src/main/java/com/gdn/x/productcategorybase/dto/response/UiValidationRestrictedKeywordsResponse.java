package com.gdn.x.productcategorybase.dto.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
@AllArgsConstructor
@NoArgsConstructor
public class UiValidationRestrictedKeywordsResponse extends BaseResponse {
  private static final long serialVersionUID = 6814057016154730733L;
  private String keywordId;
  private String keyword;
}
