package com.gdn.x.productcategorybase.dto.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class RestrictedKeywordsListingResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = -2186824861829358117L;
  private String keywordId;
  private String keyword;
  private Boolean validateOnUi;
  private Boolean validateByDs;
}
