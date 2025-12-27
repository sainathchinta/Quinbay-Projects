package com.gdn.x.productcategorybase.dto.request;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class RestrictedKeywordsSearchRequest extends BaseRequest {

  private static final long serialVersionUID = 737613712036001321L;

  private String keyword;
}
