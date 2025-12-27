package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class LookupResponse extends BaseResponse {

  private static final long serialVersionUID = 6243678682799331799L;

  private String id;
  private String lookupGroup;
  private String code;
  private String description;
  private Integer orderNumber;
  private String name;
}
