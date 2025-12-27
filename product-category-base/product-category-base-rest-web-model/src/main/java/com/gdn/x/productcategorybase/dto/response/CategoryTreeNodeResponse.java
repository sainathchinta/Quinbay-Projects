package com.gdn.x.productcategorybase.dto.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@NoArgsConstructor
public class CategoryTreeNodeResponse extends BaseResponse {

  private static final long serialVersionUID = -4359863963131951586L;

  private String id;
  private String name;
  private String code;
  private boolean review;
  private List<CategoryTreeNodeResponse> child = new ArrayList<>();

  public CategoryTreeNodeResponse(String id, String name, String categoryCode, boolean review) {
    this.id = id;
    this.name = name;
    this.code = categoryCode;
    this.review = review;
  }
}
