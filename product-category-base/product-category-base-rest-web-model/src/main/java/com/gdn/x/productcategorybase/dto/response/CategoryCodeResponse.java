package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import java.util.List;

/**
 * Created by priyanka on 19/06/17.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryCodeResponse extends BaseResponse {

  private static final long serialVersionUID = 5086001597045517432L;

  private List<String> categoryCodes;

  public CategoryCodeResponse() {
  }

  public CategoryCodeResponse(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
  }

  public List<String> getCategoryCodes() {
    return categoryCodes;
  }

  public void setCategoryCodes(List<String> categoryCodes) {
    this.categoryCodes = categoryCodes;
  }

  /*String Builder*/
}
