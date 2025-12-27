package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

import java.util.List;

/**
 * Created by priyanka on 19/06/17.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryCodeRequest extends BaseRequest{
  private static final long serialVersionUID = 5086001598843317432L;

  private List<String> categoryCodes;

  public CategoryCodeRequest() {
  }

  public CategoryCodeRequest(List<String> categoryCodes) {
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
