package com.gdn.x.productcategorybase.dto.response;

import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

/**
 * Created by sarang on 31/05/17.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryNamesResponse extends BaseResponse{

  private Map<String, String> categoryMap = new HashMap<>();

  public CategoryNamesResponse() {
  }

  public CategoryNamesResponse(Map<String, String> categoryMap) {
    this.categoryMap = categoryMap;
  }

  public Map<String, String> getCategoryMap() {
    return categoryMap;
  }

  public void setCategoryMap(Map<String, String> categoryMap) {
    this.categoryMap = categoryMap;
  }

  @Override public String toString() {
    return new ToStringBuilder(this).append("categoryMap", categoryMap).toString();
  }
}
