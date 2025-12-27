package com.gdn.x.product.rest.web.model.response;

import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.gdn.common.web.base.BaseResponse;

/**
 * Created by Vishal on 04/10/17.
 */
public class PristineCategoryAttributeMapResponse extends BaseResponse {

  private static final long serialVersionUID = 7362822966512609305L;
  private Map<String, String> categoryAttributeMap;

  public Map<String, String> getCategoryAttributeMap() {
    return categoryAttributeMap;
  }

  public void setCategoryAttributeMap(Map<String, String> categoryAttributeMap) {
    this.categoryAttributeMap = categoryAttributeMap;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("categoryAttributeMap", categoryAttributeMap)
        .toString();
  }
}
