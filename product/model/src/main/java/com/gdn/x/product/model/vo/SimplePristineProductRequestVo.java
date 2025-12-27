package com.gdn.x.product.model.vo;

import org.apache.commons.lang3.builder.ToStringBuilder;


import java.util.Set;

/**
 * Created by govind on 20/09/2017
 */
public class SimplePristineProductRequestVo {

  private String pristineId;
  private Set<SimpleProductRequestVo> simpleProductDTOList;

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public Set<SimpleProductRequestVo> getSimpleProductDTOList() {
    return simpleProductDTOList;
  }

  public void setSimpleProductDTOList(
      Set<SimpleProductRequestVo> simpleProductDTOList) {
    this.simpleProductDTOList = simpleProductDTOList;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("pristineId", pristineId)
        .append("simpleProductDTOList", simpleProductDTOList).toString();
  }
}
