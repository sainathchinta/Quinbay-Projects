package com.gdn.partners.pbp.dto.productlevel3;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3CountOOSResponse extends BaseResponse {
  private static final long serialVersionUID = -9021799208870973154L;

  private long count;

  public long getCount() {
    return this.count;
  }

  public void setCount(long count) {
    this.count = count;
  }

  @Override
  public String toString() {
    return String.format("ProductLevel3CountOOSResponse [count=%s]", this.count);
  }
}
