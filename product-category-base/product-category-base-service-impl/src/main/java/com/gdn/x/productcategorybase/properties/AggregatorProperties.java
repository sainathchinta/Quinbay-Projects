package com.gdn.x.productcategorybase.properties;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class AggregatorProperties {

  int pageSize;

  int totalData;

  int totalPage;

  public AggregatorProperties(int pageSize, int totalData) {
    this.pageSize = pageSize;
    this.totalData = totalData;
    this.totalPage = totalData / pageSize;
  }

}
