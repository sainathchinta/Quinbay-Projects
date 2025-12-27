package com.gdn.x.productcategorybase.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SizeChartDataColumn {

  private String keyName;
  private String keyType;
  private String value;
  private String min;
  private String max;
}
