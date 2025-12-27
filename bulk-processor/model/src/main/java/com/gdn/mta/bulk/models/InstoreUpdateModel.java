package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class InstoreUpdateModel {
  private String productSku;
  private String productName;
  private String off2OnFlag;
}
