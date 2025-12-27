package com.gdn.x.product.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductSkuSummaryRequestVo {

  private String productSkuName;
  private List<String> productSkus;
  private List<String> categoryCodes;
  private List<String> brand;
  private String sortOrder;
  private Boolean isArchived;
}
