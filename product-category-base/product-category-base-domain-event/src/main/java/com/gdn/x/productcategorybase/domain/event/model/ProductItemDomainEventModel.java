package com.gdn.x.productcategorybase.domain.event.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemDomainEventModel {

  private String generatedItemName;
  private String upcCode;
  private String skuCode;
  private boolean activated;
  private boolean viewable;
  private List<ImageDomainEventModel> images;
  private Integer dangerousGoodsLevel;
  private boolean contentChanged;
  private boolean publishL4;

  private DistributionItemInfoEventModel distributionItemInfo;
  private List<DimensionsAndUomEventModel> dimensionsAndUOM;
}
