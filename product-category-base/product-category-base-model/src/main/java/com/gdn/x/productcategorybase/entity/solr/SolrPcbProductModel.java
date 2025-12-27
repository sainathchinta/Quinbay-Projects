package com.gdn.x.productcategorybase.entity.solr;


import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SolrPcbProductModel {

  private String id;
  private String name;
  private String categoryId;
  private String productCode;
  private String parentCategoryId;
  private List<String> upcCodes;
  private List<String> skuCodes;
  private List<String> generatedItemNames;
  private List<Integer> dangerousGoodsLevels;
  private List<String> imageLocationPaths;
  private boolean reviewPending;
}
