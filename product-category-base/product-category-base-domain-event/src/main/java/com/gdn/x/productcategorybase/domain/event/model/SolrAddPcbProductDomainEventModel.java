package com.gdn.x.productcategorybase.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SolrAddPcbProductDomainEventModel implements Serializable {

  private static final long serialVersionUID = 8340249296830929545L;
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
}
