package com.gdn.partners.pbp.entity.productlevel3;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;

public class CountProductLevel3Wip implements Serializable {

  private static final long serialVersionUID = -375928908651695697L;
  private Map<ProductLevel3WipSummaryCriteria, Long> totalItemsByCriterias =
      new HashMap<ProductLevel3WipSummaryCriteria, Long>();
  private Long totalItems;

  public CountProductLevel3Wip() {}

  public CountProductLevel3Wip(Map<ProductLevel3WipSummaryCriteria, Long> totalItemsByCriterias, Long totalItems) {
    super();
    this.totalItemsByCriterias = totalItemsByCriterias;
    this.totalItems = totalItems;
  }

  public Map<ProductLevel3WipSummaryCriteria, Long> getTotalItemsByCriterias() {
    return totalItemsByCriterias;
  }

  public void setTotalItemsByCriterias(Map<ProductLevel3WipSummaryCriteria, Long> totalItemsByCriterias) {
    this.totalItemsByCriterias = totalItemsByCriterias;
  }

  public Long getTotalItems() {
    return totalItems;
  }

  public void setTotalItems(Long totalItems) {
    this.totalItems = totalItems;
  }

  @Override
  public String toString() {
    return String.format("CountProductLevel3Wip [totalItemsByCriterias=%s, totalItems=%s]", totalItemsByCriterias,
        totalItems);
  }

}
