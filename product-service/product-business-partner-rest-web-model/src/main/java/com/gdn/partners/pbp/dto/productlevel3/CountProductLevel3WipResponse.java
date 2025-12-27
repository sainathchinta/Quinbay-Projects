package com.gdn.partners.pbp.dto.productlevel3;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CountProductLevel3WipResponse extends BaseResponse {

  private static final long serialVersionUID = 4529665961797962792L;
  private Map<ProductLevel3WipSummaryCriteria, Long> totalItemsByCriterias =
      new HashMap<ProductLevel3WipSummaryCriteria, Long>();
  private Long totalItems;

  public CountProductLevel3WipResponse() {}

  public CountProductLevel3WipResponse(Map<ProductLevel3WipSummaryCriteria, Long> totalItemsByCriterias, Long totalItems) {
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
    return String.format("CountProductLevel3WipResponse [totalItemsByCriterias=%s, totalItems=%s]",
        totalItemsByCriterias, totalItems);
  }

}
