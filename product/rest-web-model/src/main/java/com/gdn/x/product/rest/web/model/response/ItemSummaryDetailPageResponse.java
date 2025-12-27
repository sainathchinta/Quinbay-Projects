package com.gdn.x.product.rest.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ItemSummaryDetailPageResponse {
  private List<ItemSummaryDetailResponse> itemSummaryDetailResponses;
  private long totalNum;
  private long totalPage;

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }
}
