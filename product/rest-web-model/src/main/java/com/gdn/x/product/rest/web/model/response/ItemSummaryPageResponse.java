package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.util.List;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemSummaryPageResponse implements Serializable {
  private static final long serialVersionUID = 1L;

  private List<ItemSummaryResponse> itemSummaryResponses;
  private long totalNum;
  private long totalPage;

  public ItemSummaryPageResponse() {}

  public ItemSummaryPageResponse(List<ItemSummaryResponse> itemSummaryResponses, long totalNum,
      long totalPage) {
    super();
    this.itemSummaryResponses = itemSummaryResponses;
    this.totalNum = totalNum;
    this.totalPage = totalPage;
  }

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }

  public List<ItemSummaryResponse> getItemSummaryResponses() {
    return this.itemSummaryResponses;
  }

  public long getTotalNum() {
    return this.totalNum;
  }

  public long getTotalPage() {
    return this.totalPage;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItemSummaryResponses(List<ItemSummaryResponse> itemSummaryResponses) {
    this.itemSummaryResponses = itemSummaryResponses;
  }

  public void setTotalNum(long totalNum) {
    this.totalNum = totalNum;
  }


  public void setTotalPage(long totalPage) {
    this.totalPage = totalPage;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ItemSummaryResponsePage [itemSummaryResponses=%s, totalNum=%s, totalPage=%s, toString()=%s]",
            this.itemSummaryResponses, this.totalNum, this.totalPage, super.toString());
  }
}
