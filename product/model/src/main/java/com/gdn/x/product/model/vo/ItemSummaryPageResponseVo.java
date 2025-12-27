package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;
import java.util.List;

public class ItemSummaryPageResponseVo implements Serializable {
  private static final long serialVersionUID = 1L;

  private List<ItemSummaryResponseVO> itemSummaryResponses;
  private long totalNum;
  private long totalPage;

  public ItemSummaryPageResponseVo() {}

  public ItemSummaryPageResponseVo(List<ItemSummaryResponseVO> itemSummaryResponses, long totalNum,
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

  public List<ItemSummaryResponseVO> getItemSummaryResponses() {
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

  public void setItemSummaryResponses(List<ItemSummaryResponseVO> itemSummaryResponses) {
    this.itemSummaryResponses = itemSummaryResponses;
  }

  public void setTotalNum(long totalNum) {
    this.totalNum = totalNum;
  }


  public void setTotalPage(long totalPage) {
    this.totalPage = totalPage;
  }

  @Override public String toString() {
    return new ToStringBuilder(this).append("itemSummaryResponses", itemSummaryResponses)
        .append("totalNum", totalNum).append("totalPage", totalPage).toString();
  }
}
