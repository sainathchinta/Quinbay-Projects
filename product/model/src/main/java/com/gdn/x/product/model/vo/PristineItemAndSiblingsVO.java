package com.gdn.x.product.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.entity.PristineDataItem;

import java.io.Serializable;
import java.util.List;


@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineItemAndSiblingsVO implements Serializable{

  private static final long serialVersionUID = 1L;

  private PristineDataItem pristineDataItem;

  private List<PristineDataItem> siblings;

  public PristineItemAndSiblingsVO() {
  }

  public PristineItemAndSiblingsVO(PristineDataItem pristineDataItem, List<PristineDataItem> siblings) {
    this.pristineDataItem = pristineDataItem;
    this.siblings = siblings;
  }

  public PristineDataItem getPristineDataItem() {
    return pristineDataItem;
  }

  public void setPristineDataItem(PristineDataItem pristineDataItem) {
    this.pristineDataItem = pristineDataItem;
  }

  public List<PristineDataItem> getSiblings() {
    return siblings;
  }

  public void setSiblings(List<PristineDataItem> siblings) {
    this.siblings = siblings;
  }
}
