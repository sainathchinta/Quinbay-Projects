package com.gdn.micro.graphics.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;

public class ImageResultDetailList extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -1531236553372459922L;

  private List<ImageResultDetail> resultList;

  public ImageResultDetailList() {
    // nothing to do here
  }

  public ImageResultDetailList(List<ImageResultDetail> resultList) {
    this.resultList = resultList;
  }

  public List<ImageResultDetail> getResultList() {
    return resultList;
  }

  public void setResultList(List<ImageResultDetail> resultList) {
    this.resultList = resultList;
  }

}
