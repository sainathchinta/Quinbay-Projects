package com.gdn.x.product.model.vo;

import java.util.List;

import org.springframework.data.domain.Pageable;

public class SolrGroupResultVO<T> {
  List<T> content;
  long numGroups;
  Pageable pageable;

  public List<T> getContent() {
    return content;
  }

  public void setContent(List<T> content) {
    this.content = content;
  }

  public long getNumGroups() {
    return numGroups;
  }

  public void setNumGroups(long numGroups) {
    this.numGroups = numGroups;
  }

  public Pageable getPageable() {
    return pageable;
  }

  public void setPageable(Pageable pageable) {
    this.pageable = pageable;
  }

  public SolrGroupResultVO(List<T> content, Pageable pageable, long numGroups) {
    this.content = content;
    this.numGroups = numGroups;
    this.pageable = pageable;
  }

  public Integer getPageNumber() {
    return this.pageable.getPageNumber();
  }

  public Integer getPageSize() {
    return this.pageable.getPageSize();
  }
}
