package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ListRequest<T> implements Serializable {

  private static final long serialVersionUID = 9164165929482091298L;

  private List<T> list;

  public ListRequest() {

  }

  public ListRequest(List<T> list) {
    super();
    this.list = list;
  }

  public List<T> getList() {
    return list;
  }

  public void setList(List<T> list) {
    this.list = list;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ListRequest{");
    sb.append("list=").append(list);
    sb.append('}');
    return sb.toString();
  }
}
