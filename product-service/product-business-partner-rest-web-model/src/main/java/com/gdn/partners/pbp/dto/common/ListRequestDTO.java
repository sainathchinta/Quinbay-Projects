package com.gdn.partners.pbp.dto.common;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ListRequestDTO<T> implements Serializable {

  private static final long serialVersionUID = -6499818897147992837L;

  private List<T> list;

  public ListRequestDTO() {
  }

  public ListRequestDTO(List<T> list) {
    super();
    this.list = list;
  }

  public List<T> getList() {
    return this.list;
  }

  public void setList(List<T> list) {
    this.list = list;
  }

  @Override
  public String toString() {
    return String.format("ListRequestDTO [list=%s, toString()=%s]", this.list, super.toString());
  }
}
