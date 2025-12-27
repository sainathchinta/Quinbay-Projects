package com.gdn.x.productcategorybase.dto;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gdn.common.web.base.BaseRequest;

/**
 * Object request to hold list of requests, to avoid direct List usage as body request in controller level
 * 
 * @author agie.falah
 *
 */
public class ListHolderRequest<T> extends BaseRequest {
  private static final long serialVersionUID = 2409051754119883352L;
  private List<T> lists;

  public ListHolderRequest() {
    super();
  }

  public List<T> getLists() {
    return lists;
  }

  public void setLists(List<T> lists) {
    this.lists = lists;
  }
  
  @Override
  public String toString() {
    return new ToStringBuilder(this).append("lists", lists).toString();
  }
}
