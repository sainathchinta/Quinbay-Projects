package com.gdn.mta.bulk.dto;

import java.util.List;

import com.gdn.common.web.base.BaseRequest;

/**
 * Object request to hold list of requests for respective rest API
 * 
 * @author agie.falah
 *
 * @param <T>
 */
public class ListHolderRequest<T> extends BaseRequest {
  private static final long serialVersionUID = -9149307505687239221L;
  private List<T> lists;
  
  public ListHolderRequest() {
    super();
  }
  public ListHolderRequest(List<T> lists) {
    super();
    this.lists = lists;
  }
  public List<T> getLists() {
    return lists;
  }
  public void setLists(List<T> lists) {
    this.lists = lists;
  }
  
}
