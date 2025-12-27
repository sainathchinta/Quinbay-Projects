package com.gdn.x.product.rest.web.model;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDiscoverableRequest extends BaseRequest {

  private static final long serialVersionUID = -5285256509130637987L;

  private String channel;

  private boolean discoverable;

  public ItemDiscoverableRequest() {}

  public ItemDiscoverableRequest(String channel, boolean discoverable) {
    this.channel = channel;
    this.discoverable = discoverable;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getChannel() {
    return this.channel;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isDiscoverable() {
    return this.discoverable;
  }

  public void setChannel(String channel) {
    this.channel = channel;
  }

  public void setDiscoverable(boolean discoverable) {
    this.discoverable = discoverable;
  }

  @Override
  public String toString() {
    return String.format("ItemDiscoverableRequest [channel=%s, discoverable=%s, toString()=%s]",
        this.channel, this.discoverable, super.toString());
  }

}
