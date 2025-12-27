package com.gdn.mta.bulk.dto.product;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3ViewConfigRequest extends BaseRequest {

  private static final long serialVersionUID = -2614781582594208223L;
  private String channelId;
  private Boolean display;
  private Boolean buyable;

  public ProductLevel3ViewConfigRequest() {
  	// do nothing
  }

  public ProductLevel3ViewConfigRequest(String channelId, Boolean display, Boolean buyable) {
    super();
    this.channelId = channelId;
    this.display = display;
    this.buyable = buyable;
  }

  public String getChannelId() {
    return channelId;
  }

  public void setChannelId(String channelId) {
    this.channelId = channelId;
  }

  public Boolean getDisplay() {
    return display;
  }

  public void setDisplay(Boolean display) {
    this.display = display;
  }

  public Boolean getBuyable() {
    return buyable;
  }

  public void setBuyable(Boolean buyable) {
    this.buyable = buyable;
  }

  public static long getSerialversionuid() {
    return serialVersionUID;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3ViewConfigRequest [channelId=%s, display=%s, buyable=%s, getChannelId()=%s, getDisplay()=%s, getBuyable()=%s]",
            channelId, display, buyable, getChannelId(), getDisplay(), getBuyable());
  }


}
