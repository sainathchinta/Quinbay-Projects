package com.gda.mta.product.dto;

import com.gda.mta.product.dto.response.BuyableScheduleResponse;
import com.gda.mta.product.dto.response.DiscoverableScheduleResponse;
import com.gdn.common.web.base.BaseResponse;
import lombok.ToString;

@ToString
public class ProductLevel3ViewConfigResponse extends BaseResponse {

  private static final long serialVersionUID = 6328542631160859870L;
  private String channelId;
  private Boolean display;
  private Boolean buyable;
  private BuyableScheduleResponse buyableScheduleResponse;
  private DiscoverableScheduleResponse discoverableScheduleResponse;

  public ProductLevel3ViewConfigResponse() {
    // do nothing
  }

  public ProductLevel3ViewConfigResponse(String channelId, Boolean display, Boolean buyable) {
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

  public BuyableScheduleResponse getBuyableScheduleResponse() {
    return buyableScheduleResponse;
  }

  public void setBuyableScheduleResponse(BuyableScheduleResponse buyableScheduleResponse) {
    this.buyableScheduleResponse = buyableScheduleResponse;
  }

  public DiscoverableScheduleResponse getDiscoverableScheduleResponse() {
    return discoverableScheduleResponse;
  }

  public void setDiscoverableScheduleResponse(DiscoverableScheduleResponse discoverableScheduleResponse) {
    this.discoverableScheduleResponse = discoverableScheduleResponse;
  }

}
