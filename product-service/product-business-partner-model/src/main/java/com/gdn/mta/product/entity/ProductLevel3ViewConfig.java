package com.gdn.mta.product.entity;

import java.io.Serializable;

import com.gdn.common.web.base.BaseResponse;
import com.gdn.partners.pbp.model.productlevel3.BuyableScheduleDTO;
import com.gdn.partners.pbp.model.productlevel3.DiscoverableScheduleDTO;

import lombok.AllArgsConstructor;
import lombok.Builder;

@AllArgsConstructor
@Builder
public class ProductLevel3ViewConfig extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 323768792600124290L;
  private String channelId;
  private Boolean display;
  private Boolean buyable;
  private BuyableScheduleDTO buyableScheduleDTO;
  private DiscoverableScheduleDTO discoverableScheduleDTO;
  
  public ProductLevel3ViewConfig(){
    // do nothing
  }

  public ProductLevel3ViewConfig(String channelId, Boolean display, Boolean buyable) {
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

  public BuyableScheduleDTO getBuyableScheduleDTO() {
    return buyableScheduleDTO;
  }

  public DiscoverableScheduleDTO getDiscoverableScheduleDTO() {
    return discoverableScheduleDTO;
  }

  public void setBuyableScheduleDTO(BuyableScheduleDTO buyableScheduleDTO) {
    this.buyableScheduleDTO = buyableScheduleDTO;
  }

  public void setDiscoverableScheduleDTO(DiscoverableScheduleDTO discoverableScheduleDTO) {
    this.discoverableScheduleDTO = discoverableScheduleDTO;
  }



  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel3ViewConfig [channelId=").append(channelId).append(", display=")
        .append(display).append(", buyable=").append(buyable).append(", getChannelId()=")
        .append(getChannelId()).append(", getDisplay()=").append(getDisplay())
        .append(", getBuyable()=").append(getBuyable()).append("]");
    return builder.toString();
  }

}
