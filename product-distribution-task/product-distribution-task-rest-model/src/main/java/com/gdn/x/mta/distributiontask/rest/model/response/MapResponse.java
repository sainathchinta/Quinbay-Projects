package com.gdn.x.mta.distributiontask.rest.model.response;

import java.util.Map;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
public class MapResponse extends BaseResponse {
  private static final long serialVersionUID = -6919973386295905719L;
  private Map<String, Object> map;
  public Map<String, Object> getMap() {
    return map;
  }
  public void setMap(Map<String, Object> map) {
    this.map = map;
  }
  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("MapResponse [map=");
    builder.append(map);
    builder.append("]");
    return builder.toString();
  }
}
