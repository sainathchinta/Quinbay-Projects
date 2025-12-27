package com.gdn.x.productcategorybase.dto.response;

import java.util.Map;
import com.gdn.common.web.base.BaseResponse;

public class MapResponse<K,V> extends BaseResponse{
  private Map<K,V> map;

  public MapResponse() {
  }

  public MapResponse(Map<K,V> map) {
    this.map = map;
  }

  public Map<K, V> getMap() {
    return map;
  }

  public void setMap(Map<K, V> map) {
    this.map = map;
  }
}
