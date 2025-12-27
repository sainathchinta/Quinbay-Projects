package com.gdn.micro.graphics.web.helper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.common.web.base.BaseResponse;

public class ImageResultByTypeResponse extends BaseResponse {

  private static final long serialVersionUID = 6070988313841385957L;

  private Map<String, List<ConvertImageResponse>> resultMap;

  public ImageResultByTypeResponse() {
    this.resultMap = new HashMap<>();
  }

  public ImageResultByTypeResponse(Map<String, List<ConvertImageResponse>> map) {
    this.resultMap = map;
  }

  public void addImage(String imageType, ConvertImageResponse imageResponse) {
    if (!this.resultMap.containsKey(imageType))
      this.resultMap.put(imageType, new ArrayList<ConvertImageResponse>());

    this.resultMap.get(imageType).add(imageResponse);
  }

  public Map<String, List<ConvertImageResponse>> getResultMap() {
    return resultMap;
  }

  public int size() {
    int result = 0;
    for (Map.Entry<String, List<ConvertImageResponse>> a : resultMap.entrySet()) {
      result += a.getValue().size();
    }
    return result;
  }
}
