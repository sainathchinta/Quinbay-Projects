package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.Map;

/**
 * Created by keshashah on 18/12/17.
 */
public class PristineSimilarItemVo implements Serializable {
  private static final long serialVersionUID = -2505762671082562572L;
  private String id;
  private String name;
  private Map<String, String> attributes;

  public PristineSimilarItemVo(String id, String name, Map<String, String> attributes) {
    this.id = id;
    this.name = name;
    this.attributes = attributes;
  }

  public String getName() {
    return name;
  }

  public String getId() {
    return id;

  }

  public void setAttributes(Map<String, String> attributes) {
    this.attributes = attributes;
  }

  public Map<String, String> getAttributes() {
    return attributes;
  }
}
