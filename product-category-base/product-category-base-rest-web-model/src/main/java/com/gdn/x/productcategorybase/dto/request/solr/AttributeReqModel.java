package com.gdn.x.productcategorybase.dto.request.solr;

import java.io.Serializable;

/**
 * Created by Kesha on 03/05/16.
 */
public class AttributeReqModel implements Serializable {
  private static final long serialVersionUID = -8797722273438241663L;
  private String name;
  private String value;

  public AttributeReqModel(String name, String value) {
    this.name = name;
    this.value = value;
  }

  public AttributeReqModel() {
  }

  public String getName() {
    return name;
  }

  public String getValue() {
    return value;
  }
}
