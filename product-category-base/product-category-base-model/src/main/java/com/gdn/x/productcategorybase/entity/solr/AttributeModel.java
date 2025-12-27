package com.gdn.x.productcategorybase.entity.solr;

import java.io.Serializable;

/**
 * Created by Kesha on 02/05/16.
 */
public class AttributeModel implements Serializable{
  private static final long serialVersionUID = -8797722273438241663L;
  private String name;
  private String value;

  public AttributeModel(String name, String value) {
    this.name = name;
    this.value = value;
  }

  public AttributeModel() {
  }

  public String getName() {
    return name;
  }

  public String getValue() {
    return value;
  }
}
