package com.gdn.x.productcategorybase.solr.model;

/**
 * Created by Kesha on 24/04/16.
 */
public class AttributeModel {
  String name;
  String value;

  public AttributeModel(String name, String value) {
    this.name = name;
    this.value = value;
  }

  public String getName() {
    return name;
  }

  public String getValue() {
    return value;
  }
}
