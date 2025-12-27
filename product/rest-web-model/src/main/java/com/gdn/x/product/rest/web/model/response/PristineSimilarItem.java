package com.gdn.x.product.rest.web.model.response;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;
import java.util.Map;

/**
 * Created by keshashah on 18/12/17.
 */
public class PristineSimilarItem implements Serializable {
  private static final long serialVersionUID = -2505762671082562572L;
  private String id;
  private String name;
  private Map<String, String> attributes;

  public PristineSimilarItem() {
    // No Implementation
  }

  public PristineSimilarItem(String id, String name, Map<String, String> attributes) {
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

  public Map<String, String> getAttributes() {
    return attributes;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("id", id)
        .append("name", name)
        .append("attributes", attributes).toString();
  }
}
