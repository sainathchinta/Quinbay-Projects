package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.io.Serializable;
import java.util.Map;

/**
 * Created by w.william on 2/21/2018.
 */
public class AttributeVo implements Serializable {

  private static final long serialVersionUID = -2505762671082562572L;

  private String id;
  private Map<String, String> attributes;

  public AttributeVo(String id, Map<String, String> attributes) {
    this.id = id;
    this.attributes = attributes;
  }

  public AttributeVo() {
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public Map<String, String> getAttributes() {
    return attributes;
  }

  public void setAttributes(Map<String, String> attributes) {
    this.attributes = attributes;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("id", id).append("attributes", attributes).toString();
  }
}
