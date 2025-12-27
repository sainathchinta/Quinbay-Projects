package com.gdn.mta.bulk.dto.product;

import java.io.Serializable;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;

/**
 * Used to transfer attribute map that are grouped by attribute type and SKU value flag
 * 
 * @author agie.falah
 *
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeMapDto implements Serializable {
  private static final long serialVersionUID = 1809791210281302158L;
  private Map<String, AttributeResponse> defAttrMap;
  private Map<String, AttributeResponse> nonDefAttrMap;
  private Map<String, AttributeResponse> nonDefAttrMapSkuValue;
  
  public AttributeMapDto() {}
  
  public AttributeMapDto(Map<String, AttributeResponse> defAttrMap,
      Map<String, AttributeResponse> nonDefAttrMap,
      Map<String, AttributeResponse> nonDefAttrMapSkuValue) {
    this.defAttrMap = defAttrMap;
    this.nonDefAttrMap = nonDefAttrMap;
    this.nonDefAttrMapSkuValue = nonDefAttrMapSkuValue;
  }
  public Map<String, AttributeResponse> getDefAttrMap() {
    return defAttrMap;
  }
  public void setDefAttrMap(Map<String, AttributeResponse> defAttrMap) {
    this.defAttrMap = defAttrMap;
  }
  public Map<String, AttributeResponse> getNonDefAttrMap() {
    return nonDefAttrMap;
  }
  public void setNonDefAttrMap(Map<String, AttributeResponse> nonDefAttrMap) {
    this.nonDefAttrMap = nonDefAttrMap;
  }
  public Map<String, AttributeResponse> getNonDefAttrMapSkuValue() {
    return nonDefAttrMapSkuValue;
  }
  public void setNonDefAttrMapSkuValue(Map<String, AttributeResponse> nonDefAttrMapSkuValue) {
    this.nonDefAttrMapSkuValue = nonDefAttrMapSkuValue;
  }
  
}
