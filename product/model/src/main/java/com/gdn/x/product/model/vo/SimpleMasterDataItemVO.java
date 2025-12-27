package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;

/**
 * Created by govind on 01/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleMasterDataItemVO implements Serializable{

  private static final long serialVersionUID = 3132169591420433014L;
  private String productCode;
  private int dangerousLevel;
  private List<MasterDataItemImage> masterDataItemImages;
  private List<MasterDataItemAttributeValue> masterDataItemAttributeValues;

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public List<MasterDataItemImage> getMasterDataItemImages() {
    return masterDataItemImages;
  }

  public void setMasterDataItemImages(List<MasterDataItemImage> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
  }

  public List<MasterDataItemAttributeValue> getMasterDataItemAttributeValues() {
    return masterDataItemAttributeValues;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeValue> masterDataItemAttributeValues) {
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
  }

  public int getDangerousLevel() {
    return dangerousLevel;
  }

  public void setDangerousLevel(int dangerousLevel) {
    this.dangerousLevel = dangerousLevel;
  }

  @JsonIgnore
  public static Map<String, SimpleMasterDataItemVO> toMasterDataItemVo(
      Map<String, MasterDataItem> masterDataItems, Map<String,String> valueAndValueTypeMap) {
    return masterDataItems.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, entry -> {
      SimpleMasterDataItemVO result = new SimpleMasterDataItemVO();
      result.setProductCode(entry.getValue().getProductCode());
      result.setMasterDataItemAttributeValues(entry.getValue().getMasterDataItemAttributeValues());
      entry.getValue().getMasterDataItemAttributeValues().forEach(
          masterDataItemAttributeValue -> masterDataItemAttributeValue.setValueType(
              valueAndValueTypeMap.get(masterDataItemAttributeValue.getAttributeValue())));
      result.setMasterDataItemImages(entry.getValue().getMasterDataItemImages());
      result.setDangerousLevel(entry.getValue().getDangerousLevel());
      return result;
    }));
  }

  @Override
  public String toString() {
    return "SimpleMasterDataItemVO{" + "productCode='" + productCode + '\'' + ", dangerousLevel="
        + dangerousLevel + ", masterDataItemImages=" + masterDataItemImages
        + ", masterDataItemAttributeValues=" + masterDataItemAttributeValues + '}';
  }
}
