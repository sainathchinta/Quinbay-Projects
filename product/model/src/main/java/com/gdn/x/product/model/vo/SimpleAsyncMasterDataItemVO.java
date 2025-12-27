package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;

/**
 * Created by govind on 05/08/2018 AD.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimpleAsyncMasterDataItemVO implements Serializable{

  private static final long serialVersionUID = 2430135213695267739L;
  private List<MasterDataItemAttributeValue> masterDataItemAttributeValues;
  private List<MasterDataItemImage> masterDataItemImages;

  public List<MasterDataItemAttributeValue> getMasterDataItemAttributeValues() {
    return masterDataItemAttributeValues;
  }

  public void setMasterDataItemAttributeValues(
      List<MasterDataItemAttributeValue> masterDataItemAttributeValues) {
    this.masterDataItemAttributeValues = masterDataItemAttributeValues;
  }

  public List<MasterDataItemImage> getMasterDataItemImages() {
    return masterDataItemImages;
  }

  public void setMasterDataItemImages(List<MasterDataItemImage> masterDataItemImages) {
    this.masterDataItemImages = masterDataItemImages;
  }

  @JsonIgnore
  public static SimpleAsyncMasterDataItemVO toSimpleAsyncMasterDataItemVO(Item item){
    SimpleAsyncMasterDataItemVO simpleAsyncMasterDataItemVO = new SimpleAsyncMasterDataItemVO();
    MasterDataItem masterDataItem = item.getMasterDataItem();
    if(Objects.nonNull(masterDataItem)) {
      simpleAsyncMasterDataItemVO
          .setMasterDataItemAttributeValues(masterDataItem.getMasterDataItemAttributeValues());
        simpleAsyncMasterDataItemVO
            .setMasterDataItemImages(masterDataItem.getMasterDataItemImages());
    }

    return simpleAsyncMasterDataItemVO;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("masterDataItemAttributeValues", masterDataItemAttributeValues)
        .append("masterDataItemImages", masterDataItemImages).toString();
  }
}
