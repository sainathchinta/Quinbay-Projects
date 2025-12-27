package com.gdn.x.product.service.impl;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.api.MasterDataAttributeRepository;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.MasterDataAttributeService;

@Service
public class MasterDataAttributeServiceImpl implements MasterDataAttributeService {

  @Autowired
  private MasterDataAttributeRepository masterDataAttributeRepository;

  @Override
  public MasterDataAttribute findByAttributeCode(String attributeCode) {
    return this.masterDataAttributeRepository.findMasterDataAttributeByAttributeCode(attributeCode);
  }

  private MasterDataAttribute getOrSaveMasterDataAttribute(MasterDataAttribute masterDataAttribute) {
    MasterDataAttribute attribute =
        this.masterDataAttributeRepository
            .findMasterDataAttributeByAttributeCode(masterDataAttribute.getAttributeCode());
    if (attribute == null) {
      attribute = this.masterDataAttributeRepository.save(masterDataAttribute);
    }
    return attribute;
  }


  @Override
  public void setAndSaveMasterDataAttributeItems(List<Item> items) {
    for (Item item : items) {
      if (item.getMasterDataItem().getMasterDataItemAttributeValues() == null) {
        break;
      }
      for (MasterDataItemAttributeValue itemAttr : item.getMasterDataItem()
          .getMasterDataItemAttributeValues()) {
        itemAttr.setMasterDataAttribute(this.getOrSaveMasterDataAttribute(itemAttr
            .getMasterDataAttribute()));
      }
    }
  }

  @Override
  public void setAndSaveMasterDataAttributeProduct(Product product) {
    if (CollectionUtils.isNotEmpty(product.getMasterDataProduct().getMasterDataProductAttributes())) {
      for (MasterDataProductAttribute masterDataProductAttr : product.getMasterDataProduct()
          .getMasterDataProductAttributes()) {
        masterDataProductAttr
            .setMasterDataAttribute(this.getOrSaveMasterDataAttribute(masterDataProductAttr.getMasterDataAttribute()));
      }
    }
  }

  @Override
  public MasterDataAttribute getOrSaveNewMasterDataAttribute(MasterDataAttribute masterDataAttribute) {
    return getOrSaveMasterDataAttribute(masterDataAttribute);
  }
}
