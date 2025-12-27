package com.gdn.x.product.service.api;

import java.util.List;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.Product;

public interface MasterDataAttributeService {

  MasterDataAttribute findByAttributeCode(String attributeCode);

  /**
   * @param items
   */
  void setAndSaveMasterDataAttributeItems(List<Item> items);

  /**
   * @param product
   */
  void setAndSaveMasterDataAttributeProduct(Product product);

  /**
   * get or save new master data attribute
   *
   * @param masterDataAttribute
   */
  MasterDataAttribute getOrSaveNewMasterDataAttribute(MasterDataAttribute masterDataAttribute);

}
