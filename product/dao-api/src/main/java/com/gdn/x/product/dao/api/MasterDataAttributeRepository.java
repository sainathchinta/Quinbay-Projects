package com.gdn.x.product.dao.api;

import java.util.List;
import java.util.Set;
import org.springframework.data.mongodb.repository.MongoRepository;
import com.gdn.x.product.model.entity.MasterDataAttribute;

public interface MasterDataAttributeRepository extends MongoRepository<MasterDataAttribute, String> {

  /**
   * @param attributeCode must not be blank
   * @return master data attribute
   */
  MasterDataAttribute findMasterDataAttributeByAttributeCode(String attributeCode);

  List<MasterDataAttribute> findMasterDataAttributeByIdIn(Set<String> ids);
}
