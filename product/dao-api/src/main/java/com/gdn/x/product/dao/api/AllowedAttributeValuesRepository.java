package com.gdn.x.product.dao.api;

import java.util.List;
import java.util.Set;

import org.springframework.data.mongodb.repository.MongoRepository;

import com.gdn.x.product.model.entity.AllowedAttributeValues;

public interface AllowedAttributeValuesRepository extends MongoRepository<AllowedAttributeValues,String> {

  AllowedAttributeValues findByStoreIdAndAttributeCodeAndMarkForDeleteFalse(String storeId,
      String attributeCode);

  List<AllowedAttributeValues> findByStoreIdAndAttributeCodeInAndMarkForDeleteFalse(String storeId, Set<String> definingAttributeCodes);
}
