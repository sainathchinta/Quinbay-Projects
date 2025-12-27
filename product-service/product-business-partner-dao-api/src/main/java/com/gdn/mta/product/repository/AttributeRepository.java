package com.gdn.mta.product.repository;

import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.entity.Attribute;

public interface AttributeRepository {

  Attribute findOne(String id) throws Exception;
  
  AttributeResponse findDetailById(String attributeId) throws Exception;

}
