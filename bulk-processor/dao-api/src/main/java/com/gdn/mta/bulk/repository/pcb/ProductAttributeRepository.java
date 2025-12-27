package com.gdn.mta.bulk.repository.pcb;

import java.util.List;

import com.gdn.x.productcategorybase.dto.response.AttributeResponse;

public interface ProductAttributeRepository {

  List<AttributeResponse> getAttributeDetailByAttributeCodes(String requestId, String username,
      List<String> attributeCodes) throws Exception;

  List<AttributeResponse> getAttributeDetailByAttributeCodes(String requestId, String username,
      List<String> attributeCodes, boolean fetchOnlyBasicAttributeDetails) throws Exception;
}
