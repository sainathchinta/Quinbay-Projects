package com.gdn.partners.pbp.service.tools;

import java.util.List;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;

public interface ProductCollectionToolsService  {
  
  public void syncState(List<String> productCodes, String specifiedState, String storeId, String username) throws Exception;
  public void moveProductCollectionCategory(String requestId, String username, String storeId, 
      String productCode, String categoryCode) throws Exception;
  public GdnBaseRestResponse addProductAttributesByProductCode(
     String requestId, String username, AddProductAttributesRequest request) throws Exception;
 
}
