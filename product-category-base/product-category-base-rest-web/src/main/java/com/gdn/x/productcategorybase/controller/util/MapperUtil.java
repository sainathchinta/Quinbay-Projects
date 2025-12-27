package com.gdn.x.productcategorybase.controller.util;

import java.util.List;

import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.entity.CategoryShipping;

public interface MapperUtil {

  String mapRequestToString(Object request) throws Exception;

  ShippingResponse mapStringToShippingCodeResponse(String content) throws Exception;

  /**
   * Get shipping response list from @CategoryShipping list
   *
   * @param categoryShippings
   * @return
   * @throws Exception
   */
  List<ShippingResponse> getShippingResponseList(List<CategoryShipping> categoryShippings) throws Exception;

}
